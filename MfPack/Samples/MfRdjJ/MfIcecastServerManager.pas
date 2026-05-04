// FactoryX
//
// Copyright: FactoryX. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: RDJ.FilenameParser.pas
// Kind: Pascal Unit
// Release date: 24-01-2026
// Language: ENU
//
// Revision Version: 3.1.9
// Description: Parses filenames containing artist and title. This is more
//              convenient than reading tags, because they are most of the
//              time badly maintenanced or not implemented.
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
unit MfIcecastServerManager;

interface

uses

  {WinApi}
  WinApi.Windows,
  WinApi.WinSock,
  WinApi.WinInet,
  {System}
  System.SysUtils,
  System.Classes,
  {Vcl}
  Vcl.ExtCtrls;

type

  TMfIcecastServerState = (issStopped,
                           issStarting,
                           issRunningNotReady,
                           issReady,
                           issStopping);

  TMfIcecastLogEvent = procedure(Sender: TObject;
                                 const AText: string) of object;

  TMfIcecastStateEvent = procedure(Sender: TObject;
                                   AState: TMfIcecastServerState) of object;

  TMfIcecastServerManager = class{(TComponent)}
  private

    FExePath: string;
    FConfigPath: string;
    FWorkingDir: string;

    FHost: string;
    FPort: Word;
    FHttpPath: string;

    FAutoRestart: Boolean;
    FRestartDelayMs: Cardinal;

    FProcessInfo: TProcessInformation;
    FStartupInfo: TStartupInfo;

    FStdOutRead: THandle;
    FStdOutWrite: THandle;
    FStdErrWrite: THandle;

    FState: TMfIcecastServerState;
    FOnLog: TMfIcecastLogEvent;
    FOnStateChanged: TMfIcecastStateEvent;

    FPollTimer: TTimer;
    FLastStopWasManual: Boolean;
    FPendingRestartTick: Cardinal;
    FLastReady: Boolean;

    procedure SetState(const AValue: TMfIcecastServerState);
    procedure PollTimerTick(Sender: TObject);
    procedure CloseProcessHandles();
    procedure ClosePipeHandles();
    procedure ReadAvailableOutput;
    procedure EmitLog(const AText: string);
    procedure EmitStateChanged();
    function BuildCommandLine(): string;
    function InternalStartProcess: HRESULT;
    procedure InternalStopProcess(AppIsHuttingDown: Boolean = False);
    function GetRunning(): Boolean;
    function FileExistsSafe(const AFileName: string): Boolean;

    function ProbeTcp(): Boolean;
    function ProbeHttp(): Boolean;
    function ProbeReady(): Boolean;
    function GetStatusText(): string;


  public

    constructor Create(){(AOwner: TComponent)}; //override;
    destructor Destroy(); override;

    function Start(): HRESULT;
    procedure Stop(AppIsShuttingDown: Boolean = False);
    procedure Restart();
    procedure CheckProcess();

    property Running: Boolean read GetRunning;
    property State: TMfIcecastServerState read FState;
    property StatusText: string read GetStatusText;

    property ExePath: string read FExePath write FExePath;
    property ConfigPath: string read FConfigPath write FConfigPath;
    property WorkingDir: string read FWorkingDir write FWorkingDir;

    property Host: string read FHost write FHost;
    property Port: Word read FPort write FPort default 8000;
    property HttpPath: string read FHttpPath write FHttpPath;

    property AutoRestart: Boolean read FAutoRestart write FAutoRestart;
    property RestartDelayMs: Cardinal read FRestartDelayMs write FRestartDelayMs default 3000;

    property OnLog: TMfIcecastLogEvent read FOnLog write FOnLog;
    property OnStateChanged: TMfIcecastStateEvent read FOnStateChanged write FOnStateChanged;
  end;


implementation


function _StateToText(AState: TMfIcecastServerState): string;
begin

  case AState of
    issStopped:
      Result := 'Stopped';
    issStarting:
      Result := 'Starting';
    issRunningNotReady:
      Result := 'Running (not ready yet)';
    issReady:
      Result := 'Ready';
    issStopping:
      Result := 'Stopping';
  else
    Result := 'Unknown';
  end;
end;


{ TMfIcecastServerManager }

constructor TMfIcecastServerManager.Create{(AOwner: TComponent)};
begin

  inherited {Create(AOwner)};

  FillChar(FProcessInfo,
           SizeOf(FProcessInfo),
           0);
  FillChar(FStartupInfo,
           SizeOf(FStartupInfo),
           0);

  FStdOutRead := 0;
  FStdOutWrite := 0;
  FStdErrWrite := 0;

  FState := issStopped;
  FHost := '127.0.0.1';
  FPort := 8000;
  FHttpPath := '/';
  FAutoRestart := False;
  FRestartDelayMs := 3000;
  FLastStopWasManual := False;
  FPendingRestartTick := 0;
  FLastReady := False;

  FPollTimer := TTimer.Create(nil{Self});
  FPollTimer.Enabled := False;
  FPollTimer.Interval := 500;
  FPollTimer.OnTimer := PollTimerTick;
end;


destructor TMfIcecastServerManager.Destroy();
begin

  Stop();
  FPollTimer.Enabled := False;
  FreeAndNil(FPollTimer);
  FOnLog := nil;
  FOnStateChanged := nil;

  inherited Destroy;
end;


function TMfIcecastServerManager.FileExistsSafe(const AFileName: string): Boolean;
begin

  Result := (AFileName <> '') and FileExists(AFileName);
end;


procedure TMfIcecastServerManager.SetState(const AValue: TMfIcecastServerState);
begin

  if (FState = AValue) then
    Exit;

  FState := AValue;
  EmitStateChanged;
end;


procedure TMfIcecastServerManager.EmitStateChanged;
begin

  if Assigned(FOnStateChanged) then
    FOnStateChanged(Self,
                    FState);
end;


procedure TMfIcecastServerManager.EmitLog(const AText: string);
begin

  if Assigned(FOnLog) then
    FOnLog(Self,
           AText);
end;


function TMfIcecastServerManager.GetStatusText(): string;
begin

  case FState of
    issStopped:
      Result := 'Stopped';

    issStarting:
      Result := 'Starting';

    issRunningNotReady:
      Result := Format('Running (not ready yet) [%s:%d]',
                       [FHost,
                        FPort]);

    issReady:
      Result := Format('Ready on port %d',
                       [FPort]);

    issStopping:
      Result := 'Stopping';
  else
    Result := 'Unknown';
  end;
end;


function TMfIcecastServerManager.BuildCommandLine(): string;
begin

  Result := '"' + FExePath + '"';

  if (FConfigPath <> '') then
    Result := Result + ' -c "' + FConfigPath + '"';
end;


function TMfIcecastServerManager.InternalStartProcess(): HRESULT;
var
  SA: TSecurityAttributes;
  CmdLine: string;
  CreationFlags: DWORD;
  WorkDir: string;

begin


  if not FileExistsSafe(FExePath) then
    raise Exception.CreateFmt('Icecast executable not found: %s',
                              [FExePath]);

  if (FConfigPath <> '') and (not FileExistsSafe(FConfigPath)) then
    raise Exception.CreateFmt('Icecast config not found: %s',
                              [FConfigPath]);

  if (FWorkingDir <> '') then
    WorkDir := FWorkingDir
  else
    WorkDir := ExtractFilePath(FExePath);

  FillChar(SA,
           SizeOf(SA),
           0);

  SA.nLength := SizeOf(SA);
  SA.bInheritHandle := True;
  SA.lpSecurityDescriptor := nil;

  if not CreatePipe(FStdOutRead,
                    FStdOutWrite,
                    @SA,
                    0) then
    RaiseLastOSError;

  if not SetHandleInformation(FStdOutRead,
                              HANDLE_FLAG_INHERIT,
                              0) then
    RaiseLastOSError;

  FStdErrWrite := FStdOutWrite;

  FillChar(FStartupInfo,
           SizeOf(FStartupInfo),
           0);

  FStartupInfo.cb := SizeOf(FStartupInfo);
  FStartupInfo.dwFlags := STARTF_USESTDHANDLES or STARTF_USESHOWWINDOW;
  FStartupInfo.wShowWindow := SW_HIDE;
  FStartupInfo.hStdInput := GetStdHandle(STD_INPUT_HANDLE);
  FStartupInfo.hStdOutput := FStdOutWrite;
  FStartupInfo.hStdError := FStdErrWrite;

  FillChar(FProcessInfo,
           SizeOf(FProcessInfo),
           0);

  CmdLine := BuildCommandLine;
  CreationFlags := NORMAL_PRIORITY_CLASS or CREATE_NO_WINDOW or CREATE_NEW_PROCESS_GROUP;

  if not CreateProcess(nil,
                       PChar(CmdLine),
                       nil,
                       nil,
                       True,
                       CreationFlags,
                       nil,
                       PChar(WorkDir),
                       FStartupInfo,
                       FProcessInfo) then
    begin

      ClosePipeHandles();
      RaiseLastOSError();
    end;

  if (FStdOutWrite <> 0) then
    begin

      CloseHandle(FStdOutWrite);
      FStdOutWrite := 0;
    end;

  FStdErrWrite := 0;

  EmitLog(Format('[Icecast] Started. PID=%d  Command=%s',
                 [FProcessInfo.dwProcessId,
                  CmdLine]));

  Result := S_OK;
end;


procedure TMfIcecastServerManager.ClosePipeHandles;
begin

  if (FStdOutRead <> 0) then
    begin

      CloseHandle(FStdOutRead);
      FStdOutRead := 0;
    end;

  if (FStdOutWrite <> 0) then
    begin

      CloseHandle(FStdOutWrite);
      FStdOutWrite := 0;
    end;

  if (FStdErrWrite <> 0) then
    begin

      CloseHandle(FStdErrWrite);
      FStdErrWrite := 0;
    end;
end;


procedure TMfIcecastServerManager.CloseProcessHandles;
begin

  if (FProcessInfo.hThread <> 0) then
    begin

      CloseHandle(FProcessInfo.hThread);
      FProcessInfo.hThread := 0;
    end;

  if (FProcessInfo.hProcess <> 0) then
    begin

      CloseHandle(FProcessInfo.hProcess);
      FProcessInfo.hProcess := 0;
    end;

  FProcessInfo.dwProcessId := 0;
  FProcessInfo.dwThreadId := 0;
end;


function TMfIcecastServerManager.Start: HRESULT;
begin

  Result := S_OK;

  if Running then
    Exit;

  FLastStopWasManual := False;
  FPendingRestartTick := 0;
  FLastReady := False;

  SetState(issStarting);

  try

    Result := InternalStartProcess;

    if Succeeded(Result) then
      begin

        SetState(issRunningNotReady);
        FPollTimer.Enabled := True;
      end
    else
      SetState(issStopped);
  except

    on E: Exception do
    begin
      EmitLog('[Icecast] Start failed: ' + E.Message);
      SetState(issStopped);
      Result := E_FAIL;
    end;
  end;
end;


procedure TMfIcecastServerManager.InternalStopProcess(AppIsHuttingDown: Boolean = False);
var
  WaitRes: DWORD;
  ExitCode: DWORD;

begin

  if (FProcessInfo.hProcess = 0) then
    Exit;

  SetState(issStopping);

  GenerateConsoleCtrlEvent(CTRL_BREAK_EVENT,
                           FProcessInfo.dwProcessId);

  WaitRes := WaitForSingleObject(FProcessInfo.hProcess,
                                 1500);

  if (WaitRes = WAIT_TIMEOUT) then
    begin

      TerminateProcess(FProcessInfo.hProcess,
                       0);
      WaitForSingleObject(FProcessInfo.hProcess,
                          2000);
    end;

  ExitCode := 0;
  GetExitCodeProcess(FProcessInfo.hProcess,
                     ExitCode);
  // If the application is shutting down, don't send any messages.
  if not AppIsHuttingDown then
    EmitLog(Format('[Icecast] Stopped. ExitCode=%d',
                   [ExitCode]));

  CloseProcessHandles();
  ClosePipeHandles();
  FLastReady := False;
  SetState(issStopped);
end;


procedure TMfIcecastServerManager.Stop(AppIsShuttingDown: Boolean = False);
begin

  FLastStopWasManual := True;
  FPendingRestartTick := 0;
  FPollTimer.Enabled := False;

  if not Running then
    begin

      FLastReady := False;
      SetState(issStopped);
      Exit;
    end;

  try

    InternalStopProcess(AppIsShuttingDown);
  except

    on E: Exception do
      begin

        EmitLog('[Icecast] Stop failed: ' + E.Message);
        CloseProcessHandles();
        ClosePipeHandles();
        FLastReady := False;
        SetState(issStopped);
      end;
  end;
end;

procedure TMfIcecastServerManager.Restart();
begin

  Stop();
  Start();
end;


function TMfIcecastServerManager.GetRunning(): Boolean;
var
  ExitCode: DWORD;

begin

  Result := False;

  if (FProcessInfo.hProcess = 0) then
    Exit;

  ExitCode := 0;
  if not GetExitCodeProcess(FProcessInfo.hProcess,
                            ExitCode) then
    Exit(False);

  Result := (ExitCode = STILL_ACTIVE);
end;


procedure TMfIcecastServerManager.ReadAvailableOutput;
var
  BytesAvail: DWORD;
  BytesRead: DWORD;
  Buffer: array [0..4095] of AnsiChar;
  S: AnsiString;

begin

  if (FStdOutRead = 0) then
    Exit;

  BytesAvail := 0;
  if not PeekNamedPipe(FStdOutRead,
                       nil,
                       0,
                       nil,
                       @BytesAvail,
                       nil) then
    Exit;

  while (BytesAvail > 0) do
    begin

      BytesRead := 0;
      if not ReadFile(FStdOutRead,
                      Buffer[0],
                      SizeOf(Buffer) - 1,
                      BytesRead,
                     nil) then
        Break;

      if (BytesRead = 0) then
        Break;

      Buffer[BytesRead] := #0;
      SetString(S,
                PAnsiChar(@Buffer[0]),
                BytesRead);

      EmitLog(string(S));

      BytesAvail := 0;
      if not PeekNamedPipe(FStdOutRead,
                           nil,
                           0,
                           nil,
                           @BytesAvail,
                           nil) then
        Break;
    end;
end;


function TMfIcecastServerManager.ProbeTcp(): Boolean;
var
  WsaData: TWSAData;
  Sock: TSocket;
  Addr: TSockAddrIn;
  HostAnsi: AnsiString;
  Tv: TimeVal;
  WriteSet: TFDSet;
  OptVal: u_long;
  SelRes: Integer;
  Err: Integer;
  ErrLen: Integer;

begin

  Result := False;

  if WSAStartup($0202,
                WsaData) <> 0 then
    Exit;
  try

    Sock := socket(AF_INET,
                   SOCK_STREAM,
                   IPPROTO_TCP);
    if Sock = INVALID_SOCKET then
      Exit;

    try

      OptVal := 1;
      ioctlsocket(Sock,
                  FIONBIO,
                  OptVal);

      FillChar(Addr,
               SizeOf(Addr),
               0);
      Addr.sin_family := AF_INET;
      Addr.sin_port := htons(FPort);

      HostAnsi := AnsiString(FHost);
      Addr.sin_addr.S_addr := inet_addr(PAnsiChar(HostAnsi));

      if (LongInt(Addr.sin_addr.S_addr) = LongInt(INADDR_NONE)) then
        Exit;

      connect(Sock,
              Addr,
              SizeOf(Addr));

      FD_ZERO(WriteSet);
      FD_SET(Sock,
             WriteSet);

      Tv.tv_sec := 0;
      Tv.tv_usec := 200 * 1000; // 200 ms

      SelRes := select(0,
                       nil,
                       @WriteSet,
                       nil,
                       @Tv);

      if SelRes > 0 then
      begin
        ErrLen := SizeOf(Err);
        Err := 0;
        getsockopt(Sock,
                   SOL_SOCKET,
                   SO_ERROR,
                   @Err,
                   ErrLen);

        Result := (Err = 0);
      end;
    finally
      closesocket(Sock);
    end;
  finally

    WSACleanup;
  end;
end;


function TMfIcecastServerManager.ProbeHttp: Boolean;
var
  hInet: HINTERNET;
  hUrl: HINTERNET;
  Url: string;
  Buffer: array [0..255] of Byte;
  BytesRead: DWORD;
  TimeoutMs: DWORD;

begin

  Result := False;

  Url := Format('http://%s:%d%s',
                [FHost,
                 FPort,
                 FHttpPath]);

  hInet := InternetOpen('CarmenIcecastProbe',
                        INTERNET_OPEN_TYPE_PRECONFIG,
                        nil,
                        nil,
                        0);
  if hInet = nil then
    Exit;

  try

    TimeoutMs := 300;

    InternetSetOption(hInet,
                      INTERNET_OPTION_CONNECT_TIMEOUT,
                      @TimeoutMs,
                      SizeOf(TimeoutMs));

    InternetSetOption(hInet,
                      INTERNET_OPTION_RECEIVE_TIMEOUT,
                      @TimeoutMs,
                      SizeOf(TimeoutMs));

    InternetSetOption(hInet,
                      INTERNET_OPTION_SEND_TIMEOUT,
                      @TimeoutMs,
                      SizeOf(TimeoutMs));

    hUrl := InternetOpenUrl(hInet,
                            PChar(Url),
                            nil,
                            0,
                            INTERNET_FLAG_RELOAD or
                            INTERNET_FLAG_NO_CACHE_WRITE or
                            INTERNET_FLAG_PRAGMA_NOCACHE,
                            0);
    if hUrl = nil then
      Exit;

    try
      BytesRead := 0;
      Result := InternetReadFile(hUrl,
                                 @Buffer[0],
                                 SizeOf(Buffer),
                                 BytesRead);
    finally

      InternetCloseHandle(hUrl);
    end;
  finally

    InternetCloseHandle(hInet);
  end;
end;


function TMfIcecastServerManager.ProbeReady(): Boolean;
var
  TcpOk: Boolean;
  HttpOk: Boolean;

begin

  TcpOk := ProbeTcp;

  if not TcpOk then
    Exit(False);

  HttpOk := ProbeHttp;

  if HttpOk then
    Exit(True);

  Result := TcpOk;
end;


procedure TMfIcecastServerManager.CheckProcess();
var
  ExitCode: DWORD;
  ReadyNow: Boolean;

begin

  ReadAvailableOutput();

  if FPendingRestartTick <> 0 then
  begin

    if (GetTickCount >= FPendingRestartTick) then
      begin

        FPendingRestartTick := 0;
        EmitLog('[Icecast] Auto-restarting...');
        Start();
      end;
    Exit;
  end;

  if (FProcessInfo.hProcess = 0) then
    Exit;

  ExitCode := 0;
  if not GetExitCodeProcess(FProcessInfo.hProcess,
                            ExitCode) then
    Exit;

  if (ExitCode <> STILL_ACTIVE) then
    begin

      EmitLog(Format('[Icecast] Process exited unexpectedly. ExitCode=%d',
                     [ExitCode]));

      CloseProcessHandles();
      ClosePipeHandles();
      FLastReady := False;
      SetState(issStopped);

      if FAutoRestart and (not FLastStopWasManual) then
        begin

          FPendingRestartTick := GetTickCount + FRestartDelayMs;
          EmitLog(Format('[Icecast] Restart scheduled in %d ms',
                         [FRestartDelayMs]));
        end;
      Exit;
    end;

  ReadyNow := ProbeReady;

  if ReadyNow then
    begin

      if not FLastReady then
        EmitLog(Format('[Icecast] Server ready on %s:%d',
                       [FHost,
                        FPort]));
      FLastReady := True;
      SetState(issReady);
    end
  else
    begin

      if FLastReady then
        EmitLog('[Icecast] Server no longer responding to readiness probe.');
      FLastReady := False;
      SetState(issRunningNotReady);
    end;
end;


procedure TMfIcecastServerManager.PollTimerTick(Sender: TObject);
begin

  CheckProcess();
end;

end.
