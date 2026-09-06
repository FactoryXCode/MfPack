// FactoryX
//
// Copyright: � FactoryX. All rights reserved.
//
// Project: MfPack CaddyAdmin
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: frmCaddyAdmin.pas
// Kind: Pascal / Delphi unit
// Release date: 13-08-2026
// Language: ENU
//
// Revision Version: 4.0.0
// Description: Dialog to edit Caddy settings.
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Carmen (carmenh).
// Contributor(s): Tony Kalf (maXcomX), Carmen (carmenh).
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 24/08/2026 All                 Moby release  SDK 10.0.28000.2705  (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 or later.
//          Works with RDJ and RDJ Pro Caddy configurations on local or remote servers.
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
unit frmCaddyAdmin;

interface

uses

  {WinApi}
  Winapi.Windows,
  Winapi.Messages,
  Winapi.WinSvc,
  Winapi.ShellAPI,
  {System}
  System.SysUtils,
  System.UITypes,
  System.Classes,
  System.IniFiles,
  System.Win.Registry,
  {Vcl}
  Vcl.Controls,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.ComCtrls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.Graphics;

const
  PROV_RSA_AES = 24;
  CRYPT_VERIFYCONTEXT = $F0000000;
  CALG_SHA_256 = $0000800C;
  HP_HASHVAL = $0002;

  REGISTRY_BASE_KEY = '\Software\FactoryX\CaddyAdmin';
  LEGACY_REGISTRY_BASE_KEY = '\Software\RDJPro\CaddyAdmin';
  REGISTRY_VALUE_MISSING = #1;
  WM_CADDYADMIN_REFRESH_STATUS = WM_APP + 100;


type

  HCRYPTPROV = NativeUInt;
  HCRYPTHASH = NativeUInt;
  ALG_ID = Cardinal;

  TfrmCaddyAdmin = class(TForm)
    lblServer: TLabel;
    lblService: TLabel;
    lblCaddyRoot: TLabel;
    lblServerCaddyRoot: TLabel;
    lblSetupIni: TLabel;
    FServerEdit: TEdit;
    FServiceEdit: TEdit;
    FCaddyRootEdit: TEdit;
    FServerCaddyRootEdit: TEdit;
    FSetupIniEdit: TEdit;
    FLogMemo: TMemo;
    btnStatus: TButton;
    btnStart: TButton;
    btnStop: TButton;
    btnRestart: TButton;
    btnInstall: TButton;
    btnUninstall: TButton;
    btnOpenFolder: TButton;
    btnOpenLog: TButton;
    btnSave: TButton;
    btnChangePassword: TButton;
    sbStatus: TStatusBar;
    Bevel1: TBevel;
    Bevel2: TBevel;
    Label1: TLabel;
    Bevel3: TBevel;
    btnCaddyConfig: TButton;

    procedure BtnStatusClick(Sender: TObject);
    procedure BtnStartClick(Sender: TObject);
    procedure BtnStopClick(Sender: TObject);
    procedure BtnRestartClick(Sender: TObject);
    procedure BtnInstallClick(Sender: TObject);
    procedure BtnUninstallClick(Sender: TObject);
    procedure BtnOpenFolderClick(Sender: TObject);
    procedure BtnOpenLogClick(Sender: TObject);
    procedure btnCaddyConfigClick(Sender: TObject);
    procedure BtnSaveClick(Sender: TObject);
    procedure BtnChangePasswordClick(Sender: TObject);

  private

    FAuthenticated: Boolean;

    procedure LoadSettings();
    procedure SaveSettings();
    function EnsureAuthenticated: Boolean;
    function EnsurePasswordConfigured: Boolean;
    function PromptPassword(const ACaption: string;
                            const APrompt: string;
                            out APassword: string): Boolean;

    function PromptNewPassword(out APassword: string): Boolean;
    function VerifyPassword(const APassword: string): Boolean;
    procedure StorePassword(const APassword: string);

    function ReadRegistryString(const ASection: string;
                                const AIdent: string;
                                const ADefault: string): string;
    procedure WriteRegistryString(const ASection: string;
                                  const AIdent: string;
                                  const AValue: string);

    function ServerMachineName(): string;
    function OpenServiceHandle(const ADesiredAccess: DWORD;
                               out AManager: SC_HANDLE;
                               out AService: SC_HANDLE): Boolean;

    function QueryServiceState(out AState: DWORD;
                               out AProcessId: DWORD;
                               out AWin32ExitCode: DWORD;
                               out AServiceExitCode: DWORD): Boolean;

    function ServiceStateText(const AState: DWORD): string;
    function ServiceExitText(const AWin32ExitCode: DWORD;
                             const AServiceExitCode: DWORD): string;

    procedure RefreshStatus();

    procedure WmCaddyAdminRefreshStatus(var AMsg: TMessage); message WM_CADDYADMIN_REFRESH_STATUS;

    procedure StartCaddyService();
    procedure StopCaddyService();
    procedure RestartCaddyService();
    procedure InstallCaddyService();
    procedure UninstallCaddyService();
    function BuildServiceBinaryPath(): string;
    function DeploySourceCaddyRoot(): string;
    function ClientCaddyRoot(): string;
    procedure CopyMissingDeploymentFiles(const ASourceRoot: string;
                                         const ADestRoot: string);
    procedure EnsureCaddyDeployment();

    function WaitForServiceState(const ATargetState: DWORD;
                                 const ATimeoutMs: Cardinal): Boolean;
    procedure AddLog(const S: string);
    procedure BeginOperation(const AMessage: string);
    procedure EndOperation();
    procedure SetBusy(const ABusy: Boolean);


  public

    constructor Create(AOwner: TComponent); override;
    procedure AfterConstruction(); override;
  end;

  // Crypto helpers ============================================================
  function CryptAcquireContext(var phProv: HCRYPTPROV;
                               pszContainer: PChar;
                               pszProvider: PChar;
                               dwProvType: DWORD;
                               dwFlags: DWORD): BOOL; stdcall;

  function CryptReleaseContext(hProv: HCRYPTPROV;
                               dwFlags: DWORD): BOOL; stdcall;

  function CryptCreateHash(hProv: HCRYPTPROV;
                           Algid: ALG_ID;
                           hKey: NativeUInt;
                           dwFlags: DWORD;
                           var phHash: HCRYPTHASH): BOOL; stdcall;

  function CryptHashData(hHash: HCRYPTHASH;
                         pbData: PByte;
                         dwDataLen: DWORD;
                         dwFlags: DWORD): BOOL; stdcall;

  function CryptGetHashParam(hHash: HCRYPTHASH;
                             dwParam: DWORD;
                             pbData: PByte;
                             var pdwDataLen: DWORD;
                             dwFlags: DWORD): BOOL; stdcall;

  function CryptDestroyHash(hHash: HCRYPTHASH): BOOL; stdcall;

  function CryptGenRandom(hProv: HCRYPTPROV;
                          dwLen: DWORD;
                          pbBuffer: PByte): BOOL; stdcall;
  // ===========================================================================
var
  CaddyAdminFrm: TfrmCaddyAdmin;


implementation

{$R *.dfm}

uses
  dlgPassWord,
  frmCaddyConfigEditor;


function BytesToHex(const ABytes: TBytes): string;
const
  HEX: array[0..15] of Char = '0123456789ABCDEF';

var
  I: Integer;

begin

  SetLength(Result,
            Length(ABytes) * 2);

  for I := 0 to High(ABytes) do
    begin

      Result[(I * 2) + 1] := HEX[ABytes[I] shr 4];
      Result[(I * 2) + 2] := HEX[ABytes[I] and $0F];
    end;
end;


function HexToBytes(const AHex: string): TBytes;
var
  I: Integer;
  S: string;

begin

  S := Trim(AHex);
  if Odd(Length(S)) then
    raise Exception.Create('Invalid hex string length');

  SetLength(Result,
            Length(S) div 2);

  for I := 0 to High(Result) do
    Result[I] := StrToInt('$' + Copy(S,
                                     (I * 2) + 1,
                                     2));
end;


function SecureEquals(const A: TBytes;
                      const B: TBytes): Boolean;
var
  I: Integer;
  Diff: Byte;

begin

  Result := Length(A) = Length(B);
  if not Result then
    Exit;

  Diff := 0;
  for I := 0 to High(A) do
    Diff := Diff or (A[I] xor B[I]);
  Result := (Diff = 0);

  // When you forgot the Admin password, you have close CaddyAdmin and open RegEdit with,
  // admin rights and search for:
  // HKEY_CURRENT_USER\Software\FactoryX\CaddyAdmin\Security
  // Then remove the values "Salt" and "Hash", close RegEdit and start CaddyAdmin again.
end;


function Sha256Bytes(const ABytes: TBytes): TBytes;
var
  Provider: HCRYPTPROV;
  Hash: HCRYPTHASH;
  HashLen: DWORD;

begin

  Provider := 0;
  Hash := 0;
  SetLength(Result, 32);
  HashLen := Length(Result);

  if not CryptAcquireContext(Provider,
                             nil,
                             nil,
                             PROV_RSA_AES,
                             CRYPT_VERIFYCONTEXT) then
    RaiseLastOSError();

  try

    if not CryptCreateHash(Provider,
                           CALG_SHA_256,
                           0,
                           0,
                           Hash) then
      RaiseLastOSError();

    try

      if (Length(ABytes) > 0) then
        if not CryptHashData(Hash,
                             @ABytes[0],
                             Length(ABytes),
                             0) then
          RaiseLastOSError();

      if not CryptGetHashParam(Hash,
                               HP_HASHVAL,
                               @Result[0],
                               HashLen,
                               0) then
        RaiseLastOSError();

      SetLength(Result,
                HashLen);
    finally

      if (Hash <> 0) then
        CryptDestroyHash(Hash);
    end;
  finally

    if (Provider <> 0) then
      CryptReleaseContext(Provider,
                          0);
  end;
end;


function RandomBytes(const ACount: Integer): TBytes;
var
  Provider: HCRYPTPROV;

begin

  Provider := 0;
  SetLength(Result,
            ACount);

  if not CryptAcquireContext(Provider,
                             nil,
                             nil,
                             PROV_RSA_AES,
                             CRYPT_VERIFYCONTEXT) then
    RaiseLastOSError();

  try

    if (ACount > 0) and not CryptGenRandom(Provider,
                                           ACount,
                                           @Result[0]) then
      RaiseLastOSError();

  finally

    if (Provider <> 0) then
      CryptReleaseContext(Provider,
                          0);
  end;
end;


function Utf8Bytes(const S: string): TBytes;
var
  U: UTF8String;

begin

  U := UTF8String(S);
  SetLength(Result,
            Length(U));

  if (Length(U) > 0) then
    Move(U[1],
         Result[0],
         Length(U));
end;


function CombineBytes(const A: TBytes;
                      const B: TBytes): TBytes;
begin

  SetLength(Result,
            Length(A) + Length(B));

  if (Length(A) > 0) then
    Move(A[0],
         Result[0],
         Length(A));

  if (Length(B) > 0) then
    Move(B[0],
         Result[Length(A)],
         Length(B));
end;


constructor TfrmCaddyAdmin.Create(AOwner: TComponent);
begin

  inherited Create(AOwner);

  LoadSettings();
end;


procedure TfrmCaddyAdmin.AfterConstruction();
begin

  inherited AfterConstruction;

  FAuthenticated := EnsureAuthenticated;

  if not FAuthenticated then
    begin
      Application.ShowMainForm := False;
      PostMessage(Handle,
                  WM_CLOSE,
                  0,
                  0);
      Exit;
    end;

  PostMessage(Handle,
              WM_CADDYADMIN_REFRESH_STATUS,
              0,
              0);
end;


procedure TfrmCaddyAdmin.WmCaddyAdminRefreshStatus(var AMsg: TMessage);
begin

  BeginOperation('Checking service status');
  try
    RefreshStatus();
  finally
    EndOperation;
  end;
end;


procedure TfrmCaddyAdmin.LoadSettings();
var
  SetupIniFileName: string;

begin

  FServerEdit.Text := ReadRegistryString('Service',
                                         'Server',
                                         '');

  FServiceEdit.Text := ReadRegistryString('Service',
                                          'Name',
                                          '');

  FCaddyRootEdit.Text := ReadRegistryString('Service',
                                            'CaddyRoot',
                                            '');
  FServerCaddyRootEdit.Text := ReadRegistryString('Service',
                                                  'ServerCaddyRoot',
                                                  '');

  SetupIniFileName := ReadRegistryString('Service',
                                         'SetupIni',
                                         REGISTRY_VALUE_MISSING);

  { Migrate the earlier RDJ (Pro) field once.  A deliberately saved empty value
    is retained and is never replaced by a guessed MfRdjJ.ini path. }
  if SetupIniFileName = REGISTRY_VALUE_MISSING then
    SetupIniFileName := ReadRegistryString('Service',
                                           'RdjProSetupIni',
                                           '');

  FSetupIniEdit.Text := SetupIniFileName;
end;


procedure TfrmCaddyAdmin.SaveSettings();
var
  SetupIniFileName: string;
  Ini: TIniFile;

begin

  SetupIniFileName := Trim(FSetupIniEdit.Text);

  if (SetupIniFileName <> '') and
     (not FileExists(SetupIniFileName)) then
    raise Exception.CreateFmt('Application setup INI file not found:'#13#10'%s',
                              [SetupIniFileName]);

  if SetupIniFileName <> '' then
    begin
      Ini := TIniFile.Create(SetupIniFileName);
      try
        if not (Ini.SectionExists('Icecast') or
                Ini.SectionExists('SetupBroadcast') or
                Ini.SectionExists('Caddy')) then
          raise Exception.CreateFmt('The selected file is not a recognized RDJ or RDJ Pro setup INI:'#13#10'%s',
                                    [SetupIniFileName]);
      finally
        Ini.Free;
      end;
    end;

  WriteRegistryString('Service',
                      'Server',
                      Trim(FServerEdit.Text));

  WriteRegistryString('Service',
                      'Name',
                      Trim(FServiceEdit.Text));

  WriteRegistryString('Service',
                      'CaddyRoot',
                      Trim(FCaddyRootEdit.Text));

  WriteRegistryString('Service',
                      'ServerCaddyRoot',
                      Trim(FServerCaddyRootEdit.Text));

  WriteRegistryString('Service',
                      'SetupIni',
                      SetupIniFileName);
  AddLog('Settings saved.');
end;


function TfrmCaddyAdmin.ReadRegistryString(const ASection: string;
                                           const AIdent: string;
                                           const ADefault: string): string;
var
  Reg: TRegistry;
  KeyName: string;

begin

  Result := ADefault;
  KeyName := REGISTRY_BASE_KEY + '\' + ASection;
  Reg := TRegistry.Create(KEY_READ);

  try

    Reg.RootKey := HKEY_CURRENT_USER;
    if Reg.OpenKeyReadOnly(KeyName) and Reg.ValueExists(AIdent) then
      Exit(Reg.ReadString(AIdent));

    // Preserve profiles and password hashes created by RDJ Pro CaddyAdmin.
    KeyName := LEGACY_REGISTRY_BASE_KEY + '\' + ASection;

    if Reg.OpenKeyReadOnly(KeyName) and Reg.ValueExists(AIdent) then
      Result := Reg.ReadString(AIdent);

  finally

    Reg.Free;
  end;
end;


procedure TfrmCaddyAdmin.WriteRegistryString(const ASection: string;
                                             const AIdent: string;
                                             const AValue: string);
var
  Reg: TRegistry;
  KeyName: string;

begin

  KeyName := REGISTRY_BASE_KEY + '\' + ASection;
  Reg := TRegistry.Create(KEY_READ or KEY_WRITE);

  try

    Reg.RootKey := HKEY_CURRENT_USER;

    if Reg.OpenKey(KeyName,
                   True) then
      Reg.WriteString(AIdent,
                      AValue);

  finally

    Reg.Free;
  end;
end;


function TfrmCaddyAdmin.EnsurePasswordConfigured(): Boolean;
var
  Password: string;

begin

  Result := ReadRegistryString('Security',
                               'Hash',
                               '') <> '';
  if Result then
    Exit;

  if (MessageDlg('No admin password has been configured yet. Create one now?',
                 mtConfirmation,
                 [mbYes, mbNo],
                 0) <> mrYes) then
    Exit(False);

  Result := PromptNewPassword(Password);

  if Result then
    begin
      StorePassword(Password);
      AddLog('Admin password configured.');
    end;
end;


function TfrmCaddyAdmin.EnsureAuthenticated: Boolean;
var
  Password: string;

begin

  Result := False;

  if not EnsurePasswordConfigured then
    Exit;

  if not PromptPassword('Caddy Admin',
                        'Enter admin password:',
                        Password) then
    Exit;

  Result := VerifyPassword(Password);
  if not Result then
    MessageDlg('Invalid password.',
               mtError,
               [mbOK],
               0);
end;


function TfrmCaddyAdmin.PromptPassword(const ACaption: string;
                                       const APrompt: string;
                                       out APassword: string): Boolean;
var
  Dlg: TPasswordDlg;

begin

  APassword := '';
  Dlg := TPasswordDlg.Create(nil);

  try

    Dlg.Caption := ACaption;
    Dlg.Label1.Caption := APrompt;
    Dlg.Password.Text := '';
    Dlg.Password.PasswordChar := '*';
    Dlg.ActiveControl := Dlg.Password;
    Dlg.Position := poScreenCenter;
    Dlg.Visible := False;

    Result := Dlg.ShowModal = mrOK;
    if Result then
      APassword := Dlg.Password.Text;

  finally

    Dlg.Free;
  end;
end;


function TfrmCaddyAdmin.PromptNewPassword(out APassword: string): Boolean;
var
  First: string;
  Second: string;

begin

  Result := False;
  APassword := '';
  // Default password = Admin_1
  if not PromptPassword('Create Admin Password',
                        'New password (Use at least 6 characters):',
                        First) then
    Exit;

  if (Length(First) < 6) then
    begin

      MessageDlg('Use at least 6 characters.',
                 mtWarning,
                 [mbOK],
                 0);
      Exit;
    end;

  if not PromptPassword('Create Admin Password',
                        'Confirm password:',
                        Second) then
    Exit;

  if (First <> Second) then
    begin

      MessageDlg('Passwords do not match.',
                 mtError,
                 [mbOK],
                 0);
      Exit;
    end;

  APassword := First;
  Result := True;
end;


function TfrmCaddyAdmin.VerifyPassword(const APassword: string): Boolean;
var
  Salt: TBytes;
  Expected: TBytes;
  Actual: TBytes;

begin

  try

    Salt := HexToBytes(ReadRegistryString('Security',
                                          'Salt',
                                          ''));
    Expected := HexToBytes(ReadRegistryString('Security',
                                              'Hash',
                                              ''));
    Actual := Sha256Bytes(CombineBytes(Salt,
                                       Utf8Bytes(APassword)));
    Result := SecureEquals(Expected,
                           Actual);
  except
    Result := False;
  end;
end;


procedure TfrmCaddyAdmin.StorePassword(const APassword: string);
var
  Salt: TBytes;
  Hash: TBytes;

begin

  Salt := RandomBytes(16);
  Hash := Sha256Bytes(CombineBytes(Salt,
                                   Utf8Bytes(APassword)));
  WriteRegistryString('Security',
                      'Salt',
                      BytesToHex(Salt));
  WriteRegistryString('Security',
                      'Hash',
                      BytesToHex(Hash));
end;


function TfrmCaddyAdmin.ServerMachineName: string;
begin

  Result := Trim(FServerEdit.Text);

  if (Result = '') then
    Exit('');
  if Copy(Result, 1, 2) <> '\\' then
    Result := '\\' + Result;
end;


function TfrmCaddyAdmin.OpenServiceHandle(const ADesiredAccess: DWORD;
                                          out AManager: SC_HANDLE;
                                          out AService: SC_HANDLE): Boolean;
var
  Machine: string;

begin

  Result := False;
  AManager := 0;
  AService := 0;
  Machine := ServerMachineName();

  if (Trim(FServiceEdit.Text) = '') then
  begin
    AddLog('Service name is empty.');
    Exit;
  end;

  if (Machine = '') then
    AManager := OpenSCManager(nil,
                              nil,
                              SC_MANAGER_CONNECT)
  else
    AManager := OpenSCManager(PChar(Machine),
                              nil,
                              SC_MANAGER_CONNECT);

  if (AManager = 0) then
  begin
    AddLog('OpenSCManager failed: ' + SysErrorMessage(GetLastError));
    Exit;
  end;

  AService := OpenService(AManager,
                          PChar(Trim(FServiceEdit.Text)),
                          ADesiredAccess);
  if (AService = 0) then
    begin
      if (GetLastError = ERROR_SERVICE_DOES_NOT_EXIST) then
        AddLog('OpenService failed: service is not installed. Use Install first.')
    else
      AddLog('OpenService failed: ' + SysErrorMessage(GetLastError));

    CloseServiceHandle(AManager);
    AManager := 0;
    Exit;
  end;

  Result := True;
end;


function TfrmCaddyAdmin.QueryServiceState(out AState: DWORD;
                                          out AProcessId: DWORD;
                                          out AWin32ExitCode: DWORD;
                                          out AServiceExitCode: DWORD): Boolean;
var
  Manager: SC_HANDLE;
  Service: SC_HANDLE;
  Status: SERVICE_STATUS_PROCESS;
  BytesNeeded: DWORD;

begin

  AState := 0;
  AProcessId := 0;
  AWin32ExitCode := NO_ERROR;
  AServiceExitCode := 0;
  Result := False;

  if not OpenServiceHandle(SERVICE_QUERY_STATUS,
                           Manager,
                           Service) then
    Exit;

  try

    ZeroMemory(@Status,
               SizeOf(Status));
    BytesNeeded := 0;

    Result := QueryServiceStatusEx(Service,
                                   SC_STATUS_PROCESS_INFO,
                                   @Status,
                                   SizeOf(Status),
                                   BytesNeeded);
    if Result then
      begin

        AState := Status.dwCurrentState;
        AProcessId := Status.dwProcessId;
        AWin32ExitCode := Status.dwWin32ExitCode;
        AServiceExitCode := Status.dwServiceSpecificExitCode;
      end
    else
      AddLog('QueryServiceStatusEx failed: ' + SysErrorMessage(GetLastError));
  finally

    CloseServiceHandle(Service);
    CloseServiceHandle(Manager);
  end;
end;


function TfrmCaddyAdmin.ServiceExitText(const AWin32ExitCode: DWORD;
                                        const AServiceExitCode: DWORD): string;
var
  ErrorText: string;

begin

  Result := '';

  if (AWin32ExitCode = NO_ERROR) then
    Exit;

  ErrorText := Trim(SysErrorMessage(AWin32ExitCode));
  Result := Format('Windows exit code %d',
                   [AWin32ExitCode]);

  if (ErrorText <> '') then
    Result := Result + ': ' + ErrorText;

  if (AWin32ExitCode = ERROR_SERVICE_SPECIFIC_ERROR) and
     (AServiceExitCode <> 0) then
    Result := Result + Format(' (service-specific exit code %d)',
                              [AServiceExitCode]);
end;


function TfrmCaddyAdmin.ServiceStateText(const AState: DWORD): string;
begin

  case AState of
    SERVICE_STOPPED: Result := 'Stopped';
    SERVICE_START_PENDING: Result := 'Start pending';
    SERVICE_STOP_PENDING: Result := 'Stop pending';
    SERVICE_RUNNING: Result := 'Running';
    SERVICE_CONTINUE_PENDING: Result := 'Continue pending';
    SERVICE_PAUSE_PENDING: Result := 'Pause pending';
    SERVICE_PAUSED: Result := 'Paused';
  else
    Result := 'Unknown';
  end;
end;


procedure TfrmCaddyAdmin.RefreshStatus();
var
  State: DWORD;
  ProcessId: DWORD;
  Win32ExitCode: DWORD;
  ServiceExitCode: DWORD;
  StateText: string;
  ExitText: string;

begin

  if QueryServiceState(State,
                       ProcessId,
                       Win32ExitCode,
                       ServiceExitCode) then
    begin

      StateText := ServiceStateText(State);
      ExitText := '';

      if State = SERVICE_STOPPED then
        ExitText := ServiceExitText(Win32ExitCode,
                                    ServiceExitCode);

      if (ExitText <> '') then
        begin
          sbStatus.SimpleText := Format('Status: %s  Exit: %d  PID: %d',
                                        [StateText, Win32ExitCode, ProcessId]);
          AddLog('Service status: ' + StateText + ' - ' + ExitText);
        end
      else
        begin
          sbStatus.SimpleText := Format('Status: %s  PID: %d',
                                        [StateText, ProcessId]);
          AddLog('Service status: ' + StateText);
        end;
    end
  else
    sbStatus.SimpleText := 'Status: unavailable';
end;


function TfrmCaddyAdmin.BuildServiceBinaryPath: string;
var
  Root: string;

begin

  Root := Trim(FServerCaddyRootEdit.Text);

  if (Root = '') then
    Exit('');

  Root := IncludeTrailingPathDelimiter(Root);

  Result := '"' + Root + 'caddy.exe" run --config "' + Root + 'caddy.cff" --adapter caddyfile';
end;



function TfrmCaddyAdmin.DeploySourceCaddyRoot(): string;
begin

  Result := ExpandFileName(IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName)) + '..\Deploy\Server\Caddy');
  if DirectoryExists(Result) then
    Exit;

  Result := ExpandFileName(IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName)) + 'Deploy\Server\Caddy');
  if DirectoryExists(Result) then
    Exit;

  Result := '';
end;


function TfrmCaddyAdmin.ClientCaddyRoot(): string;
var
  ServerName: string;
  ServerRoot: string;

begin

  Result := Trim(FCaddyRootEdit.Text);
  if (Result <> '') then
    Exit;

  ServerRoot := Trim(FServerCaddyRootEdit.Text);
  ServerName := Trim(FServerEdit.Text);

  if (ServerRoot = '') then
    Exit('');

  if (ServerName = '') then
    Exit(ServerRoot);

  if Copy(ServerName,
          1,
          2) = '\\' then
    Delete(ServerName,
           1,
           2);

  if (Length(ServerRoot) >= 2) and (ServerRoot[2] = ':') then
    Result := '\\' + ServerName + '\' + ServerRoot[1] + '$' + Copy(ServerRoot,
                                                                   3,
                                                                   MaxInt)
  else
    Result := '';
end;


procedure TfrmCaddyAdmin.CopyMissingDeploymentFiles(const ASourceRoot: string;
                                                   const ADestRoot: string);
var
  SearchRec: TSearchRec;
  SourcePath: string;
  DestPath: string;

begin

  if not ForceDirectories(ADestRoot) then
    raise Exception.CreateFmt('Could not create Caddy folder:'#13#10'%s', [ADestRoot]);

  if (FindFirst(IncludeTrailingPathDelimiter(ASourceRoot) + '*',
                faAnyFile,
                SearchRec) = 0) then
    try
      repeat
        if (SearchRec.Name = '.') or (SearchRec.Name = '..') then
          Continue;

        SourcePath := IncludeTrailingPathDelimiter(ASourceRoot) + SearchRec.Name;
        DestPath := IncludeTrailingPathDelimiter(ADestRoot) + SearchRec.Name;

        if (SearchRec.Attr and faDirectory) <> 0 then
          CopyMissingDeploymentFiles(SourcePath, DestPath)
        else
          if not FileExists(DestPath) then
            begin
              if not CopyFile(PChar(SourcePath),
                              PChar(DestPath),
                              True) then
                raise Exception.CreateFmt('Could not copy:'#13#10'%s'#13#10'to:'#13#10'%s'#13#10#13#10'%s',
                                          [SourcePath, DestPath, SysErrorMessage(GetLastError)]);
            end;
      until FindNext(SearchRec) <> 0;
    finally
      FindClose(SearchRec);
    end;
end;


procedure TfrmCaddyAdmin.EnsureCaddyDeployment();
var
  SourceRoot: string;
  DestRoot: string;
  DestCaddyExe: string;
  DestConfig: string;

begin

  DestRoot := ClientCaddyRoot();
  if (DestRoot = '') then
    raise Exception.Create('Caddy share/folder is empty.');

  DestCaddyExe := IncludeTrailingPathDelimiter(DestRoot) + 'caddy.exe';
  DestConfig := IncludeTrailingPathDelimiter(DestRoot) + 'caddy.cff';

  if DirectoryExists(DestRoot) and FileExists(DestCaddyExe) and FileExists(DestConfig) then
    begin
      AddLog('Caddy deployment already present: ' + DestRoot);
      Exit;
    end;

  SourceRoot := DeploySourceCaddyRoot();
  if (SourceRoot = '') then
    raise Exception.Create('Caddy deploy source folder was not found. Expected Deploy\Server\Caddy near CaddyAdmin.');

  AddLog('Preparing Caddy deployment folder: ' + DestRoot);
  AddLog('Deploy source: ' + SourceRoot);
  CopyMissingDeploymentFiles(SourceRoot, DestRoot);

  if not FileExists(DestCaddyExe) then
    raise Exception.CreateFmt('Caddy.exe is still missing after deployment:'#13#10'%s', [DestCaddyExe]);

  if not FileExists(DestConfig) then
    raise Exception.CreateFmt('Caddy.cff is still missing after deployment:'#13#10'%s', [DestConfig]);

  AddLog('Caddy deployment ready: ' + DestRoot);
end;


procedure TfrmCaddyAdmin.InstallCaddyService();
var
  Manager: SC_HANDLE;
  Service: SC_HANDLE;
  Machine: string;
  ServiceName: string;
  BinaryPath: string;

begin

  Service := 0;
  Machine := ServerMachineName();
  ServiceName := Trim(FServiceEdit.Text);
  BinaryPath := BuildServiceBinaryPath();

  if (ServiceName = '') then
    begin

      AddLog('Service name is empty.');
      Exit;
    end;

  if (BinaryPath = '') then

  begin

    AddLog('Server local path is empty.');
    Exit;
  end;

  try
    EnsureCaddyDeployment();
  except
    on E: Exception do
      begin
        AddLog('Caddy deployment failed: ' + E.Message);
        Exit;
      end;
  end;

  if (Machine = '') then
    Manager := OpenSCManager(nil,
                             nil,
                             SC_MANAGER_CONNECT or SC_MANAGER_CREATE_SERVICE)
  else
    Manager := OpenSCManager(PChar(Machine),
                             nil,
                             SC_MANAGER_CONNECT or SC_MANAGER_CREATE_SERVICE);

  if (Manager = 0) then
    begin

      AddLog('OpenSCManager for install failed: ' + SysErrorMessage(GetLastError));

      Exit;
    end;

  try

    Service := CreateService(Manager,
                             PChar(ServiceName),
                             PChar(ServiceName + ' (FactoryX Caddy)'),
                             SERVICE_ALL_ACCESS,
                             SERVICE_WIN32_OWN_PROCESS,
                             SERVICE_AUTO_START,
                             SERVICE_ERROR_NORMAL,
                             PChar(BinaryPath),
                             nil,
                             nil,
                             nil,
                             nil,
                             nil);

    if (Service = 0) then

    begin

      if (GetLastError = ERROR_SERVICE_EXISTS) then
        AddLog('Service already exists.')
      else
        AddLog('CreateService failed: ' + SysErrorMessage(GetLastError));

      Exit;
    end;

    AddLog('Service installed: ' + ServiceName);
    AddLog('Command: ' + BinaryPath);

  finally

    if (Service <> 0) then
      CloseServiceHandle(Service);
    CloseServiceHandle(Manager);
  end;

  RefreshStatus();
end;


procedure TfrmCaddyAdmin.UninstallCaddyService();
var
  Manager: SC_HANDLE;
  Service: SC_HANDLE;

begin

  if not OpenServiceHandle($00010000 or SERVICE_QUERY_STATUS or SERVICE_STOP,
                           Manager,
                           Service) then
    Exit;

  try

    if DeleteService(Service) then
      AddLog('Service marked for deletion.')
    else
      AddLog('DeleteService failed: ' + SysErrorMessage(GetLastError));

  finally

    CloseServiceHandle(Service);
    CloseServiceHandle(Manager);
  end;

  RefreshStatus();

end;


procedure TfrmCaddyAdmin.StartCaddyService;
var
  Manager: SC_HANDLE;
  Service: SC_HANDLE;

  Args: PChar;

begin

  if not OpenServiceHandle(SERVICE_START or SERVICE_QUERY_STATUS,
                           Manager,
                           Service) then
    Exit;

  try

    Args := nil;

    if StartService(Service,
                    0,
                    Args) then
      AddLog('Start command sent.')
    else
      if (GetLastError = ERROR_SERVICE_ALREADY_RUNNING) then
        AddLog('Service is already running.')
      else
        AddLog('StartService failed: ' + SysErrorMessage(GetLastError));

  finally

    CloseServiceHandle(Service);
    CloseServiceHandle(Manager);
  end;

  WaitForServiceState(SERVICE_RUNNING,
                      15000);
  RefreshStatus;
end;


procedure TfrmCaddyAdmin.StopCaddyService;
var
  Manager: SC_HANDLE;
  Service: SC_HANDLE;
  Status: SERVICE_STATUS;

begin

  if not OpenServiceHandle(SERVICE_STOP or SERVICE_QUERY_STATUS,
                           Manager,
                           Service) then
    Exit;

  try

    ZeroMemory(@Status,
               SizeOf(Status));

    if ControlService(Service,
                      SERVICE_CONTROL_STOP,
                      Status) then
      AddLog('Stop command sent.')
    else
      if (GetLastError = ERROR_SERVICE_NOT_ACTIVE) then
        AddLog('Service is already stopped.')
      else
        AddLog('ControlService stop failed: ' + SysErrorMessage(GetLastError));

  finally

    CloseServiceHandle(Service);
    CloseServiceHandle(Manager);
  end;

  WaitForServiceState(SERVICE_STOPPED,
                      15000);
  RefreshStatus;
end;


procedure TfrmCaddyAdmin.RestartCaddyService;
begin

  StopCaddyService;
  StartCaddyService;
end;


function TfrmCaddyAdmin.WaitForServiceState(const ATargetState: DWORD;
                                            const ATimeoutMs: Cardinal): Boolean;
var
  StartTick: Cardinal;
  State: DWORD;
  ProcessId: DWORD;
  Win32ExitCode: DWORD;
  ServiceExitCode: DWORD;

begin

  Result := False;
  StartTick := GetTickCount;

  repeat
    if QueryServiceState(State,
                         ProcessId,
                         Win32ExitCode,
                         ServiceExitCode) and (State = ATargetState) then
      Exit(True);

    Sleep(300);
    Application.ProcessMessages;
  until (GetTickCount - StartTick) > ATimeoutMs;
end;


procedure TfrmCaddyAdmin.AddLog(const S: string);
begin

  if Assigned(FLogMemo) then
    FLogMemo.Lines.Add(FormatDateTime('hh:nn:ss',
                       Now) + '  ' + S);
end;


procedure TfrmCaddyAdmin.SetBusy(const ABusy: Boolean);
begin

  btnStatus.Enabled := not ABusy;
  btnStart.Enabled := not ABusy;
  btnStop.Enabled := not ABusy;
  btnRestart.Enabled := not ABusy;
  btnInstall.Enabled := not ABusy;
  btnUninstall.Enabled := not ABusy;
  btnOpenFolder.Enabled := not ABusy;
  btnOpenLog.Enabled := not ABusy;
  btnSave.Enabled := not ABusy;
  btnChangePassword.Enabled := not ABusy;
  FServerEdit.Enabled := not ABusy;
  FServiceEdit.Enabled := not ABusy;
  FCaddyRootEdit.Enabled := not ABusy;
  FServerCaddyRootEdit.Enabled := not ABusy;
  FSetupIniEdit.Enabled := not ABusy;
  btnCaddyConfig.Enabled := not ABusy;

  if ABusy then
    Screen.Cursor := crHourGlass
  else
    Screen.Cursor := crDefault;
end;


procedure TfrmCaddyAdmin.BeginOperation(const AMessage: string);
begin

  SetBusy(True);
  sbStatus.SimpleText := 'Busy: ' + AMessage;
  AddLog(AMessage + '...');
  sbStatus.Update;
  if Assigned(FLogMemo) then
    FLogMemo.Update;
  Application.ProcessMessages;
end;


procedure TfrmCaddyAdmin.EndOperation();
begin

  SetBusy(False);
  sbStatus.Update;
  Application.ProcessMessages;
end;


procedure TfrmCaddyAdmin.BtnStatusClick(Sender: TObject);
begin

  BeginOperation('Checking service status');
  try
    RefreshStatus();
  finally
    EndOperation;
  end;
end;


procedure TfrmCaddyAdmin.BtnStartClick(Sender: TObject);
begin

  BeginOperation('Starting Caddy service');
  try

    StartCaddyService();
  finally

    EndOperation;
  end;
end;


procedure TfrmCaddyAdmin.BtnStopClick(Sender: TObject);
begin

  if (MessageDlg('Stop the Caddy service?',
                mtConfirmation,
                [mbYes, mbNo],
                0) <> mrYes) then
    Exit;

  BeginOperation('Stopping Caddy service');
  try
    StopCaddyService();
  finally
    EndOperation;
  end;
end;


procedure TfrmCaddyAdmin.BtnRestartClick(Sender: TObject);
begin

  if (MessageDlg('Restart the Caddy service?',
                 mtConfirmation,
                 [mbYes, mbNo],
                 0) <> mrYes) then
    Exit;

  BeginOperation('Restarting Caddy service');

  try
    RestartCaddyService();
  finally
    EndOperation;
  end;
end;


procedure TfrmCaddyAdmin.BtnInstallClick(Sender: TObject);
var
  BinaryPath: string;

begin

  BinaryPath := BuildServiceBinaryPath;
  if (BinaryPath = '') then
    begin

      MessageDlg('Enter the server-local Caddy path first.',
                 mtWarning,
                 [mbOK],
                 0);
      Exit;
    end;

  if (Trim(FServiceEdit.Text) = '') then
    begin

      MessageDlg('Enter the service name first.',
                 mtWarning,
                 [mbOK],
                 0);
      Exit;
    end;

  if (MessageDlg('Install the Caddy service on the server?'#13#10#13#10 + BinaryPath,
                 mtConfirmation,
                 [mbYes,
                 mbNo],
                 0) <> mrYes) then
    Exit;

  SaveSettings();
  BeginOperation('Installing Caddy service');

  try

    InstallCaddyService();
  finally

    EndOperation();
  end;
end;


procedure TfrmCaddyAdmin.BtnUninstallClick(Sender: TObject);
begin

  if MessageDlg('Uninstall the Caddy service?',
                mtWarning,
                [mbYes, mbNo], 0) <> mrYes then
    Exit;

  BeginOperation('Uninstalling Caddy service');

  try

    UninstallCaddyService();
  finally

    EndOperation();
  end;
end;


procedure TfrmCaddyAdmin.BtnOpenFolderClick(Sender: TObject);
var
  CaddyRoot: string;

begin

  CaddyRoot := Trim(FCaddyRootEdit.Text);
  if (CaddyRoot = '') then
    begin

      AddLog('Caddy share/folder is empty.');
      Exit;
  end;

  ShellExecute(Handle,
               'open',
               PChar(CaddyRoot),
               nil,
               nil,
               SW_SHOWNORMAL);
end;


procedure TfrmCaddyAdmin.BtnOpenLogClick(Sender: TObject);
var
  LogFile: string;

begin

  if (Trim(FCaddyRootEdit.Text) = '') then
    begin

      AddLog('Caddy share/folder is empty.');
      Exit;
    end;

  LogFile := IncludeTrailingPathDelimiter(Trim(FCaddyRootEdit.Text)) + 'Caddy.log';
  ShellExecute(Handle,
               'open',
               PChar(LogFile),
               nil,
               nil,
               SW_SHOWNORMAL);
end;


procedure TfrmCaddyAdmin.btnCaddyConfigClick(Sender: TObject);
var
  CaddyRoot: string;
  ConfigFileName: string;
  Dlg: TfrmCaddyConfigEditor;

begin

  CaddyRoot := Trim(FCaddyRootEdit.Text);

  if (CaddyRoot = '') then
    begin
      AddLog('Caddy share/folder is empty.');
      Exit;
    end;

  ConfigFileName := IncludeTrailingPathDelimiter(CaddyRoot) + 'caddy.cff';
  Dlg := TfrmCaddyConfigEditor.Create(nil);

  try
    if Dlg.Execute(ConfigFileName,
                   Trim(FSetupIniEdit.Text)) then
      begin
        AddLog('Caddy configuration saved: ' + ConfigFileName);

        if (Trim(FSetupIniEdit.Text) <> '') then
          AddLog('Application setup INI updated: ' + Trim(FSetupIniEdit.Text));
      end;
  except
    on E: Exception do
      MessageDlg(E.Message, mtError, [mbOK], 0);
  end;
  Dlg.Free;
end;

procedure TfrmCaddyAdmin.BtnSaveClick(Sender: TObject);
begin

  try
    SaveSettings();
    RefreshStatus();
  except
    on E: Exception do
      MessageDlg(E.Message,
                 mtError,
                 [mbOK],
                 0);
  end;
end;


procedure TfrmCaddyAdmin.BtnChangePasswordClick(Sender: TObject);
var
  OldPassword: string;
  NewPassword: string;

begin

  if not PromptPassword('Change Admin Password',
                        'Current password:',
                        OldPassword) then
    Exit;

  if not VerifyPassword(OldPassword) then
    begin

      MessageDlg('Invalid current password.',
                 mtError,
                 [mbOK],
                 0);
      Exit;
    end;

  if not PromptNewPassword(NewPassword) then
    Exit;

  StorePassword(NewPassword);
  AddLog('Admin password changed.');
end;


// External methods
//=================
{$WARN SYMBOL_PLATFORM OFF}

  function CryptAcquireContext; external advapi32 name 'CryptAcquireContextW' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function CryptReleaseContext; external advapi32 name 'CryptReleaseContext' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function CryptCreateHash; external advapi32 name 'CryptCreateHash' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function CryptHashData; external advapi32 name 'CryptHashData' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function CryptGetHashParam; external advapi32 name 'CryptGetHashParam' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function CryptDestroyHash; external advapi32 name 'CryptDestroyHash' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function CryptGenRandom; external advapi32 name 'CryptGenRandom' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};

{$WARN SYMBOL_PLATFORM ON}

end.

