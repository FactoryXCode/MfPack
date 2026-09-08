// FactoryX
//
// Copyright © FactoryX, Netherlands/Australia/Germany. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: frmFxServeAdmin.pas
// Kind: Pascal Unit
// Release date: 13-08-2026
// Language: ENU
//
// Revision Version: 4.0.0
// Description: FxServe Admin is the admin tool for managing the `FxServe` Windows
//              service locally or on a remote Windows server.
//
// Company: FactoryX
// Intiator(s): Tony (maXcomX), Carmen (carmenh).
// Contributor(s): Tony Kalf (maXcomX), Carmen (carmenh).
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 24/08/2026 All                 Moby release  SDK 10.0.28000.2705  (Windows 11)ws 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 or higher.
//
// Related objects: -
// Related projects: MfPackX400
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.26100.4654
//
// Todo: -
//
// =============================================================================
// Source: Microsoft Learn
//==============================================================================
//
// LICENSE
//
// The contents of this file are subject to the Mozilla Public License
// Version 2.0 (the "License"); you may not use this file except in
// compliance with the License. You may obtain a copy of the License at
// https://mozilla.org/MPL/2.0/
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
unit frmFxServeAdmin;

interface

uses
  {WinApi}
  WinApi.Windows,
  WinApi.Messages,
  WinApi.WinSvc,
  WinApi.ShellAPI,
  WinApi.WinInet,
  WinApi.WinSock,
  {System}
  System.SysUtils,
  System.Classes,
  System.IniFiles,
  System.UITypes,
  System.Win.Registry,
  {Vcl}
  Vcl.Forms,
  Vcl.Controls,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.ComCtrls,
  Vcl.Dialogs,
  Vcl.Graphics;

type

  TFxServeServiceStatus = record
    Success: Boolean;
    State: DWORD;
    ProcessId: DWORD;
    Win32ExitCode: DWORD;
    ServiceExitCode: DWORD;
    ErrorText: string;
    Manager: SC_HANDLE;
    ManagerMachine: string;
    ConnectionMilliseconds: Cardinal;
    QueryMilliseconds: Cardinal;
  end;
  PFxServeServiceStatus = ^TFxServeServiceStatus;

const
  WM_FXSERVE_STATUS_COMPLETE = WM_APP + $301;

type

  TFxServeStatusThread = class(TThread)
  private
    FWindowHandle: HWND;
    FMachineName: string;
    FServiceName: string;
    FManager: SC_HANDLE;
  protected
    procedure Execute(); override;
  public
    constructor Create(const AWindowHandle: HWND;
                       const AMachineName: string;
                       const AServiceName: string;
                       const AManager: SC_HANDLE);
    destructor Destroy(); override;
  end;


  TfrmFxServeAdmin = class(TForm)
    FLogMemo: TMemo;
    Panel1: TPanel;
    lblServer: TLabel;
    lblService: TLabel;
    lblShare: TLabel;
    lblServerRoot: TLabel;
    lblPublicHost: TLabel;
    FServerEdit: TEdit;
    FServiceEdit: TEdit;
    FShareEdit: TEdit;
    FServerRootEdit: TEdit;
    FPublicHostEdit: TEdit;
    btnStatus: TButton;
    btnStart: TButton;
    btnStop: TButton;
    btnRestart: TButton;
    btnInstall: TButton;
    btnUninstall: TButton;
    btnOpenFolder: TButton;
    btnOpenLog: TButton;
    btnEditIni: TButton;
    btnHealth: TButton;
    btnOpenLan: TButton;
    btnOpenHttps: TButton;
    btnSaveSettings: TButton;
    btnChangePassword: TButton;
    Bevel1: TBevel;
    Bevel2: TBevel;
    Bevel3: TBevel;
    Label1: TLabel;
    Label2: TLabel;
    FStatusBar: TStaticText;

    procedure StatusClick(Sender: TObject);
    procedure StartClick(Sender: TObject);
    procedure StopClick(Sender: TObject);
    procedure RestartClick(Sender: TObject);
    procedure InstallClick(Sender: TObject);
    procedure UninstallClick(Sender: TObject);
    procedure OpenFolderClick(Sender: TObject);
    procedure OpenLogClick(Sender: TObject);
    procedure EditConfigClick(Sender: TObject);
    procedure HealthClick(Sender: TObject);
    procedure OpenLanClick(Sender: TObject);
    procedure OpenPublicClick(Sender: TObject);
    procedure SaveClick(Sender: TObject);
    procedure ChangePasswordClick(Sender: TObject);

  private

    FAuthenticated: Boolean;
    FBusy: Boolean;
    FBusyCaption: string;
    FStatusBeforeBusy: string;
    FStatusQueryActive: Boolean;
    FStatusManager: SC_HANDLE;
    FStatusManagerMachine: string;
    FStatusThread: TFxServeStatusThread;

    procedure AddLog(const AText: string);
    procedure BeginBusy(const AAction: string);
    procedure EndBusy();
    procedure SaveSettings();
    procedure ApplyDeploymentSettings();
    procedure LoadSettings();
    function ServerMachineName(): string;
    function EnsureAuthenticated(): Boolean;
    function EnsurePasswordConfigured(): Boolean;
    function PromptPassword(const ACaption: string;
                            const APrompt: string;
                            out APassword: string): Boolean;
    function PromptNewPassword(out APassword: string): Boolean;
    function VerifyPassword(const APassword: string): Boolean;
    procedure StorePassword(const APassword: string);
    function ReadSecurityValue(const AName: string): string;
    procedure WriteSecurityValue(const AName: string;
                                 const AValue: string);

    function OpenServiceHandle(const AAccess: DWORD;
                               out AManager: SC_HANDLE;
                               out AService: SC_HANDLE): Boolean;

    function OpenNamedServiceHandle(const AServiceName: string;
                                    const AAccess: DWORD;
                                    out AManager: SC_HANDLE;
                                    out AService: SC_HANDLE;
                                    const ALogFailure: Boolean = True): Boolean;

    function QueryServiceState(out AState: DWORD;
                               out AProcessId: DWORD;
                               out AWin32ExitCode: DWORD;
                               out AServiceExitCode: DWORD): Boolean;

    function QueryNamedServiceState(const AServiceName: string;
                                    out AState: DWORD;
                                    out AProcessId: DWORD;
                                    out AWin32ExitCode: DWORD;
                                    out AServiceExitCode: DWORD;
                                    const ALogFailure: Boolean = True): Boolean;

    function StateText(const AState: DWORD): string;

    function ExitText(const AWin32ExitCode: DWORD;
                      const AServiceExitCode: DWORD): string;

    function WaitForNamedServiceState(const AServiceName: string;
                                      const AState: DWORD;
                                      const ATimeoutMs: Cardinal): Boolean;

    function StartNamedService(const AServiceName: string): Boolean;
    function StopNamedService(const AServiceName: string): Boolean;

    function BuildServiceCommand(): string;
    function ConfigFileName(): string;
    function LogFileName(): string;

    function HttpGetStatus(const AUrl: string;
                           out AStatus: DWORD;
                           out AError: string): Boolean;

    procedure RefreshServiceStatus();
    procedure BeginServiceStatusQuery();
    procedure StatusQueryComplete(var AMessage: TMessage); message WM_FXSERVE_STATUS_COMPLETE;
    procedure StartFxServe();
    procedure StopFxServe();
    procedure RestartFxServe();
    procedure InstallFxServe();
    procedure UninstallFxServe();
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;
    procedure AfterConstruction(); override;
  end;

var
  FxServeAdminForm: TfrmFxServeAdmin;

implementation

{$R *.dfm}

uses
  dlgFxServePassword;

const
  REGISTRY_KEY = '\Software\FactoryX\FxServeAdmin';
  REGISTRY_SECURITY_KEY = REGISTRY_KEY + '\Security';
  DEFAULT_SERVER = 'MY_SERVER1';
  DEFAULT_SERVICE = 'FxServe';
  DEFAULT_SHARE = '\\MY_SERVER1\FxServe';
  DEFAULT_SERVER_ROOT = 'C:\FxServe';
  DEFAULT_PUBLIC_HOST = 'myradio.myhost.com';
  SITE_CONFIG_FILE = 'fxserve-config.json';
  PROV_RSA_AES = 24;
  CRYPT_VERIFYCONTEXT = $F0000000;
  CALG_SHA_256 = $0000800C;
  HP_HASHVAL = $0002;


function JsonString(const AValue: string): string;
var
  I: Integer;
  C: Char;

begin

  Result := '"';

  for I := 1 to Length(AValue) do
    begin
      C := AValue[I];

      case C of
        '"': Result := Result + '\"';
        '\': Result := Result + '\\';
        #8: Result := Result + '\b';
        #9: Result := Result + '\t';
        #10: Result := Result + '\n';
        #12: Result := Result + '\f';
        #13: Result := Result + '\r';
      else
        if Ord(C) < 32 then
          Result := Result + Format('\u%.4x',
                                    [Ord(C)])
        else
          Result := Result + C;
      end;
    end;

  Result := Result + '"';
end;


procedure ReplaceFileAtomically(const ATemporaryFileName: string;
                                const ATargetFileName: string);
begin

  if not MoveFileEx(PChar(ATemporaryFileName),
                    PChar(ATargetFileName),
                    MOVEFILE_REPLACE_EXISTING or MOVEFILE_WRITE_THROUGH) then
    raise Exception.CreateFmt('Could not replace %s: %s',
                              [ATargetFileName,
                               SysErrorMessage(GetLastError)]);
end;


procedure BackupFile(const AFileName: string);
begin

  if FileExists(AFileName) and
     not CopyFile(PChar(AFileName),
                  PChar(AFileName + '.bak'),
                  False) then
    raise Exception.CreateFmt('Could not create backup %s: %s',
                              [AFileName + '.bak',
                               SysErrorMessage(GetLastError)]);
end;

constructor TFxServeStatusThread.Create(const AWindowHandle: HWND;
                                        const AMachineName: string;
                                        const AServiceName: string;
                                        const AManager: SC_HANDLE);
begin

  inherited Create(True);

  FreeOnTerminate := False;
  FWindowHandle := AWindowHandle;
  FMachineName := AMachineName;
  FServiceName := AServiceName;
  FManager := AManager;
end;


destructor TFxServeStatusThread.Destroy();
begin

  if (FManager <> 0) then
    CloseServiceHandle(FManager);

  inherited Destroy();
end;


procedure TFxServeStatusThread.Execute();
var
  StatusResult: PFxServeServiceStatus;
  Manager: SC_HANDLE;
  Service: SC_HANDLE;
  Status: SERVICE_STATUS_PROCESS;
  BytesNeeded: DWORD;
  ErrorCode: DWORD;
  StartTick: Cardinal;

begin

  New(StatusResult);
  StatusResult^.Success := False;
  StatusResult^.State := 0;
  StatusResult^.ProcessId := 0;
  StatusResult^.Win32ExitCode := 0;
  StatusResult^.ServiceExitCode := 0;
  StatusResult^.ErrorText := '';
  StatusResult^.Manager := 0;
  StatusResult^.ManagerMachine := FMachineName;
  StatusResult^.ConnectionMilliseconds := 0;
  StatusResult^.QueryMilliseconds := 0;
  Manager := FManager;
  FManager := 0;
  Service := 0;

  try
    if (Manager = 0) then
      begin
        StartTick := GetTickCount();

        if (FMachineName = '') then
          Manager := OpenSCManager(nil,
                                   nil,
                                   SC_MANAGER_CONNECT)
        else
          Manager := OpenSCManager(PChar(FMachineName),
                                   nil,
                                   SC_MANAGER_CONNECT);

        StatusResult^.ConnectionMilliseconds := GetTickCount() - StartTick;
      end;

    if (Manager = 0) then
      begin
        ErrorCode := GetLastError;
        StatusResult^.ErrorText := 'OpenSCManager failed: ' +
                                   SysErrorMessage(ErrorCode);
      end
    else
      begin
        StartTick := GetTickCount();
        Service := OpenService(Manager,
                               PChar(FServiceName),
                               SERVICE_QUERY_STATUS);

        if (Service = 0) then
          begin
            ErrorCode := GetLastError;

            if (ErrorCode = ERROR_SERVICE_DOES_NOT_EXIST) then
              StatusResult^.ErrorText := 'FxServe service is not installed.'
            else
              StatusResult^.ErrorText := 'OpenService failed: ' +
                                         SysErrorMessage(ErrorCode);
          end
        else
          begin
            ZeroMemory(@Status,
                       SizeOf(Status));
            BytesNeeded := 0;

            if QueryServiceStatusEx(Service,
                                    SC_STATUS_PROCESS_INFO,
                                    @Status,
                                    SizeOf(Status),
                                    BytesNeeded) then
              begin
                StatusResult^.Success := True;
                StatusResult^.State := Status.dwCurrentState;
                StatusResult^.ProcessId := Status.dwProcessId;
                StatusResult^.Win32ExitCode := Status.dwWin32ExitCode;
                StatusResult^.ServiceExitCode := Status.dwServiceSpecificExitCode;
              end
            else
              begin
                ErrorCode := GetLastError;
                StatusResult^.ErrorText := 'QueryServiceStatusEx failed: ' +
                                           SysErrorMessage(ErrorCode);
              end;
          end;

        StatusResult^.QueryMilliseconds := GetTickCount() - StartTick;
      end;

  except
    on E: Exception do
      StatusResult^.ErrorText := E.Message;
  end;

  if (Service <> 0) then
    CloseServiceHandle(Service);

  StatusResult^.Manager := Manager;

  if Terminated then
    begin
      if (StatusResult^.Manager <> 0) then
        CloseServiceHandle(StatusResult^.Manager);

      Dispose(StatusResult);
      Exit;
    end;

  if not PostMessage(FWindowHandle,
                     WM_FXSERVE_STATUS_COMPLETE,
                     0,
                     LPARAM(StatusResult)) then
    begin
      if (StatusResult^.Manager <> 0) then
        CloseServiceHandle(StatusResult^.Manager);

      Dispose(StatusResult);
    end;
end;


type

  HCRYPTPROV = NativeUInt;
  HCRYPTHASH = NativeUInt;
  ALG_ID = Cardinal;

function CryptAcquireContext(var phProv: HCRYPTPROV;
                             pszContainer: PChar;
                             pszProvider: PChar;
                             dwProvType: DWORD;
                             dwFlags: DWORD): BOOL; stdcall; forward;

function CryptReleaseContext(hProv: HCRYPTPROV;
                             dwFlags: DWORD): BOOL; stdcall; forward;

function CryptCreateHash(hProv: HCRYPTPROV;
                         Algid: ALG_ID;
                         hKey: NativeUInt;
                         dwFlags: DWORD;
                         var phHash: HCRYPTHASH): BOOL; stdcall; forward;

function CryptHashData(hHash: HCRYPTHASH;
                       pbData: PByte;
                       dwDataLen: DWORD;
                       dwFlags: DWORD): BOOL; stdcall; forward;

function CryptGetHashParam(hHash: HCRYPTHASH;
                           dwParam: DWORD;
                           pbData: PByte;
                           var pdwDataLen: DWORD;
                           dwFlags: DWORD): BOOL; stdcall; forward;

function CryptDestroyHash(hHash: HCRYPTHASH): BOOL; stdcall; forward;

function CryptGenRandom(hProv: HCRYPTPROV;
                        dwLen: DWORD;
                        pbBuffer: PByte): BOOL; stdcall; forward;

type

  TConfigEditorForm = class(TForm)
  private
    FMemo: TMemo;
    FFileName: string;
    procedure SaveClick(Sender: TObject);

  public

    constructor CreateEditor(AOwner: TComponent;
                             const AFileName: string);

  end;

constructor TConfigEditorForm.CreateEditor(AOwner: TComponent;
                                           const AFileName: string);
var
  Button: TButton;

begin

  inherited CreateNew(AOwner);

  FFileName := AFileName;
  Caption := 'FxServe configuration - ' + AFileName;
  Position := poOwnerFormCenter;
  Width := 760;
  Height := 620;
  BorderStyle := bsSizeable;

  FMemo := TMemo.Create(Self);
  FMemo.Parent := Self;
  FMemo.Align := alClient;
  FMemo.ScrollBars := ssBoth;
  FMemo.WordWrap := False;
  FMemo.Font.Name := 'Consolas';
  FMemo.Font.Size := 10;
  FMemo.Lines.LoadFromFile(FFileName);

  Button := TButton.Create(Self);
  Button.Parent := Self;
  Button.Align := alBottom;
  Button.Height := 38;
  Button.Caption := 'Save configuration';
  Button.OnClick := SaveClick;
end;


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
  Value: string;

begin

  Value := Trim(AHex);

  if Odd(Length(Value)) then
    raise Exception.Create('Invalid hex string length.');

  SetLength(Result,
            Length(Value) div 2);

  for I := 0 to High(Result) do
    Result[I] := StrToInt('$' + Copy(Value,
                                     (I * 2) + 1,
                                     2));
end;


function SecureEquals(const A: TBytes;
                      const B: TBytes): Boolean;
var
  I: Integer;
  Difference: Byte;

begin

  Result := Length(A) = Length(B);

  if not Result then
    Exit;

  Difference := 0;

  for I := 0 to High(A) do
    Difference := Difference or (A[I] xor B[I]);

  Result := Difference = 0;
end;


function Sha256Bytes(const ABytes: TBytes): TBytes;
var
  Provider: HCRYPTPROV;
  Hash: HCRYPTHASH;
  HashLength: DWORD;

begin

  Provider := 0;
  Hash := 0;
  SetLength(Result,
            32);
  HashLength := Length(Result);

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
      if (Length(ABytes) > 0) and
         not CryptHashData(Hash,
                           @ABytes[0],
                           Length(ABytes),
                           0) then
        RaiseLastOSError();

      if not CryptGetHashParam(Hash,
                               HP_HASHVAL,
                               @Result[0],
                               HashLength,
                               0) then
        RaiseLastOSError();

      SetLength(Result,
                HashLength);

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
    if (ACount > 0) and
       not CryptGenRandom(Provider,
                          ACount,
                          @Result[0]) then
      RaiseLastOSError();

  finally
    if (Provider <> 0) then
      CryptReleaseContext(Provider,
                          0);
  end;
end;


function Utf8Bytes(const AValue: string): TBytes;
var
  Utf8Value: UTF8String;

begin

  Utf8Value := UTF8String(AValue);
  SetLength(Result,
            Length(Utf8Value));

  if (Length(Utf8Value) > 0) then
    Move(Utf8Value[1],
         Result[0],
         Length(Utf8Value));
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


procedure TConfigEditorForm.SaveClick(Sender: TObject);
var
  BackupName: string;

begin

  BackupName := FFileName + '.bak';
  if FileExists(FFileName) and not CopyFile(PChar(FFileName),
                                            PChar(BackupName),
                                            False) then
    raise Exception.Create('Could not create configuration backup: ' + SysErrorMessage(GetLastError));

  FMemo.Lines.SaveToFile(FFileName);
  ModalResult := mrOk;
end;


constructor TfrmFxServeAdmin.Create(AOwner: TComponent);
begin

  inherited Create(AOwner);

  LoadSettings();
end;


destructor TfrmFxServeAdmin.Destroy();
var
  Message: TMsg;
  StatusResult: PFxServeServiceStatus;

begin

  if Assigned(FStatusThread) then
    begin
      FStatusThread.Terminate();
      FStatusThread.WaitFor();
    end;

  if HandleAllocated then
    while PeekMessage(Message,
                      Handle,
                      WM_FXSERVE_STATUS_COMPLETE,
                      WM_FXSERVE_STATUS_COMPLETE,
                      PM_REMOVE) do
      begin
        StatusResult := PFxServeServiceStatus(Message.lParam);

        if Assigned(StatusResult) then
          begin
            if (StatusResult^.Manager <> 0) then
              CloseServiceHandle(StatusResult^.Manager);

            Dispose(StatusResult);
          end;
      end;

  FreeAndNil(FStatusThread);

  if (FStatusManager <> 0) then
    begin
      CloseServiceHandle(FStatusManager);
      FStatusManager := 0;
    end;

  inherited Destroy();
end;


procedure TfrmFxServeAdmin.AfterConstruction();
begin

  inherited AfterConstruction();

  FAuthenticated := EnsureAuthenticated();

  if not FAuthenticated then
    begin
      Application.ShowMainForm := False;
      PostMessage(Handle,
                  WM_CLOSE,
                  0,
                  0);
      Exit;
    end;

  FStatusBar.BringToFront();
  FStatusBar.Repaint();
  AddLog('Ready. Windows account permissions control local and remote service access.');
  BeginServiceStatusQuery();
end;


procedure TfrmFxServeAdmin.AddLog(const AText: string);
begin

  FLogMemo.Lines.Add(FormatDateTime('hh:nn:ss',
                                    Now) + '  ' + AText);
  FLogMemo.SelStart := Length(FLogMemo.Text);
end;


procedure TfrmFxServeAdmin.BeginBusy(const AAction: string);
begin

  if FBusy then
    Exit;

  FBusy := True;
  FStatusBeforeBusy := FStatusBar.Caption;
  FBusyCaption := 'Processing: ' + AAction + ' Please wait...';
  Panel1.Enabled := False;
  Screen.Cursor := crHourGlass;
  FStatusBar.Caption := FBusyCaption;
  FStatusBar.Repaint();
  AddLog(FBusyCaption);
  FLogMemo.Repaint();
  Application.ProcessMessages();
end;


procedure TfrmFxServeAdmin.EndBusy();
begin

  if not FBusy then
    Exit;

  if FStatusBar.Caption = FBusyCaption then
    FStatusBar.Caption := FStatusBeforeBusy;

  FBusyCaption := '';
  FStatusBeforeBusy := '';
  FBusy := False;
  Panel1.Enabled := True;
  Screen.Cursor := crDefault;
  FStatusBar.Repaint();
  Application.ProcessMessages();
end;


function TfrmFxServeAdmin.ReadSecurityValue(const AName: string): string;
var
  Registry: TRegistry;

begin

  Result := '';
  Registry := TRegistry.Create(KEY_READ);

  try
    Registry.RootKey := HKEY_CURRENT_USER;

    if Registry.OpenKeyReadOnly(REGISTRY_SECURITY_KEY) and
       Registry.ValueExists(AName) then
      Result := Registry.ReadString(AName);

  finally
    Registry.Free;
  end;
end;


procedure TfrmFxServeAdmin.WriteSecurityValue(const AName: string;
                                              const AValue: string);
var
  Registry: TRegistry;

begin

  Registry := TRegistry.Create(KEY_READ or KEY_WRITE);

  try
    Registry.RootKey := HKEY_CURRENT_USER;

    if not Registry.OpenKey(REGISTRY_SECURITY_KEY,
                            True) then
      raise Exception.Create('Could not open the FxServeAdmin security registry key.');

    Registry.WriteString(AName,
                         AValue);

  finally
    Registry.Free;
  end;
end;


function TfrmFxServeAdmin.EnsurePasswordConfigured(): Boolean;
var
  Password: string;

begin

  Result := ReadSecurityValue('Hash') <> '';

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


function TfrmFxServeAdmin.EnsureAuthenticated(): Boolean;
var
  Password: string;

begin

  Result := False;

  if not EnsurePasswordConfigured() then
    Exit;

  if not PromptPassword('FxServe Admin',
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


function TfrmFxServeAdmin.PromptPassword(const ACaption: string;
                                         const APrompt: string;
                                         out APassword: string): Boolean;
var
  Dialog: TFxServePasswordDlg;

begin

  APassword := '';
  Dialog := TFxServePasswordDlg.Create(nil);

  try
    Dialog.Caption := ACaption;
    Dialog.lblPrompt.Caption := APrompt;
    Dialog.edtPassword.Text := '';
    Dialog.edtPassword.PasswordChar := '*';
    Dialog.ActiveControl := Dialog.edtPassword;
    Dialog.Position := poScreenCenter;

    Result := Dialog.ShowModal = mrOk;

    if Result then
      APassword := Dialog.edtPassword.Text;

  finally
    Dialog.Free;
  end;
end;


function TfrmFxServeAdmin.PromptNewPassword(out APassword: string): Boolean;
var
  First: string;
  Second: string;

begin

  Result := False;
  APassword := '';

  if not PromptPassword('Create Admin Password',
                        'New password (use at least 6 characters):',
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


function TfrmFxServeAdmin.VerifyPassword(const APassword: string): Boolean;
var
  Salt: TBytes;
  Expected: TBytes;
  Actual: TBytes;

begin

  try
    Salt := HexToBytes(ReadSecurityValue('Salt'));
    Expected := HexToBytes(ReadSecurityValue('Hash'));
    Actual := Sha256Bytes(CombineBytes(Salt,
                                       Utf8Bytes(APassword)));
    Result := SecureEquals(Expected,
                           Actual);
  except
    Result := False;
  end;
end;


procedure TfrmFxServeAdmin.StorePassword(const APassword: string);
var
  Salt: TBytes;
  Hash: TBytes;

begin

  Salt := RandomBytes(16);
  Hash := Sha256Bytes(CombineBytes(Salt,
                                   Utf8Bytes(APassword)));

  WriteSecurityValue('Salt',
                     BytesToHex(Salt));
  WriteSecurityValue('Hash',
                     BytesToHex(Hash));
end;


procedure TfrmFxServeAdmin.LoadSettings;
var
  Registry: TRegistry;

begin

  FServerEdit.Text := DEFAULT_SERVER;
  FServiceEdit.Text := DEFAULT_SERVICE;
  FShareEdit.Text := DEFAULT_SHARE;
  FServerRootEdit.Text := DEFAULT_SERVER_ROOT;
  FPublicHostEdit.Text := DEFAULT_PUBLIC_HOST;
  Registry := TRegistry.Create(KEY_READ);

  try
    Registry.RootKey := HKEY_CURRENT_USER;

    if Registry.OpenKeyReadOnly(REGISTRY_KEY) then
      begin
        if Registry.ValueExists('Server') then
          FServerEdit.Text := Registry.ReadString('Server');

        if Registry.ValueExists('Share') then
          FShareEdit.Text := Registry.ReadString('Share');

        if Registry.ValueExists('ServerRoot') then
          FServerRootEdit.Text := Registry.ReadString('ServerRoot');

        if Registry.ValueExists('PublicHost') then
          FPublicHostEdit.Text := Registry.ReadString('PublicHost');
      end;

  finally
    Registry.Free;
  end;
end;


procedure TfrmFxServeAdmin.SaveSettings();
var
  Registry: TRegistry;

begin

  Registry := TRegistry.Create(KEY_WRITE);

  try
    Registry.RootKey := HKEY_CURRENT_USER;

    if not Registry.OpenKey(REGISTRY_KEY,
                            True) then
      raise Exception.Create('Could not open the FxServeAdmin registry key.');

    Registry.WriteString('Server',
                         Trim(FServerEdit.Text));

    Registry.WriteString('Share',
                         Trim(FShareEdit.Text));

    Registry.WriteString('ServerRoot',
                         Trim(FServerRootEdit.Text));

    Registry.WriteString('PublicHost',
                         Trim(FPublicHostEdit.Text));

  finally
    Registry.Free;
  end;
end;


procedure TfrmFxServeAdmin.ApplyDeploymentSettings();
var
  ShareValue: string;
  Share: string;
  ServerName: string;
  PublicHost: string;
  ConfigName: string;
  ConfigTemporaryName: string;
  WebRoot: string;
  SiteConfigName: string;
  SiteTemporaryName: string;
  Ini: TMemIniFile;
  Json: TStringList;

begin

  ShareValue := Trim(FShareEdit.Text);

  if (ShareValue = '') then
    raise Exception.Create('Enter the FxServe share path.');

  Share := IncludeTrailingPathDelimiter(ShareValue);
  ServerName := Trim(FServerEdit.Text);
  PublicHost := LowerCase(Trim(FPublicHostEdit.Text));

  while (Copy(ServerName,
              1,
              2) = '\\') do
    Delete(ServerName,
           1,
           2);

  if not DirectoryExists(Share) then
    raise Exception.Create('The configured FxServe share is not available.');

  if (ServerName = '') or
     (Pos('\', ServerName) > 0) or
     (Pos('/', ServerName) > 0) then
    raise Exception.Create('Enter a valid Windows server name.');

  if (PublicHost = '') or
     (Pos(' ', PublicHost) > 0) or
     (Pos('/', PublicHost) > 0) or
     (Pos('\', PublicHost) > 0) or
     (Pos(':', PublicHost) > 0) then
    raise Exception.Create('Enter only the public hostname, without https://, a port, or a path.');

  ConfigName := Share + 'FxServe.ini';

  if not FileExists(ConfigName) then
    raise Exception.Create('FxServe configuration not found: ' + ConfigName);

  ConfigTemporaryName := ConfigName + '.admin.tmp';
  SiteTemporaryName := '';

  if FileExists(ConfigTemporaryName) then
    DeleteFile(ConfigTemporaryName);

  if not CopyFile(PChar(ConfigName),
                  PChar(ConfigTemporaryName),
                  False) then
    raise Exception.CreateFmt('Could not prepare %s for editing: %s',
                              [ConfigName,
                               SysErrorMessage(GetLastError)]);

  try
    Ini := TMemIniFile.Create(ConfigTemporaryName);

    try
      Ini.WriteString('Wan',
                      'HostName',
                      PublicHost);
      WebRoot := Trim(Ini.ReadString('Server',
                                    'WebRoot',
                                    '.\www'));
      Ini.UpdateFile();
    finally
      Ini.Free;
    end;

    BackupFile(ConfigName);
    ReplaceFileAtomically(ConfigTemporaryName,
                          ConfigName);

    if ExtractFileDrive(WebRoot) = '' then
      WebRoot := ExpandFileName(IncludeTrailingPathDelimiter(
                                  ExtractFileDir(ConfigName)) + WebRoot);

    if not ForceDirectories(WebRoot) then
      raise Exception.Create('Could not create the configured web root: ' + WebRoot);

    SiteConfigName := IncludeTrailingPathDelimiter(WebRoot) + SITE_CONFIG_FILE;
    SiteTemporaryName := SiteConfigName + '.admin.tmp';

    if FileExists(SiteTemporaryName) then
      DeleteFile(SiteTemporaryName);

    Json := TStringList.Create();

    try
      Json.Add('{');
      Json.Add('  "version": 1,');
      Json.Add('  "serverName": ' + JsonString(ServerName) + ',');
      Json.Add('  "publicHostName": ' + JsonString(PublicHost) + ',');
      Json.Add('  "lanUrl": ' + JsonString('http://' + ServerName + ':8080/') + ',');
      Json.Add('  "publicUrl": ' + JsonString('https://' + PublicHost + '/'));
      Json.Add('}');
      Json.SaveToFile(SiteTemporaryName,
                      TEncoding.UTF8);
    finally
      Json.Free;
    end;

    BackupFile(SiteConfigName);
    ReplaceFileAtomically(SiteTemporaryName,
                          SiteConfigName);
  finally
    if FileExists(ConfigTemporaryName) then
      DeleteFile(ConfigTemporaryName);

    if (SiteTemporaryName <> '') and FileExists(SiteTemporaryName) then
      DeleteFile(SiteTemporaryName);
  end;

  FPublicHostEdit.Text := PublicHost;
  AddLog('Deployment updated: FxServe.ini and www\' + SITE_CONFIG_FILE + '.');
  AddLog('Live stream JSON and media files were left unchanged. Restart FxServe to apply the WAN hostname.');
end;


function TfrmFxServeAdmin.ServerMachineName: string;
var
  ComputerName: array[0..MAX_COMPUTERNAME_LENGTH] of Char;
  ComputerNameLength: DWORD;

begin

  Result := Trim(FServerEdit.Text);

  while (Copy(Result,
              1,
              2) = '\\') do
    Delete(Result,
           1,
           2);

  if (Result = '') or
     SameText(Result,
              '.') or
     SameText(Result,
              'localhost') or
     SameText(Result,
              '127.0.0.1') or
     SameText(Result,
              '::1') then
    begin
      Result := '';
      Exit;
    end;

  ComputerNameLength := Length(ComputerName);

  if GetComputerName(@ComputerName[0],
                     ComputerNameLength) and
     SameText(Result,
              string(ComputerName)) then
    begin
      Result := '';
      Exit;
    end;

  if (Result = '') then
    Exit;

  Result := '\\' + Result;
end;


function TfrmFxServeAdmin.OpenServiceHandle(const AAccess: DWORD;
                                            out AManager: SC_HANDLE;
                                            out AService: SC_HANDLE): Boolean;
begin

  Result := OpenNamedServiceHandle(DEFAULT_SERVICE,
                                   AAccess,
                                   AManager,
                                   AService,
                                   True);
end;


function TfrmFxServeAdmin.OpenNamedServiceHandle(const AServiceName: string;
                                                 const AAccess: DWORD;
                                                 out AManager: SC_HANDLE;
                                                 out AService: SC_HANDLE;
                                                 const ALogFailure: Boolean): Boolean;
var
  Machine: string;

begin
  Result := False;
  AManager := 0;
  AService := 0;
  Machine := ServerMachineName;

  if (Trim(AServiceName) = '') then
    begin
      if ALogFailure then
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
      if ALogFailure then
        AddLog('OpenSCManager failed: ' + SysErrorMessage(GetLastError));
      Exit;
    end;

  AService := OpenService(AManager,
                          PChar(Trim(AServiceName)),
                          AAccess);

  if (AService = 0) then
    begin
      if GetLastError = ERROR_SERVICE_DOES_NOT_EXIST then
        begin
          if ALogFailure then
            AddLog('Service is not installed: ' + AServiceName)
        end
      else
        begin
          if ALogFailure then
            AddLog('OpenService failed for ' + AServiceName + ': ' + SysErrorMessage(GetLastError));
        end;

    CloseServiceHandle(AManager);
    AManager := 0;
    Exit;
    end;

  Result := True;
end;


function TfrmFxServeAdmin.QueryServiceState(out AState, AProcessId,
                                            AWin32ExitCode,
                                            AServiceExitCode: DWORD): Boolean;
begin

  Result := QueryNamedServiceState(DEFAULT_SERVICE,
                                   AState,
                                   AProcessId,
                                   AWin32ExitCode,
                                   AServiceExitCode,
                                   True);
end;


function TfrmFxServeAdmin.QueryNamedServiceState(const AServiceName: string;
                                                 out AState: DWORD;
                                                 out AProcessId: DWORD;
                                                 out AWin32ExitCode: DWORD;
                                                 out AServiceExitCode: DWORD;
                                                 const ALogFailure: Boolean = True): Boolean;
var
  Manager, Service: SC_HANDLE;
  Status: SERVICE_STATUS_PROCESS;
  BytesNeeded: DWORD;

begin

  AState := 0;
  AProcessId := 0;
  AWin32ExitCode := 0;
  AServiceExitCode := 0;
  Result := False;

  if not OpenNamedServiceHandle(AServiceName,
                                SERVICE_QUERY_STATUS,
                                Manager,
                                Service,
                                ALogFailure) then
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
    if (Result = True) then
      begin
        AState := Status.dwCurrentState;
        AProcessId := Status.dwProcessId;
        AWin32ExitCode := Status.dwWin32ExitCode;
        AServiceExitCode := Status.dwServiceSpecificExitCode;
      end
    else
      if ALogFailure then
        AddLog('QueryServiceStatusEx failed for ' + AServiceName + ': ' + SysErrorMessage(GetLastError));

  finally
    CloseServiceHandle(Service);
    CloseServiceHandle(Manager);
  end;
end;


function TfrmFxServeAdmin.StateText(const AState: DWORD): string;
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


function TfrmFxServeAdmin.ExitText(const AWin32ExitCode,
                                   AServiceExitCode: DWORD): string;
begin

  Result := '';

  if (AWin32ExitCode = NO_ERROR) then
    Exit;

  Result := Format('Windows exit code %d: %s',
                   [AWin32ExitCode, Trim(SysErrorMessage(AWin32ExitCode))]);

  if (AWin32ExitCode = ERROR_SERVICE_SPECIFIC_ERROR) and (AServiceExitCode <> 0) then
    Result := Result + Format(' (service-specific exit code %d)',
                              [AServiceExitCode]);
end;


procedure TfrmFxServeAdmin.RefreshServiceStatus();
var
  State: DWORD;
  ProcessId: DWORD;
  Win32ExitCode: DWORD;
  ServiceExitCode: DWORD;
  Text: string;
  Failure: string;

begin

  if QueryServiceState(State,
                       ProcessId,
                       Win32ExitCode,
                       ServiceExitCode) then
    begin
      Text := StateText(State);
      Failure := ExitText(Win32ExitCode,
                          ServiceExitCode);

      FStatusBar.Caption := Format('FxServe: %s   PID: %d',
                                   [Text, ProcessId]);
      if (Failure <> '') then
        AddLog('FxServe status: ' + Text + ' - ' + Failure)
      else
        AddLog('FxServe status: ' + Text);
    end
  else
    FStatusBar.Caption := 'FxServe: unavailable';

  FStatusBar.Repaint();
end;


procedure TfrmFxServeAdmin.BeginServiceStatusQuery();
var
  Thread: TFxServeStatusThread;
  Machine: string;
  Manager: SC_HANDLE;

begin

  if FStatusQueryActive then
    Exit;

  FStatusQueryActive := True;
  BeginBusy('checking FxServe status.');
  btnStatus.Enabled := False;
  Machine := ServerMachineName();

  if (FStatusManager <> 0) and
     not SameText(FStatusManagerMachine,
                  Machine) then
    begin
      CloseServiceHandle(FStatusManager);
      FStatusManager := 0;
      FStatusManagerMachine := '';
    end;

  Manager := FStatusManager;
  FStatusManager := 0;
  FStatusManagerMachine := '';
  Thread := TFxServeStatusThread.Create(Handle,
                                        Machine,
                                        DEFAULT_SERVICE,
                                        Manager);
  FStatusThread := Thread;

  try
    Thread.Start();
  except
    FreeAndNil(FStatusThread);
    FStatusQueryActive := False;
    btnStatus.Enabled := True;
    FStatusBar.Caption := 'FxServe: status check failed';
    FStatusBar.Repaint();
    EndBusy();
    raise;
  end;
end;


procedure TfrmFxServeAdmin.StatusQueryComplete(var AMessage: TMessage);
var
  StatusResult: PFxServeServiceStatus;
  Text: string;
  Failure: string;

begin

  StatusResult := PFxServeServiceStatus(AMessage.LParam);

  if not Assigned(StatusResult) then
    begin
      FStatusQueryActive := False;
      EndBusy();
      Exit;
    end;

  try
    FStatusQueryActive := False;
    btnStatus.Enabled := True;

    if Assigned(FStatusThread) then
      begin
        FStatusThread.WaitFor();
        FreeAndNil(FStatusThread);
      end;

    if (FStatusManager <> 0) then
      CloseServiceHandle(FStatusManager);

    FStatusManager := StatusResult^.Manager;
    FStatusManagerMachine := StatusResult^.ManagerMachine;
    StatusResult^.Manager := 0;

    if StatusResult^.Success then
      begin
        Text := StateText(StatusResult^.State);
        Failure := ExitText(StatusResult^.Win32ExitCode,
                            StatusResult^.ServiceExitCode);
        FStatusBar.Caption := Format('FxServe: %s   PID: %d',
                                     [Text, StatusResult^.ProcessId]);

        if (Failure <> '') then
          AddLog('FxServe status: ' + Text + ' - ' + Failure)
        else
          AddLog('FxServe status: ' + Text);
      end
    else
      begin
        FStatusBar.Caption := 'FxServe: unavailable';

        if (StatusResult^.ErrorText <> '') then
          AddLog(StatusResult^.ErrorText);
      end;

    AddLog(Format('Status timing: connection %d ms, query %d ms.',
                  [StatusResult^.ConnectionMilliseconds,
                   StatusResult^.QueryMilliseconds]));

    FStatusBar.Repaint();

  finally
    Dispose(StatusResult);
    EndBusy();
  end;
end;


function TfrmFxServeAdmin.WaitForNamedServiceState(const AServiceName: string;
                                                   const AState: DWORD;
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
    if QueryNamedServiceState(AServiceName,
                              State,
                              ProcessId,
                              Win32ExitCode,
                              ServiceExitCode,
                              False) and (State = AState) then
      Exit(True);

    Sleep(250);

    Application.ProcessMessages;

  until (GetTickCount - StartTick > ATimeoutMs);
end;


function TfrmFxServeAdmin.StartNamedService(const AServiceName: string): Boolean;
var
  Manager: SC_HANDLE;
  Service: SC_HANDLE;
  Args: PChar;
  ErrorCode: DWORD;

begin

  Result := False;

  if not OpenNamedServiceHandle(AServiceName,
                                SERVICE_START or SERVICE_QUERY_STATUS,
                                Manager,
                                Service,
                                True) then
    Exit;

  try
    Args := nil;
    if StartService(Service,
                    0,
                    Args) then
      AddLog('Start command sent: ' + AServiceName)
    else
      begin
        ErrorCode := GetLastError;

        if (ErrorCode = ERROR_SERVICE_ALREADY_RUNNING) then
          AddLog('Service is already running: ' + AServiceName)
        else
          begin
            AddLog('StartService failed for ' + AServiceName + ': ' + SysErrorMessage(ErrorCode));

            Exit;
          end;
      end;

  finally
    CloseServiceHandle(Service);
    CloseServiceHandle(Manager);
  end;

  Result := WaitForNamedServiceState(AServiceName,
                                     SERVICE_RUNNING,
                                     20000);

  if not Result then
    AddLog('Timed out waiting for ' + AServiceName + ' to reach Running.');
end;


function TfrmFxServeAdmin.StopNamedService(const AServiceName: string): Boolean;
var
  Manager, Service: SC_HANDLE;
  Status: SERVICE_STATUS;
  ErrorCode: DWORD;

begin

  Result := False;

  if not OpenNamedServiceHandle(AServiceName,
                                SERVICE_STOP or SERVICE_QUERY_STATUS,
                                Manager,
                                Service,
                                True) then
    Exit;

  try
    ZeroMemory(@Status,
               SizeOf(Status));

    if ControlService(Service,
                      SERVICE_CONTROL_STOP,
                      Status) then
      AddLog('Stop command sent: ' + AServiceName)
    else
      begin
        ErrorCode := GetLastError;

        if (ErrorCode = ERROR_SERVICE_NOT_ACTIVE) then
          AddLog('Service is already stopped: ' + AServiceName)
        else
          begin
            AddLog('ControlService failed for ' + AServiceName + ': ' + SysErrorMessage(ErrorCode));
            Exit;
          end;
      end;

  finally
    CloseServiceHandle(Service);
    CloseServiceHandle(Manager);
  end;

  Result := WaitForNamedServiceState(AServiceName,
                                     SERVICE_STOPPED,
                                     20000);

  if not Result then
    AddLog('Timed out waiting for ' + AServiceName + ' to reach Stopped.');
end;


procedure TfrmFxServeAdmin.StartFxServe();
begin

  StartNamedService(DEFAULT_SERVICE);
  RefreshServiceStatus;
end;


procedure TfrmFxServeAdmin.StopFxServe;
begin

  StopNamedService(DEFAULT_SERVICE);
  RefreshServiceStatus();
end;


procedure TfrmFxServeAdmin.RestartFxServe();
begin

  StopFxServe();
  StartFxServe();
end;


function TfrmFxServeAdmin.BuildServiceCommand(): string;
var
  Root: string;

begin

  Root := Trim(FServerRootEdit.Text);

  if (Root = '') then
    Exit('');

  Root := IncludeTrailingPathDelimiter(Root);
  Result := '"' + Root + 'FxServe.exe" --service --config "' + Root + 'FxServe.ini"';
end;


procedure TfrmFxServeAdmin.InstallFxServe;
var
  Manager: SC_HANDLE;
  Service: SC_HANDLE;
  Machine: string;
  ServiceName: string;
  Command: string;
  Share: string;

begin

  Machine := ServerMachineName;
  ServiceName := DEFAULT_SERVICE;
  Command := BuildServiceCommand;
  Share := IncludeTrailingPathDelimiter(Trim(FShareEdit.Text));

  if (ServiceName = '') or
     (Command = '') or
     (Share = '') then
    begin
      AddLog('Service, share and server-local path are required.');
      Exit;
    end;

  if not FileExists(Share + 'FxServe.exe') or not FileExists(Share + 'FxServe.ini') then
    begin
      AddLog('FxServe.exe and FxServe.ini must exist in the configured share.');
      Exit;
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

  Service := 0;

  try
    Service := CreateService(Manager,
                             PChar(ServiceName),
                             'FactoryX FxServe',
                             SERVICE_ALL_ACCESS,
                             SERVICE_WIN32_OWN_PROCESS,
                             SERVICE_AUTO_START,
                             SERVICE_ERROR_NORMAL,
                             PChar(Command),
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


    AddLog('FxServe service installed.');
    AddLog('Command: ' + Command);

  finally
    if (Service <> 0) then
      CloseServiceHandle(Service);

    CloseServiceHandle(Manager);
  end;

  RefreshServiceStatus();
end;


procedure TfrmFxServeAdmin.UninstallFxServe();
var
  Manager: SC_HANDLE;
  Service: SC_HANDLE;

begin
  if not OpenServiceHandle($00010000 or SERVICE_STOP or SERVICE_QUERY_STATUS,
                           Manager,
                           Service) then
    Exit;

  try
    if DeleteService(Service) then
      AddLog('FxServe service marked for deletion.')
    else
      AddLog('DeleteService failed: ' + SysErrorMessage(GetLastError));

  finally
    CloseServiceHandle(Service);
    CloseServiceHandle(Manager);
  end;
end;


function TfrmFxServeAdmin.ConfigFileName(): string;
begin

  Result := IncludeTrailingPathDelimiter(Trim(FShareEdit.Text)) + 'FxServe.ini';
end;


function TfrmFxServeAdmin.LogFileName(): string;
begin

  Result := IncludeTrailingPathDelimiter(Trim(FShareEdit.Text)) + 'FxServe.log';
end;


function TfrmFxServeAdmin.HttpGetStatus(const AUrl: string;
                                        out AStatus: DWORD;
                                        out AError: string): Boolean;
var
  Session: HINTERNET;
  Request: HINTERNET;
  StatusSize: DWORD;
  Index: DWORD;
  Timeout: DWORD;
  ErrorCode: DWORD;

begin
  Result := False;
  AStatus := 0;
  AError := '';

  Session := InternetOpen('FxServeAdmin/1.0',
                          INTERNET_OPEN_TYPE_PRECONFIG,
                          nil,
                          nil,
                          0);

  if (Session = nil) then
    begin
      ErrorCode := GetLastError;
      AError := SysErrorMessage(ErrorCode);
      if (AError = '') then
        AError := Format('Windows error %d',
                         [ErrorCode]);

      Exit;
    end;


  try
    // Set Internet time out options.
    Timeout := 5000;

    InternetSetOption(Session,
                      INTERNET_OPTION_CONNECT_TIMEOUT,
                      @Timeout,
                      SizeOf(Timeout));

    InternetSetOption(Session,
                      INTERNET_OPTION_RECEIVE_TIMEOUT,
                      @Timeout,
                      SizeOf(Timeout));

    InternetSetOption(Session,
                      INTERNET_OPTION_SEND_TIMEOUT,
                      @Timeout,
                      SizeOf(Timeout));

    Request := InternetOpenUrl(Session,
                               PChar(AUrl),
                               nil,
                               0,
                               INTERNET_FLAG_RELOAD or INTERNET_FLAG_NO_CACHE_WRITE,
                               0);

    if (Request = nil) then
      begin
        ErrorCode := GetLastError;
        AError := SysErrorMessage(ErrorCode);

        if (AError = '') then
          AError := Format('Windows error %d',
                           [ErrorCode]);

        Exit;
      end;

    try
      StatusSize := SizeOf(AStatus);
      Index := 0;

      if not HttpQueryInfo(Request,
                           HTTP_QUERY_STATUS_CODE or HTTP_QUERY_FLAG_NUMBER,
                           @AStatus,
                           StatusSize,
                           Index) then
      begin
        ErrorCode := GetLastError;
        AError := SysErrorMessage(ErrorCode);

        if (AError = '') then
          AError := Format('Windows error %d',
                           [ErrorCode]);
        Exit;
      end;

      Result := True;

    finally
      InternetCloseHandle(Request);
    end;

  finally
    InternetCloseHandle(Session);
  end;
end;


function TryResolveIPv4Address(const AHostName: string;
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
        (HostEntry^.h_addrtype <> AF_INET) or not
        Assigned(HostEntry^.h_addr_list) or not
        Assigned(HostEntry^.h_addr_list^) then
      Exit;

    Address := PInAddr(HostEntry^.h_addr_list^);
    AIPv4Address := string(AnsiString(inet_ntoa(Address^)));

    Result := (AIPv4Address <> '');

  finally
    WSACleanup;
  end;
end;


function HttpStatusText(const AStatus: DWORD): string;
begin

  if (AStatus >= 200) and (AStatus < 300) then
    Result := Format('OK (HTTP %d)', [AStatus])
  else
    if (AStatus >= 300) and (AStatus < 400) then
      Result := Format('Redirect (HTTP %d)',
                       [AStatus])
    else
      Result := Format('Problem (HTTP %d)',
                      [AStatus]);
end;


procedure TfrmFxServeAdmin.StatusClick(Sender: TObject);
begin

  BeginServiceStatusQuery();
end;


procedure TfrmFxServeAdmin.StartClick(Sender: TObject);
begin

  BeginBusy('starting FxServe.');

  try
    StartFxServe();
  finally
    EndBusy();
  end;
end;


procedure TfrmFxServeAdmin.StopClick(Sender: TObject);
begin

  if MessageDlg('Stop the FxServe service?',
                mtConfirmation,
                 [mbYes, mbNo],
                 0) = mrYes then
    begin
      BeginBusy('stopping FxServe.');

      try
        StopFxServe();
      finally
        EndBusy();
      end;
    end;
end;


procedure TfrmFxServeAdmin.RestartClick(Sender: TObject);
begin

  if MessageDlg('Restart the FxServe service?',
                mtConfirmation,
                 [mbYes, mbNo],
                 0) = mrYes then
    begin
      BeginBusy('restarting FxServe.');

      try
        RestartFxServe();
      finally
        EndBusy();
      end;
    end;
end;


procedure TfrmFxServeAdmin.InstallClick(Sender: TObject);
begin

  if MessageDlg('Install the FxServe service?'#13#10#13#10 + BuildServiceCommand,
                 mtConfirmation,
                  [mbYes, mbNo],
                  0) = mrYes then
    begin
      BeginBusy('installing the FxServe service.');

      try
        InstallFxServe();
      finally
        EndBusy();
      end;
    end;
end;


procedure TfrmFxServeAdmin.UninstallClick(Sender: TObject);
begin

  if MessageDlg('Uninstall the FxServe service registration?'#13#10 +
                'Files will not be deleted.',
                mtWarning,
                 [mbYes, mbNo],
                 0) = mrYes then
    begin
      BeginBusy('uninstalling the FxServe service.');

      try
        UninstallFxServe();
      finally
        EndBusy();
      end;
    end;
end;


procedure TfrmFxServeAdmin.OpenFolderClick(Sender: TObject);
begin

 ShellExecute(Handle,
              'open',
              PChar(Trim(FShareEdit.Text)),
              nil,
              nil,
              SW_SHOWNORMAL);
end;


procedure TfrmFxServeAdmin.OpenLogClick(Sender: TObject);
begin

  if not FileExists(LogFileName) then
    AddLog('Log file not found: ' + LogFileName)
  else
    ShellExecute(Handle,
                 'open',
                 PChar(LogFileName),
                 nil,
                 nil,
                 SW_SHOWNORMAL);
end;


procedure TfrmFxServeAdmin.EditConfigClick(Sender: TObject);
var
  Editor: TConfigEditorForm;

begin

  if not FileExists(ConfigFileName) then
    begin
      AddLog('Configuration not found: ' + ConfigFileName);
      Exit;
    end;

  Editor := TConfigEditorForm.CreateEditor(Self,
                                           ConfigFileName);

  try
    if (Editor.ShowModal = mrOk) then
      AddLog('Configuration saved; backup: ' + ConfigFileName + '.bak');

  finally
    Editor.Free;
  end;
end;


procedure TfrmFxServeAdmin.HealthClick(Sender: TObject);
var
  Server: string;
  LanAddress: string;
  Url: string;
  DisplayUrl,
  ErrorText: string;
  Status: DWORD;

begin

  BeginBusy('running the LAN and public health checks.');

  try
    Server := Trim(FServerEdit.Text);

  if (Server = '') then
    Server := '127.0.0.1';

  if not TryResolveIPv4Address(Server,
                               LanAddress) then
    LanAddress := Server;

  Url := 'http://' + LanAddress + ':8080/stream/live.json';
  DisplayUrl := 'http://' + Server + ':8080/stream/live.json';

  if HttpGetStatus(Url, Status, ErrorText) then
    AddLog(Format('Live manifest: %s - %s',
                  [HttpStatusText(Status), DisplayUrl]))
  else
    AddLog('Live manifest check failed: ' + ErrorText);

    if (Trim(FPublicHostEdit.Text) <> '') then
    begin
      Url := 'https://' + Trim(FPublicHostEdit.Text) + '/';

      if HttpGetStatus(Url, Status, ErrorText) then
        AddLog(Format('Public HTTPS: %s - %s',
                      [HttpStatusText(Status), Url]))
      else
        AddLog('Public HTTPS check failed: ' + ErrorText);
    end;
  finally
    EndBusy();
  end;
end;


procedure TfrmFxServeAdmin.OpenLanClick(Sender: TObject);
var
  Server: string;
  Url: string;

begin

  Server := Trim(FServerEdit.Text);

  if (Server = '') then
    Server := '127.0.0.1';  // Local

  Url := 'http://' + Server + ':8080/';
  ShellExecute(Handle,
               'open',
               PChar(Url),
               nil,
               nil,
               SW_SHOWNORMAL);
end;


procedure TfrmFxServeAdmin.OpenPublicClick(Sender: TObject);
var
  Url: string;

begin

  Url := 'https://' + Trim(FPublicHostEdit.Text) + '/';
  ShellExecute(Handle,
               'open',
               PChar(Url),
               nil,
               nil,
               SW_SHOWNORMAL);
end;


procedure TfrmFxServeAdmin.SaveClick(Sender: TObject);
begin

  BeginBusy('saving and applying the FxServe settings.');

  try
    try
      SaveSettings();
      ApplyDeploymentSettings();
      AddLog('Admin profile and deployed server settings saved.');
    except
      on E: Exception do
        begin
          AddLog('The admin profile was saved, but deployment update failed: ' + E.Message);
          MessageDlg(E.Message,
                     mtError,
                     [mbOK],
                     0);
        end;
    end;
  finally
    EndBusy();
  end;
end;


procedure TfrmFxServeAdmin.ChangePasswordClick(Sender: TObject);
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
