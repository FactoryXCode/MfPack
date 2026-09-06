// FactoryX
//
// Copyright � FactoryX, Netherlands/Australia/Germany. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: FxServe.dpr
// Kind: Pascal Unit
// Release date: 10-08-2026
// Language: ENU
//
// Revision Version: 4.0.0
// Description: Command line reverse proxy service.
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
// Source: Microsoft Learn.
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
program FxServe;

{$APPTYPE CONSOLE}

uses

  {WinApi}
  WinApi.Windows,
  {System}
  System.SysUtils,
  System.Classes,
  {FxServe}
  FxServe.Config in 'FxServe.Config.pas',
  FxServe.Logging in 'FxServe.Logging.pas',
  FxServe.HttpApi in 'FxServe.HttpApi.pas',
  FxServe.Certificate in 'FxServe.Certificate.pas',
  FxServe.Server in 'FxServe.Server.pas',
  FxServe.Wan in 'FxServe.Wan.pas',
  FxServe.Host in 'FxServe.Host.pas',
  FxServe.Service in 'FxServe.Service.pas';

var
  StopRequested: LongBool = False;

function ConsoleControlHandler(ACtrlType: DWORD): BOOL; stdcall;
begin

  case ACtrlType of
    CTRL_C_EVENT,
    CTRL_BREAK_EVENT,
    CTRL_CLOSE_EVENT,
    CTRL_LOGOFF_EVENT,
    CTRL_SHUTDOWN_EVENT:
      begin
        StopRequested := True;
        Result := True;
      end;
  else
    Result := False;
  end;
end;


function FindConfigFileName(): string;
var
  I: Integer;

begin

  Result := ChangeFileExt(ParamStr(0),
                          '.ini');

  if not FileExists(Result) and FileExists('FxServe.ini') then
    Result := ExpandFileName('FxServe.ini');

  I := 1;

  while I <= ParamCount do
    begin
      if SameText(ParamStr(I),
                  '--config') or SameText(ParamStr(I),
                                          '-c') then
        begin
          if (I = ParamCount) then
            raise Exception.Create('A file name must follow --config.');

          Result := ParamStr(I + 1);
          Exit;
        end;
      Inc(I);
    end;
end;


function HasSwitch(const ASwitch: string): Boolean;
var
  I: Integer;

begin

  Result := False;

  for I := 1 to ParamCount do
    if SameText(ParamStr(I),
                ASwitch) then
      Exit(True);
end;


function SwitchValue(const ASwitch: string): string;
var
  I: Integer;

begin

  Result := '';

  for I := 1 to ParamCount - 1 do
    if SameText(ParamStr(I),
                ASwitch) then
      Exit(ParamStr(I + 1));
end;


procedure RunConsole(const AConfigFileName: string);
var
  Host: TFxServeHost;

begin

  Host := TFxServeHost.Create;

  try
    SetConsoleCtrlHandler(@ConsoleControlHandler,
                          True);
    StopRequested := False;
    Host.Start(AConfigFileName);
    Writeln('Press Ctrl+C to stop.');

    while not StopRequested do
      Sleep(100);

    Host.Stop();
  finally
    Host.Free;
  end;
end;


procedure Run();
var
  ConfigFileName: string;
  HostName: string;
  Email: string;

begin

  if HasSwitch('--certificate-setup') then
    begin
      HostName := SwitchValue('--host');
      Email := SwitchValue('--email');
      ConfigureFxServeCertificate(HostName,
                                  Email,
                                  HasSwitch('--accept-ca-terms'),
                                  HasSwitch('--staging'));
      Writeln('FxServe internal certificate management configured for ' + HostName + '.');
      Exit;
    end;

  if HasSwitch('--certificate-disable') then
    begin
      DisableFxServeCertificate();
      Writeln('FxServe internal certificate management disabled.');
      Exit;
    end;

  ConfigFileName := FindConfigFileName;

  if HasSwitch('--install') then
    InstallFxServeService(ConfigFileName)
  else
    if HasSwitch('--uninstall') then
      UninstallFxServeService
    else
      if HasSwitch('--service') then
        RunFxServeService(ConfigFileName)
      else
        RunConsole(ConfigFileName);
end;

begin

  try
    Run();

  except
    on E: Exception do
    begin
      Writeln(ErrOutput,
              'FxServe: ' + E.Message);
      ExitCode := 1;
    end;
  end;
end.
