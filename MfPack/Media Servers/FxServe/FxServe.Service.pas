// FactoryX
//
// Copyright © FactoryX, Netherlands/Australia/Germany. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: FxServe.Service.pas
// Kind: Pascal Unit
// Release date: 10-08-2026
// Language: ENU
//
// Revision Version: 4.0.0
// Description: Service unit.
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
unit FxServe.Service;

interface

uses

  {WinApi}
  WinApi.Windows,
  WinApi.WinSvc,
  {System}
  System.SysUtils;

const
  FX_SERVE_SERVICE_NAME = 'FxServe';
  FX_SERVE_DISPLAY_NAME = 'FactoryX FxServe';

  procedure RunFxServeService(const AConfigFileName: string);
  procedure InstallFxServeService(const AConfigFileName: string);
  procedure UninstallFxServeService();


implementation

uses
  {FxServe}
  FxServe.Host;

var
  GConfigFileName: string;
  GStatusHandle: SERVICE_STATUS_HANDLE;
  GStatus: TServiceStatus;
  GStopEvent: THandle;
  GCheckPoint: DWORD;

procedure ReportServiceStatus(ACurrentState: DWORD;
                              AWin32ExitCode: DWORD;
                              AWaitHint: DWORD);
begin

  GStatus.dwCurrentState := ACurrentState;
  GStatus.dwWin32ExitCode := AWin32ExitCode;
  GStatus.dwWaitHint := AWaitHint;

  if (ACurrentState = SERVICE_START_PENDING) then
    GStatus.dwControlsAccepted := 0
  else
    GStatus.dwControlsAccepted := SERVICE_ACCEPT_STOP or
      SERVICE_ACCEPT_SHUTDOWN;

  if (ACurrentState = SERVICE_RUNNING) or (ACurrentState = SERVICE_STOPPED) then
    begin
      GStatus.dwCheckPoint := 0;
      GCheckPoint := 0;
    end
  else
    begin
      Inc(GCheckPoint);
      GStatus.dwCheckPoint := GCheckPoint;
    end;
  SetServiceStatus(GStatusHandle,
                   GStatus);
end;


procedure ServiceControlHandler(AControl: DWORD); stdcall;
begin

  case AControl of
    SERVICE_CONTROL_STOP,
    SERVICE_CONTROL_SHUTDOWN: begin
                                if (GStatus.dwCurrentState = SERVICE_RUNNING) then
                                  begin
                                    ReportServiceStatus(SERVICE_STOP_PENDING,
                                                        NO_ERROR,
                                                        15000);
                                    if (GStopEvent <> 0) then
                                      SetEvent(GStopEvent);
                                  end;
                              end;

    SERVICE_CONTROL_INTERROGATE: SetServiceStatus(GStatusHandle,
                                                  GStatus);
  end;
end;


procedure FxServeServiceMain(AArgCount: DWORD;
                             AArgVectors: PLPWSTR); stdcall;
var
  Host: TFxServeHost;
  ExitCode: DWORD;

begin

  FillChar(GStatus,
          SizeOf(GStatus),
          0);

  GStatus.dwServiceType := SERVICE_WIN32_OWN_PROCESS;
  GStatusHandle := RegisterServiceCtrlHandler(FX_SERVE_SERVICE_NAME,
                                              @ServiceControlHandler);
  if (GStatusHandle = 0) then
    Exit;

  GStopEvent := CreateEvent(nil,
                            True,
                            False,
                            nil);
  if (GStopEvent = 0) then
    begin
      ReportServiceStatus(SERVICE_STOPPED,
                          GetLastError,
                          0);
      Exit;
    end;

  Host := nil;
  ExitCode := NO_ERROR;

  try
    ReportServiceStatus(SERVICE_START_PENDING,
                        NO_ERROR,
                        15000);
    try
      Host := TFxServeHost.Create();
      Host.Start(GConfigFileName);

      ReportServiceStatus(SERVICE_RUNNING,
                          NO_ERROR,
                          0);

      WaitForSingleObject(GStopEvent,
                          INFINITE);

      ReportServiceStatus(SERVICE_STOP_PENDING,
                          NO_ERROR,
                          15000);
      Host.Stop();

    except
      on E: Exception do
      begin
        OutputDebugString(PChar('FxServe service error: ' + E.Message));
        ExitCode := ERROR_SERVICE_SPECIFIC_ERROR;
        GStatus.dwServiceSpecificExitCode := 1;
      end;
    end;

  finally
    Host.Free;
    CloseHandle(GStopEvent);
    GStopEvent := 0;

    ReportServiceStatus(SERVICE_STOPPED,
                        ExitCode,
                        0);
  end;
end;


procedure RunFxServeService(const AConfigFileName: string);
var
  ServiceTable: array[0..1] of TServiceTableEntry;

begin

  GConfigFileName := ExpandFileName(AConfigFileName);
  FillChar(ServiceTable,
           SizeOf(ServiceTable),
           0);

  ServiceTable[0].lpServiceName := PChar(FX_SERVE_SERVICE_NAME);
  ServiceTable[0].lpServiceProc := @FxServeServiceMain;

  if not StartServiceCtrlDispatcher(ServiceTable[0]) then
    raise EOSError.CreateFmt('StartServiceCtrlDispatcher failed: %s',
                             [SysErrorMessage(GetLastError)]);
end;


function ServiceBinaryCommand(const AConfigFileName: string): string;
begin

  Result := '"' + ExpandFileName(ParamStr(0)) + '" --service --config "' + ExpandFileName(AConfigFileName) + '"';
end;


procedure InstallFxServeService(const AConfigFileName: string);
var
  Manager: SC_HANDLE;
  Service: SC_HANDLE;
  CommandLine: string;

begin

  if not FileExists(AConfigFileName) then
    raise Exception.CreateFmt('Configuration file not found: %s',
                              [ExpandFileName(AConfigFileName)]);

  Manager := OpenSCManager(nil,
                          nil,
                          SC_MANAGER_CREATE_SERVICE);

  if (Manager = 0) then
    RaiseLastOSError();

  try
    CommandLine := ServiceBinaryCommand(AConfigFileName);

    Service := CreateService(Manager,
                             FX_SERVE_SERVICE_NAME,
                             FX_SERVE_DISPLAY_NAME,
                             SERVICE_ALL_ACCESS,
                             SERVICE_WIN32_OWN_PROCESS,
                             SERVICE_AUTO_START,
                             SERVICE_ERROR_NORMAL,
                             PChar(CommandLine),
                             nil,
                             nil,
                             nil,
                             nil,
                             nil);
    if (Service = 0) then
      RaiseLastOSError;

    try
      Writeln('Installed Windows service: ' + FX_SERVE_DISPLAY_NAME);
      Writeln('Binary command: ' + CommandLine);

    finally
      CloseServiceHandle(Service);
    end;

  finally
    CloseServiceHandle(Manager);
  end;
end;


procedure UninstallFxServeService();
var
  Manager: SC_HANDLE;
  Service: SC_HANDLE;
  Status: TServiceStatus;

begin

  Manager := OpenSCManager(nil,
                           nil,
                           SC_MANAGER_CONNECT);
  if (Manager = 0) then
    RaiseLastOSError();

  try
    Service := OpenService(Manager,
                           FX_SERVE_SERVICE_NAME,
                           SERVICE_ALL_ACCESS);
    if (Service = 0) then
      RaiseLastOSError();

    try
      if QueryServiceStatus(Service,
                            Status) and
         (Status.dwCurrentState <> SERVICE_STOPPED) then
      begin
        ControlService(Service,
                       SERVICE_CONTROL_STOP,
                       Status);
        Sleep(500);
      end;

      if not DeleteService(Service) then
        RaiseLastOSError();

      Writeln('Removed Windows service: ' + FX_SERVE_DISPLAY_NAME);

    finally
      CloseServiceHandle(Service);
    end;

  finally
    CloseServiceHandle(Manager);
  end;
end;

end.
