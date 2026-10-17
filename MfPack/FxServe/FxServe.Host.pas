// FactoryX
//
// Copyright (c) FactoryX, Netherlands/Australia/Germany. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: FxServe.Host.pas
// Kind: Pascal Unit
// Release date: 10-08-2026
// Language: ENU
//
// Revision Version: 4.0.0
// Description: Service host.
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
unit FxServe.Host;

interface

uses

  {WinApi}
  WinApi.Windows,
  {System}
  System.SysUtils,
  {FxServe}
  FxServe.Config,
  FxServe.Logging,
  FxServe.Server,
  FxServe.Wan,
  FxServe.Certificate;

type

  TFxServeHost = class
  private
    FConfig: TFxServeConfig;
    FLogger: TFxServeLogger;
    FServer: TFxServeServer;
    FWanServer: TFxServeWanServer;
    FCertificateManager: TFxServeCertificateManager;
    FChallengeSource: IFxServeAcmeChallengeSource;
    FConfigFileName: string;
    FObservedWriteTime: UInt64;
    FObservedFileSize: UInt64;
    FObservedAtTick: Cardinal;
    FLastConfigPollTick: Cardinal;
    FObservedStateValid: Boolean;
    FReloadPending: Boolean;

    function ReadConfigFileState(out AWriteTime,
                                 AFileSize: UInt64): Boolean;
    procedure StartRuntime(AConfig: TFxServeConfig;
                           ALogger: TFxServeLogger;
                           out AServer: TFxServeServer;
                           out AWanServer: TFxServeWanServer;
                           out ACertificateManager: TFxServeCertificateManager;
                           out AChallengeSource: IFxServeAcmeChallengeSource);
    procedure StopRuntime(var AServer: TFxServeServer;
                          var AWanServer: TFxServeWanServer;
                          var ACertificateManager: TFxServeCertificateManager;
                          var AChallengeSource: IFxServeAcmeChallengeSource);
    procedure ReloadConfiguration();

  public

    destructor Destroy(); override;

    procedure Start(const AConfigFileName: string);
    procedure Stop;
    procedure CheckForConfigurationChanges();
    function IsRunning: Boolean;
  end;


implementation

const
  CONFIG_POLL_INTERVAL_MS = 250;
  CONFIG_STABLE_INTERVAL_MS = 1000;


function TFxServeHost.ReadConfigFileState(out AWriteTime,
                                          AFileSize: UInt64): Boolean;
var
  Data: TWin32FileAttributeData;
begin
  AWriteTime := 0;
  AFileSize := 0;
  FillChar(Data,
           SizeOf(Data),
           0);

  Result := GetFileAttributesEx(PChar(FConfigFileName),
                                GetFileExInfoStandard,
                                @Data);
  if not Result then
    Exit;

  AWriteTime := (UInt64(Data.ftLastWriteTime.dwHighDateTime) shl 32) or
                UInt64(Data.ftLastWriteTime.dwLowDateTime);
  AFileSize := (UInt64(Data.nFileSizeHigh) shl 32) or
               UInt64(Data.nFileSizeLow);
end;


procedure TFxServeHost.StartRuntime(
  AConfig: TFxServeConfig;
  ALogger: TFxServeLogger;
  out AServer: TFxServeServer;
  out AWanServer: TFxServeWanServer;
  out ACertificateManager: TFxServeCertificateManager;
  out AChallengeSource: IFxServeAcmeChallengeSource);
begin
  AServer := nil;
  AWanServer := nil;
  ACertificateManager := nil;
  AChallengeSource := nil;
  try
    AServer := TFxServeServer.Create(AConfig,
                                     ALogger);
    AServer.Start();

    if AConfig.WanEnabled then
      begin
        if AConfig.WanHttpsEnabled then
          begin
            ACertificateManager := TFxServeCertificateManager.Create(
              ALogger,
              AConfig.WanHostName,
              AConfig.WanHttpsPort,
              AConfig.WanHttpEnabled);
            AChallengeSource := ACertificateManager;
          end;

        AWanServer := TFxServeWanServer.Create(AConfig,
                                               ALogger,
                                               AChallengeSource);
        AWanServer.Start();

        if Assigned(ACertificateManager) then
          ACertificateManager.Start();
      end;
  except
    StopRuntime(AServer,
                AWanServer,
                ACertificateManager,
                AChallengeSource);
    raise;
  end;
end;


procedure TFxServeHost.StopRuntime(
  var AServer: TFxServeServer;
  var AWanServer: TFxServeWanServer;
  var ACertificateManager: TFxServeCertificateManager;
  var AChallengeSource: IFxServeAcmeChallengeSource);
begin
  if Assigned(ACertificateManager) then
    ACertificateManager.Stop();

  if Assigned(AWanServer) then
    begin
      AWanServer.Stop();
      FreeAndNil(AWanServer);
    end;

  AChallengeSource := nil;
  ACertificateManager := nil;

  if Assigned(AServer) then
    begin
      AServer.Stop();
      FreeAndNil(AServer);
    end;
end;


destructor TFxServeHost.Destroy();
begin

  Stop();

  inherited Destroy;
end;


procedure TFxServeHost.Start(const AConfigFileName: string);
begin

  if Assigned(FServer) then
    Exit;

  FConfigFileName := ExpandFileName(AConfigFileName);
  FConfig := TFxServeConfig.Create();

  try
    FConfig.LoadFromFile(FConfigFileName);
    FLogger := TFxServeLogger.Create(FConfig.LogFileName,
                                     FConfig.LogRetentionDays);

    try
      StartRuntime(FConfig,
                   FLogger,
                   FServer,
                   FWanServer,
                   FCertificateManager,
                   FChallengeSource);

      FObservedStateValid := ReadConfigFileState(FObservedWriteTime,
                                                  FObservedFileSize);
      FObservedAtTick := GetTickCount();
      FLastConfigPollTick := FObservedAtTick;
      FReloadPending := False;
      FLogger.Info('Configuration reload monitoring active: ' +
                   FConfigFileName);

    except
      FreeAndNil(FLogger);
      raise;
    end;

  except
    FreeAndNil(FConfig);
    raise;
  end;
end;


procedure TFxServeHost.Stop();
begin
  StopRuntime(FServer,
              FWanServer,
              FCertificateManager,
              FChallengeSource);

  FreeAndNil(FLogger);
  FreeAndNil(FConfig);
  FConfigFileName := '';
  FObservedStateValid := False;
  FReloadPending := False;
end;


procedure TFxServeHost.ReloadConfiguration();
var
  NewConfig: TFxServeConfig;
  NewLogger: TFxServeLogger;
  OldConfig: TFxServeConfig;
  OldLogger: TFxServeLogger;
begin
  NewConfig := TFxServeConfig.Create();
  NewLogger := nil;

  try
    // Parse every setting before disturbing the currently running listeners.
    NewConfig.LoadFromFile(FConfigFileName);
    NewLogger := TFxServeLogger.Create(NewConfig.LogFileName,
                                       NewConfig.LogRetentionDays);

    if Assigned(FLogger) then
      FLogger.Info('Configuration change detected; reloading.');

    StopRuntime(FServer,
                FWanServer,
                FCertificateManager,
                FChallengeSource);

    try
      StartRuntime(NewConfig,
                   NewLogger,
                   FServer,
                   FWanServer,
                   FCertificateManager,
                   FChallengeSource);
    except
      on E: Exception do
        begin
          NewLogger.Error('Configuration reload could not start: ' + E.Message);

          try
            StartRuntime(FConfig,
                         FLogger,
                         FServer,
                         FWanServer,
                         FCertificateManager,
                         FChallengeSource);
            FLogger.Info('Previous configuration restored after reload failure.');
          except
            on RestoreError: Exception do
              FLogger.Error('Previous configuration could not be restored: ' +
                            RestoreError.Message);
          end;

          Exit;
        end;
    end;

    OldConfig := FConfig;
    OldLogger := FLogger;
    FConfig := NewConfig;
    FLogger := NewLogger;
    NewConfig := nil;
    NewLogger := nil;

    FLogger.Info('Configuration reload completed.');
    OldLogger.Free;
    OldConfig.Free;
  finally
    NewLogger.Free;
    NewConfig.Free;
  end;
end;


procedure TFxServeHost.CheckForConfigurationChanges();
var
  CurrentWriteTime: UInt64;
  CurrentFileSize: UInt64;
  CurrentTick: Cardinal;
begin
  if (FConfigFileName = '') or not Assigned(FConfig) then
    Exit;

  CurrentTick := GetTickCount();
  if Cardinal(CurrentTick - FLastConfigPollTick) < CONFIG_POLL_INTERVAL_MS then
    Exit;
  FLastConfigPollTick := CurrentTick;

  if not ReadConfigFileState(CurrentWriteTime,
                             CurrentFileSize) then
    begin
      if FObservedStateValid and Assigned(FLogger) then
        FLogger.Error('Configuration file is unavailable; current settings remain active.');
      FObservedStateValid := False;
      FReloadPending := False;
      Exit;
    end;

  if (not FObservedStateValid) or
     (CurrentWriteTime <> FObservedWriteTime) or
     (CurrentFileSize <> FObservedFileSize) then
    begin
      FObservedWriteTime := CurrentWriteTime;
      FObservedFileSize := CurrentFileSize;
      FObservedAtTick := CurrentTick;
      FObservedStateValid := True;
      FReloadPending := True;
      Exit;
    end;

  if not FReloadPending or
     (Cardinal(CurrentTick - FObservedAtTick) < CONFIG_STABLE_INTERVAL_MS) then
    Exit;

  // Mark this stable version handled. A rejected version is retried only after
  // the file changes again, avoiding a repeating error every polling interval.
  FReloadPending := False;
  try
    ReloadConfiguration();
  except
    on E: Exception do
      if Assigned(FLogger) then
        FLogger.Error('Configuration reload rejected; current settings remain active: ' +
                      E.Message);
  end;
end;

function TFxServeHost.IsRunning(): Boolean;
begin

  Result := Assigned(FServer) and
            FServer.Running and
            (not FConfig.WanEnabled or
            (Assigned(FWanServer) and
            FWanServer.Running));
end;

end.
