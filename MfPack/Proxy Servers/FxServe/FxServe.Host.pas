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

  public

    destructor Destroy(); override;

    procedure Start(const AConfigFileName: string);
    procedure Stop;
    function IsRunning: Boolean;
  end;


implementation


destructor TFxServeHost.Destroy();
begin

  Stop();

  inherited Destroy;
end;


procedure TFxServeHost.Start(const AConfigFileName: string);
begin

  if Assigned(FServer) then
    Exit;

  FConfig := TFxServeConfig.Create();

  try
    FConfig.LoadFromFile(AConfigFileName);
    FLogger := TFxServeLogger.Create(FConfig.LogFileName,
                                     FConfig.LogRetentionDays);

    try
      FServer := TFxServeServer.Create(FConfig, FLogger);

      try
        FServer.Start();

        if FConfig.WanEnabled then
          begin
            if FConfig.WanHttpsEnabled then
              begin
                FCertificateManager := TFxServeCertificateManager.Create(FLogger,
                                                                          FConfig.WanHostName,
                                                                          FConfig.WanHttpsPort,
                                                                          FConfig.WanHttpEnabled);
                FChallengeSource := FCertificateManager;
              end;

            FWanServer := TFxServeWanServer.Create(FConfig,
                                                   FLogger,
                                                   FChallengeSource);
            try
              FWanServer.Start();

              if Assigned(FCertificateManager) then
                FCertificateManager.Start();

            except
              if Assigned(FCertificateManager) then
                FCertificateManager.Stop();

              FreeAndNil(FWanServer);
              FChallengeSource := nil;
              FCertificateManager := nil;
              raise;
            end; //try
        end;

      except
        FreeAndNil(FServer);
        raise;
      end;

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

  if Assigned(FCertificateManager) then
    FCertificateManager.Stop();

  if Assigned(FWanServer) then
    begin
      FWanServer.Stop();
      FreeAndNil(FWanServer);
    end;

  FChallengeSource := nil;
  FCertificateManager := nil;

  if Assigned(FServer) then
    begin
      FServer.Stop();
      FreeAndNil(FServer);
    end;

  FreeAndNil(FLogger);
  FreeAndNil(FConfig);
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
