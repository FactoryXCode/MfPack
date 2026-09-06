// FactoryX
//
// Copyright © FactoryX, Netherlands/Australia/Germany. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: FxServe.Config.pas
// Kind: Pascal Unit
// Release date: 10-08-2026
// Language: ENU
//
// Revision Version: 4.0.0
// Description: Service configurator that reads the ini values.
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
unit FxServe.Config;

interface

uses

  {System}
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  System.IniFiles;

type

  TFxServeConfig = class
  private
    FBindAddress: string;
    FPort: Word;
    FWebRoot: string;
    FIndexFile: string;
    FMaxConnections: Integer;
    FHeaderTimeoutMs: Integer;
    FSendTimeoutMs: Integer;
    FProxyEnabled: Boolean;
    FProxyHost: string;
    FProxyPort: Word;
    FProxyRoutes: TStringList;
    FCorsEnabled: Boolean;
    FNoStoreRoutes: TStringList;
    FLogFileName: string;
    FConfigFileName: string;
    FWanEnabled: Boolean;
    FWanHostName: string;
    FWanHttpEnabled: Boolean;
    FWanHttpPort: Word;
    FWanHttpsEnabled: Boolean;
    FWanHttpsPort: Word;
    FWanRedirectHttp: Boolean;

    procedure ReadRouteList(AIni: TCustomIniFile;
                            const ASection: string;
                            const AName: string;
                            const ADefault: string;
                            AList: TStrings);

  public

    constructor Create();
    destructor Destroy(); override;

    procedure LoadFromFile(const AFileName: string);
    function IsProxyRoute(const APath: string): Boolean;
    function IsNoStoreRoute(const APath: string): Boolean;

    property BindAddress: string read FBindAddress;
    property Port: Word read FPort;
    property WebRoot: string read FWebRoot;
    property IndexFile: string read FIndexFile;
    property MaxConnections: Integer read FMaxConnections;
    property HeaderTimeoutMs: Integer read FHeaderTimeoutMs;
    property SendTimeoutMs: Integer read FSendTimeoutMs;
    property ProxyEnabled: Boolean read FProxyEnabled;
    property ProxyHost: string read FProxyHost;
    property ProxyPort: Word read FProxyPort;
    property CorsEnabled: Boolean read FCorsEnabled;
    property LogFileName: string read FLogFileName;
    property ConfigFileName: string read FConfigFileName;
    property WanEnabled: Boolean read FWanEnabled;
    property WanHostName: string read FWanHostName;
    property WanHttpEnabled: Boolean read FWanHttpEnabled;
    property WanHttpPort: Word read FWanHttpPort;
    property WanHttpsEnabled: Boolean read FWanHttpsEnabled;
    property WanHttpsPort: Word read FWanHttpsPort;
    property WanRedirectHttp: Boolean read FWanRedirectHttp;
  end;


implementation

// Helper
function ReadIniBoolean(AIni: TCustomIniFile;
                        const ASection: string;
                        const AName: string;
                        ADefault: Boolean): Boolean;
var
  Text: string;

begin

  Text := LowerCase(Trim(AIni.ReadString(ASection,
                                         AName,
                                         '')));

  if (Text = '1') or
     (Text = 'true') or
     (Text = 'yes') or
     (Text = 'on') then
    Exit(True);

  if (Text = '0') or
     (Text = 'false') or
     (Text = 'no') or
     (Text = 'off') then
    Exit(False);

  Result := ADefault;
end;


function NormalizeRoute(const ARoute: string): string;
begin

  Result := Trim(ARoute);

  if (Result = '') then
    Exit;

  if (Result[1] <> '/') then
    Result := '/' + Result;

  while (Length(Result) > 1) and (Result[Length(Result)] = '/') do
    Delete(Result,
           Length(Result),
           1);

  Result := LowerCase(Result);
end;


function RouteMatches(const APath: string;
                      const ARoute: string): Boolean;
var
  PathText: string;

begin

  PathText := LowerCase(APath);

  Result := (PathText = ARoute) or
            ((Copy(PathText,
                   1,
                   Length(ARoute)) = ARoute) and
             (Length(PathText) > Length(ARoute)) and
             (PathText[Length(ARoute) + 1] = '/'));
end;


constructor TFxServeConfig.Create();
begin

  inherited Create();

  FProxyRoutes := TStringList.Create;
  FNoStoreRoutes := TStringList.Create;
end;


destructor TFxServeConfig.Destroy();
begin

  FNoStoreRoutes.Free;
  FProxyRoutes.Free;

  inherited Destroy();
end;


procedure TFxServeConfig.ReadRouteList(AIni: TCustomIniFile;
                                       const ASection: string;
                                       const AName: string;
                                       const ADefault: string;
                                       AList: TStrings);
var
  Values: TStringList;
  I: Integer;
  Route: string;

begin

  AList.Clear();
  Values := TStringList.Create();

  try
    Values.StrictDelimiter := True;
    Values.Delimiter := ',';
    Values.DelimitedText := AIni.ReadString(ASection,
                                            AName,
                                            ADefault);
    for I := 0 to Values.Count - 1 do
      begin
        Route := NormalizeRoute(Values[I]);
        if (Route <> '') then
          AList.Add(Route);
      end;

  finally
    Values.Free;
  end;
end;


procedure TFxServeConfig.LoadFromFile(const AFileName: string);
var
  Ini: TMemIniFile;
  BaseDir: string;
  Value: Integer;

begin

  FConfigFileName := ExpandFileName(AFileName);

  if not FileExists(FConfigFileName) then
    raise Exception.CreateFmt('Configuration file not found: %s',
                              [FConfigFileName]);

  BaseDir := ExtractFileDir(FConfigFileName);
  Ini := TMemIniFile.Create(FConfigFileName);

  try
    FBindAddress := Trim(Ini.ReadString('Server',
                                        'BindAddress',
                                        '0.0.0.0'));

    Value := Ini.ReadInteger('Server',
                             'Port',
                             8080);

    if (Value < 1) or (Value > 65535) then
      raise Exception.Create('Server Port must be between 1 and 65535.');

    FPort := Value;

    FWebRoot := Trim(Ini.ReadString('Server',
                                    'WebRoot',
                                    '.\www'));

    if not TPath.IsPathRooted(FWebRoot) then
      FWebRoot := ExpandFileName(IncludeTrailingPathDelimiter(BaseDir) + FWebRoot)
    else
      FWebRoot := ExpandFileName(FWebRoot);

    FWebRoot := ExcludeTrailingPathDelimiter(FWebRoot);

    FIndexFile := Trim(Ini.ReadString('Server',
                                      'IndexFile',
                                      'index.html'));

    if (FIndexFile = '') or
       (Pos('\', FIndexFile) > 0) or
       (Pos('/', FIndexFile) > 0) then
      raise Exception.Create('IndexFile must contain a file name only.');

    FMaxConnections := Ini.ReadInteger('Server',
                                       'MaxConnections',
                                       128);

    if (FMaxConnections < 1) then
      FMaxConnections := 1;

    FHeaderTimeoutMs := Ini.ReadInteger('Server',
                                        'HeaderTimeoutMs',
                                        10000);

    if (FHeaderTimeoutMs < 1000) then
      FHeaderTimeoutMs := 1000;

    FSendTimeoutMs := Ini.ReadInteger('Server',
                                      'SendTimeoutMs',
                                      30000);
    if (FSendTimeoutMs < 1000) then
      FSendTimeoutMs := 1000;

    FWanEnabled := ReadIniBoolean(Ini,
                                  'Wan',
                                  'Enabled',
                                  False);

    FWanHostName := LowerCase(Trim(Ini.ReadString('Wan',
                                                  'HostName',
                                                  '')));

    FWanHttpEnabled := ReadIniBoolean(Ini,
                                      'Wan',
                                      'HttpEnabled',
                                      True);

    Value := Ini.ReadInteger('Wan',
                             'HttpPort',
                             80);

    if (Value < 1) or (Value > 65535) then
      raise Exception.Create('WAN HTTP Port must be between 1 and 65535.');

    FWanHttpPort := Value;
    FWanHttpsEnabled := ReadIniBoolean(Ini,
                                       'Wan',
                                       'HttpsEnabled',
                                       True);

    Value := Ini.ReadInteger('Wan',
                             'HttpsPort',
                             443);

    if (Value < 1) or (Value > 65535) then
      raise Exception.Create('WAN HTTPS Port must be between 1 and 65535.');

    FWanHttpsPort := Value;
    FWanRedirectHttp := ReadIniBoolean(Ini,
                                       'Wan',
                                       'RedirectHttp',
                                       True);

    if FWanEnabled then
      begin
        if not FWanHttpEnabled and not FWanHttpsEnabled then
          raise Exception.Create('WAN mode must enable HTTP, HTTPS, or both.');

        if (FWanHttpsEnabled or FWanRedirectHttp) and (FWanHostName = '') then
          raise Exception.Create('WAN HostName is required for HTTPS or redirect.');

        if FWanHttpEnabled and (FWanHttpPort = FPort) then
          raise Exception.Create('WAN HTTP Port must differ from the LAN Server Port.');

        if FWanHttpsEnabled and (FWanHttpsPort = FPort) then
          raise Exception.Create('WAN HTTPS Port must differ from the LAN Server Port.');

        if FWanHttpEnabled and FWanHttpsEnabled and (FWanHttpPort = FWanHttpsPort) then
          raise Exception.Create('WAN HTTP and HTTPS ports must differ.');
      end;

    FProxyEnabled := ReadIniBoolean(Ini,
                                    'Proxy',
                                    'Enabled',
                                    True);

    FProxyHost := Trim(Ini.ReadString('Proxy',
                                      'Host',
                                      '127.0.0.1'));

    Value := Ini.ReadInteger('Proxy',
                             'Port',
                             8000);

    if (Value < 1) or (Value > 65535) then
      raise Exception.Create('Proxy Port must be between 1 and 65535.');

    FProxyPort := Value;

    ReadRouteList(Ini,
                  'Proxy',
                  'Routes',
                  '/live,/status,/video.mjpg',
                  FProxyRoutes);

    FCorsEnabled := ReadIniBoolean(Ini,
                                   'Headers',
                                   'Cors',
                                   True);

    ReadRouteList(Ini,
                  'Headers',
                  'NoStoreRoutes',
                  '/stream,/video,/nowplaying.json',
                  FNoStoreRoutes);

    FLogFileName := Trim(Ini.ReadString('Logging',
                                        'File',
                                        'FxServe.log'));

    if (FLogFileName <> '') then
    begin
      if not TPath.IsPathRooted(FLogFileName) then
        FLogFileName := ExpandFileName(IncludeTrailingPathDelimiter(BaseDir) + FLogFileName)
      else
        FLogFileName := ExpandFileName(FLogFileName);
    end;

  finally
    Ini.Free;
  end;

end;


function TFxServeConfig.IsProxyRoute(const APath: string): Boolean;
var
  I: Integer;

begin

  Result := False;

  if not FProxyEnabled then
    Exit;

  for I := 0 to FProxyRoutes.Count - 1 do
    if RouteMatches(APath, FProxyRoutes[I]) then
      Exit(True);
end;


function TFxServeConfig.IsNoStoreRoute(const APath: string): Boolean;
var
  I: Integer;

begin

  Result := False;

  for I := 0 to FNoStoreRoutes.Count - 1 do
    if RouteMatches(APath, FNoStoreRoutes[I]) then
      Exit(True);
end;

end.
