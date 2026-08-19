// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: frmCaddyConfigEditor.pas
// Kind: Pascal / Delphi unit
// Release date: 02-04-2023
// Language: ENU
//
// Revision Version: 3.2.0
// Description: Dialog to edit Caddy Conifiguration file settings.
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX)
// Contributor(s): Tony Kalf (maXcomX).
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 05/05/2026 All                 Bauhaus release  SDK 10.0.26100.4654 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 or later.
//          Works with RDJ and RDJ Pro Caddy configurations on local or remote servers.
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
unit frmCaddyConfigEditor;

interface

uses

  {WinApi}
  WinApi.Windows,
  {System}
  System.SysUtils,
  System.UITypes,
  System.Classes,
  System.IniFiles,
  {Vcl}
  Vcl.Controls,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.Forms,
  Vcl.Dialogs;

type

  TfrmCaddyConfigEditor = class(TForm)
    lblConfigFile: TLabel;
    lblSiteAddress: TLabel;
    lblCaddyRoot: TLabel;
    lblLogFile: TLabel;
    lblProxyHost: TLabel;
    lblProxyPort: TLabel;
    edtConfigFile: TEdit;
    edtSiteAddress: TEdit;
    edtCaddyRoot: TEdit;
    edtLogFile: TEdit;
    edtProxyHost: TEdit;
    edtProxyPort: TEdit;
    btnSave: TButton;
    btnCancel: TButton;
    Bevel1: TBevel;
    procedure btnSaveClick(Sender: TObject);

  private

    FConfigFileName: string;
    FSetupIniFileName: string;
    FConfigLines: TStringList;

    procedure LoadConfig();
    procedure SaveConfig();
    procedure ApplyConfig();
    procedure UpdateSetupIni();
    function FindFirstValue(const APrefix: string): string;
    function LeadingSpaces(const S: string): string;

    function SplitProxyTarget(const ATarget: string;
                              out AHost: string;
                              out APort: string): Boolean;

    function BuildProxyTarget: string;
    function BuildCaddyCommand(): string;
    function BuildCaddyLogFileValue(): string;
    function ClientCaddyRoot(): string;

  public

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function Execute(const AConfigFileName: string;
                     const ASetupIniFileName: string): Boolean;
  end;

implementation

{$R *.dfm}

constructor TfrmCaddyConfigEditor.Create(AOwner: TComponent);
begin

  inherited Create(AOwner);
  FConfigLines := TStringList.Create();

end;


destructor TfrmCaddyConfigEditor.Destroy();
begin

  FConfigLines.Free;
  inherited Destroy;
end;


function TfrmCaddyConfigEditor.Execute(const AConfigFileName: string;
                                       const ASetupIniFileName: string): Boolean;
begin

  FConfigFileName := AConfigFileName;
  FSetupIniFileName := ASetupIniFileName;
  edtConfigFile.Text := FConfigFileName;
  LoadConfig;
  Result := ShowModal = mrOK;
end;


function TfrmCaddyConfigEditor.LeadingSpaces(const S: string): string;
var
  I: Integer;

begin

  I := 1;
  while (I <= Length(S)) and (S[I] <= ' ') do
    Inc(I);
  Result := Copy(S, 1, I - 1);
end;


function TfrmCaddyConfigEditor.FindFirstValue(const APrefix: string): string;
var
  I: Integer;
  S: string;

begin

  Result := '';
  for I := 0 to FConfigLines.Count - 1 do
    begin
      S := Trim(FConfigLines[I]);
      if SameText(Copy(S, 1, Length(APrefix)),
                  APrefix) then
        Exit(Trim(Copy(S, Length(APrefix) + 1, MaxInt)));
    end;
end;


function TfrmCaddyConfigEditor.SplitProxyTarget(const ATarget: string;
                                                out AHost: string;
                                                out APort: string): Boolean;
var
  P: Integer;

begin

  AHost := '';
  APort := '';

  P := LastDelimiter(':',
                     ATarget);
  Result := (P > 0);
  if Result then
    begin
      AHost := Trim(Copy(ATarget,
                    1,
                    P - 1));
      APort := Trim(Copy(ATarget,
                         P + 1,
                         MaxInt));
    end;
end;


function TfrmCaddyConfigEditor.BuildProxyTarget(): string;
begin

  Result := Trim(edtProxyHost.Text) + ':' + Trim(edtProxyPort.Text);
end;


function TfrmCaddyConfigEditor.ClientCaddyRoot(): string;
begin

  Result := ExcludeTrailingPathDelimiter(ExtractFileDir(FConfigFileName));
end;


function TfrmCaddyConfigEditor.BuildCaddyCommand(): string;
var
  Root: string;
begin

  Root := IncludeTrailingPathDelimiter(Trim(edtCaddyRoot.Text));
  Result := '"' + Root + 'caddy.exe" run --config "' + Root + 'caddy.cff" --adapter caddyfile';
end;


function TfrmCaddyConfigEditor.BuildCaddyLogFileValue(): string;
var
  LogFileName: string;
  LogDir: string;
  ServerRoot: string;

begin

  LogFileName := Trim(edtLogFile.Text);
  if LogFileName = '' then
    Exit('Caddy.log');

  Result := LogFileName;

  LogDir := ExcludeTrailingPathDelimiter(ExtractFileDir(LogFileName));
  ServerRoot := ExcludeTrailingPathDelimiter(Trim(edtCaddyRoot.Text));

  if (LogDir <> '') and
     (ServerRoot <> '') and
     SameText(LogDir,
              ServerRoot) then
    Result := ExtractFileName(LogFileName);
end;


procedure TfrmCaddyConfigEditor.LoadConfig();
var
  I: Integer;
  S: string;
  Value: string;
  Host: string;
  Port: string;

begin

  if not FileExists(FConfigFileName) then
    raise Exception.CreateFmt('Caddy configuration file not found:'#13#10'%s',
                              [FConfigFileName]);

  FConfigLines.LoadFromFile(FConfigFileName);

  for I := 0 to FConfigLines.Count - 1 do
    begin

      S := Trim(FConfigLines[I]);
      if (S <> '') and (S[Length(S)] = '{') then
        begin
          edtSiteAddress.Text := Trim(Copy(S,
                                           1,
                                           Length(S) - 1));
          Break;
        end;
    end;

  edtCaddyRoot.Text := FindFirstValue('root * ');

  Value := FindFirstValue('output file ');
  if (Value <> '') and (Value[Length(Value)] = '{') then
    Value := Trim(Copy(Value,
                       1,
                       Length(Value) - 1));
  edtLogFile.Text := Value;

  Value := FindFirstValue('reverse_proxy ');
  if Pos(' ',
         Value) > 0 then
    Value := Copy(Value,
                  1,
                  Pos(' ',
                      Value) - 1);

  if SplitProxyTarget(Value,
                      Host,
                      Port) then
    begin

      edtProxyHost.Text := Host;
      edtProxyPort.Text := Port;
    end;
end;


procedure TfrmCaddyConfigEditor.ApplyConfig;
var
  I: Integer;
  J: Integer;
  Line: string;
  S: string;
  Prefix: string;
  Suffix: string;
  P: Integer;
  HasFileBlockEnd: Boolean;

begin

  { Normalize the file writer block. The braces are Caddy syntax, not part of
    the log filename exposed by the editor. }
  for I := 0 to FConfigLines.Count - 1 do
    begin

      S := Trim(FConfigLines[I]);
      if SameText(Copy(S,
                       1,
                       12),
                       'output file ') then
        begin

          HasFileBlockEnd := False;
          J := I + 1;

          while (J < FConfigLines.Count) do
            begin
              S := Trim(FConfigLines[J]);
              if SameText(Copy(S,
                               1,
                               7),
                               'format ') then
                Break;
              if S = '}' then
                begin

                  HasFileBlockEnd := True;
                  Break;
                end;
              Inc(J);
            end;

          if not HasFileBlockEnd then
            FConfigLines.Insert(J,
                                LeadingSpaces(FConfigLines[I]) + '}');
          Break;
        end;
    end;

  for I := 0 to FConfigLines.Count - 1 do
    begin

      Line := FConfigLines[I];
      S := Trim(Line);

      if (S <> '') and
         (S[Length(S)] = '{') and
         (Pos(' ',
              Copy(S,
                   1,
                   Length(S) - 1)) = 0) then
        begin
          FConfigLines[I] := LeadingSpaces(Line) + Trim(edtSiteAddress.Text) + ' {';
          Continue;
        end;

      if SameText(Copy(S,
                       1,
                       7),
                       'root * ') then
        begin

          FConfigLines[I] := LeadingSpaces(Line) + 'root * ' + Trim(edtCaddyRoot.Text);
          Continue;
        end;

      if SameText(Copy(S,
                       1,
                       12),
                       'output file ') then
        begin
          FConfigLines[I] := LeadingSpaces(Line) + 'output file ' +
                             Trim(edtLogFile.Text) + ' {';
          Continue;
        end;

      if SameText(Copy(S,
                       1,
                       14),
                       'reverse_proxy ') then
        begin
          Prefix := LeadingSpaces(Line) + 'reverse_proxy ';
          Suffix := '';

          P := Pos(' ',
                   Trim(Copy(S,
                             15,
                             MaxInt)));
          if (P > 0) then
            Suffix := Copy(Trim(Copy(S,
                                     15,
                                     MaxInt)),
                           P,
                           MaxInt);
          FConfigLines[I] := Prefix + BuildProxyTarget + Suffix;
        end;
    end;
end;


procedure TfrmCaddyConfigEditor.UpdateSetupIni();
var
  Ini: TIniFile;
  CaddyRoot: string;
  SetupIniFileName: string;
  IsRdj: Boolean;
  IsRdjPro: Boolean;
  
begin

  SetupIniFileName := Trim(FSetupIniFileName);
  if (SetupIniFileName = '') then
    Exit;

  if not FileExists(SetupIniFileName) then
    raise Exception.CreateFmt('Setup INI file not found:'#13#10'%s',
                              [SetupIniFileName]);

  CaddyRoot := ClientCaddyRoot();
  if CaddyRoot = '' then
    raise Exception.Create('Could not resolve the Caddy folder from the Caddy configuration path.');

  Ini := TIniFile.Create(SetupIniFileName);
  
  try
    IsRdj := Ini.SectionExists('Icecast') or
             Ini.SectionExists('SetupBroadcast');
    IsRdjPro := Ini.SectionExists('Caddy');

    if IsRdj then
      begin
        Ini.WriteString('Icecast',
                        'CaddyDir',
                        CaddyRoot);
        Ini.WriteString('Icecast',
                        'CaddyConfigFile',
                        FConfigFileName);
        Ini.WriteString('Icecast',
                        'NowPlayingJsonFile',
                        IncludeTrailingPathDelimiter(CaddyRoot) + 'nowplaying.json');
        Ini.WriteString('Icecast',
                        'Covers',
                        CaddyRoot);
        Ini.WriteString('Icecast',
                        'CaddyCommand',
                        BuildCaddyCommand());
        Ini.WriteString('Icecast',
                        'CaddyLogFile',
                        BuildCaddyLogFileValue());
      end
    else if IsRdjPro then
      begin
        Ini.WriteString('Caddy',
                        'CaddyDir',
                        CaddyRoot);
        Ini.WriteString('Caddy',
                        'ConfigFile',
                        FConfigFileName);
        Ini.WriteString('Caddy',
                        'NowPlayingJsonFile',
                        IncludeTrailingPathDelimiter(CaddyRoot) + 'nowplaying.json');
        Ini.WriteString('Caddy',
                        'Artwork',
                        IncludeTrailingPathDelimiter(CaddyRoot) + 'artwork');
        Ini.WriteString('Caddy',
                        'Video',
                        IncludeTrailingPathDelimiter(CaddyRoot) + 'stream');
        Ini.WriteString('Caddy',
                        'Command',
                        BuildCaddyCommand());
        Ini.WriteString('Caddy',
                        'LogFile',
                        BuildCaddyLogFileValue());
      end
    else
      raise Exception.Create('The setup INI is not recognized as RDJ or RDJ Pro.');
  finally
    
    Ini.Free;
  end;
end;


procedure TfrmCaddyConfigEditor.SaveConfig;
begin

  if Trim(edtSiteAddress.Text) = '' then
    raise Exception.Create('Site address is empty.');

  if Trim(edtCaddyRoot.Text) = '' then
    raise Exception.Create('Caddy root folder is empty.');

  if (FindFirstValue('output file ') <> '') and
     (Trim(edtLogFile.Text) = '') then
    raise Exception.Create('Log file is empty.');

  if Trim(edtProxyHost.Text) = '' then
    raise Exception.Create('Proxy host is empty.');

  if Trim(edtProxyPort.Text) = '' then
    raise Exception.Create('Proxy port is empty.');

  if (Trim(FSetupIniFileName) <> '') and
     (not FileExists(Trim(FSetupIniFileName))) then
    raise Exception.CreateFmt('Setup INI file not found:'#13#10'%s',
                              [Trim(FSetupIniFileName)]);

  ApplyConfig();
  FConfigLines.SaveToFile(FConfigFileName);
  UpdateSetupIni();
end;


procedure TfrmCaddyConfigEditor.btnSaveClick(Sender: TObject);
begin

  try

    SaveConfig();
    ModalResult := mrOK;
  except

    on E: Exception do
      MessageDlg(E.Message,
                 mtError,
                 [mbOK],
                 0);
  end;
end;

end.
