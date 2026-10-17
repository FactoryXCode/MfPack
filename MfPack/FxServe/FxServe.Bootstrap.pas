// FactoryX
//
// Copyright (c) FactoryX, Netherlands/Australia/Germany. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: FxServe.Bootstrap.pas
// Kind: Pascal Unit
// Release date: 10-08-2026
// Language: ENU
//
// Revision Version: 4.0.0
// Description: Creates a first-run FxServe layout from embedded resources.
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
unit FxServe.Bootstrap;

interface

  procedure EnsureFxServeBootstrap(const AConfigFileName: string;
                                   const ARefreshWebFiles: Boolean = False);

implementation

uses

  {WinApi}
  WinApi.Windows,
  {System}
  System.SysUtils,
  System.Classes,
  System.IniFiles,
  System.IOUtils,
  System.Zip;

const
  BOOTSTRAP_RESOURCE_NAME = 'FXSERVE_BOOTSTRAP';


procedure WriteBootstrapFile(const AZip: TZipFile;
                             const AIndex: Integer;
                             const ATargetFileName: string;
                             const AOverwrite: Boolean);
var
  Data: TBytes;
  Folder: string;
  TemporaryFileName: string;
  Stream: TFileStream;

begin

  if FileExists(ATargetFileName) and not AOverwrite then
    Exit;

  Folder := ExtractFileDir(ATargetFileName);
  if (Folder <> '') and not DirectoryExists(Folder) then
    if not ForceDirectories(Folder) then
      raise Exception.CreateFmt('Cannot create bootstrap folder: %s',
                                [Folder]);

  AZip.Read(AIndex,
            Data);

  TemporaryFileName := ATargetFileName + '.bootstrap.tmp';
  if FileExists(TemporaryFileName) then
    DeleteFile(TemporaryFileName);

  Stream := TFileStream.Create(TemporaryFileName,
                               fmCreate);

  try
    if (Length(Data) > 0) then
      Stream.WriteBuffer(Data[0],
                         Length(Data));
  finally
    Stream.Free();
  end;

  if not MoveFileEx(PChar(TemporaryFileName),
                    PChar(ATargetFileName),
                    MOVEFILE_REPLACE_EXISTING or MOVEFILE_WRITE_THROUGH) then
    begin
      DeleteFile(TemporaryFileName);
      raise Exception.CreateFmt('Cannot create bootstrap file: %s',
                                [ATargetFileName]);
    end;
end;


function ConfiguredWebRoot(const AConfigFileName: string): string;
var
  Ini: TMemIniFile;
  Value: string;

begin

  Ini := TMemIniFile.Create(AConfigFileName);

  try
    Value := Trim(Ini.ReadString('Server',
                                 'WebRoot',
                                 '.\www'));
  finally
    Ini.Free();
  end;

  if TPath.IsPathRooted(Value) then
    Result := ExpandFileName(Value)
  else
    Result := ExpandFileName(IncludeTrailingPathDelimiter(
      ExtractFileDir(AConfigFileName)) + Value);
end;


function SafeArchiveName(const AName: string): string;
begin


  Result := StringReplace(AName,
                          '\',
                          '/',
                          [rfReplaceAll]);
  if (Result = '') or
     (Result[Length(Result)] = '/') or
     (Pos('../', Result) > 0) or
     (Pos(':', Result) > 0) or
     (Result[1] = '/') then
    Result := '';
end;


procedure EnsureFxServeBootstrap(const AConfigFileName: string;
                                 const ARefreshWebFiles: Boolean);
var
  Resource: TResourceStream;
  Archive: TZipFile;
  ConfigFileName: string;
  ConfigIndex: Integer;
  WebRoot: string;
  ArchiveName: string;
  RelativeName: string;
  TargetFileName: string;
  I: Integer;

begin

  ConfigFileName := ExpandFileName(AConfigFileName);

  Resource := TResourceStream.Create(HInstance,
                                     BOOTSTRAP_RESOURCE_NAME,
                                     RT_RCDATA);
  Archive := TZipFile.Create();

  try
    Archive.Open(Resource,
                 zmRead);

    ConfigIndex := Archive.IndexOf('FxServe.ini');

    if (ConfigIndex < 0) then
      raise Exception.Create('Embedded bootstrap configuration is missing.');

    WriteBootstrapFile(Archive,
                       ConfigIndex,
                       ConfigFileName,
                       False);

    WebRoot := ConfiguredWebRoot(ConfigFileName);

    for I := 0 to Archive.FileCount - 1 do
      begin
        ArchiveName := SafeArchiveName(Archive.FileName[I]);

        if (ArchiveName = '') or
           not SameText(Copy(ArchiveName,
                             1,
                             4), 'www/') then
          Continue;

        RelativeName := Copy(ArchiveName,
                             5,
                             MaxInt);

        if (RelativeName = '') then
          Continue;

        TargetFileName := ExpandFileName(IncludeTrailingPathDelimiter(WebRoot) +
                          StringReplace(RelativeName,
                                        '/',
                                        '\',
                                        [rfReplaceAll]));

        if (Pos(IncludeTrailingPathDelimiter(WebRoot),
               TargetFileName) <> 1) then
          raise Exception.CreateFmt('Unsafe embedded bootstrap path: %s',
                                    [ArchiveName]);

        WriteBootstrapFile(Archive,
                           I,
                           TargetFileName,
                           ARefreshWebFiles and
                           not SameText(RelativeName,
                                        'fxserve-config.json'));
      end;

  finally
    Archive.Free();
    Resource.Free();
  end;
end;

end.
