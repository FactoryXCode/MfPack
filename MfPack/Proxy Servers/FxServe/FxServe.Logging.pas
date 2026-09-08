// FactoryX
//
// Copyright (c) FactoryX, Netherlands/Australia/Germany. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: FxServe.Logging.pas
// Kind: Pascal Unit
// Release date: 10-08-2026
// Language: ENU
//
// Revision Version: 4.0.0
// Description: Logger unit.
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
unit FxServe.Logging;

interface

uses
  {WinApi}
  WinApi.Windows,
  {System}
  System.SysUtils,
  System.Classes,
  System.SyncObjs;

type

  TFxServeLogger = class
  private
    FFileName: string;
    FLock: TCriticalSection;

  public

    constructor Create(const AFileName: string);
    destructor Destroy(); override;

    procedure Write(const ALevel: string;
                    const AMessage: string);

    procedure Info(const AMessage: string);
    procedure Error(const AMessage: string);
  end;


implementation


constructor TFxServeLogger.Create(const AFileName: string);
begin

  inherited Create;

  FFileName := AFileName;
  FLock := TCriticalSection.Create();
end;


destructor TFxServeLogger.Destroy();
begin

  FLock.Free;

  inherited Destroy();
end;


procedure TFxServeLogger.Write(const ALevel: string;
                               const AMessage: string);
var
  Line: UTF8String;
  ConsoleLine: string;
  Stream: TFileStream;
  Dir: string;

begin

  Line := UTF8String(FormatDateTime('yyyy-mm-dd"T"hh:nn:ss.zzz',
                                    Now) + ' [' + ALevel + '] ' + AMessage + sLineBreak);

  FLock.Acquire;

  try
    ConsoleLine := string(Line);

    while (ConsoleLine <> '') and
          CharInSet(ConsoleLine[Length(ConsoleLine)], [#10, #13]) do
      Delete(ConsoleLine,
             Length(ConsoleLine),
             1);

    if (GetConsoleOutputCP <> 0) then
      begin

        try
          Writeln(ConsoleLine);
        except
          { Logging must never stop the HTTP server. }
        end;
      end;

    if (FFileName = '') then
      Exit;

    try
      Dir := ExtractFileDir(FFileName);

      if (Dir <> '') and not DirectoryExists(Dir) then
        ForceDirectories(Dir);

      if FileExists(FFileName) then
        Stream := TFileStream.Create(FFileName,
                                     fmOpenReadWrite or fmShareDenyNone)
      else
        Stream := TFileStream.Create(FFileName,
                                     fmCreate or fmShareDenyNone);
      try
        Stream.Seek(0,
                    soEnd);

        if (Length(Line) > 0) then
          Stream.WriteBuffer(Line[1],
                             Length(Line));

      finally
        Stream.Free;
      end;

    except
      on E: Exception do
        OutputDebugString(PChar('FxServe logging error: ' + E.Message));
    end;

  finally
    FLock.Release;
  end;
end;


procedure TFxServeLogger.Info(const AMessage: string);
begin

  Write('INFO',
        AMessage);
end;


procedure TFxServeLogger.Error(const AMessage: string);
begin

  Write('ERROR',
        AMessage);
end;

end.
