// FactoryX
//
// Copyright: FactoryX. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: RDJ.LibraryScanThread.pas
// Kind: Pascal Unit
// Release date: 24-01-2026
// Language: ENU
//
// Revision Version: 3.1.9
// Description: Filescanner for the playlist editor.
//
// Company: FactoryX
// Intiator(s): Tony (maXcomX), Carmen (carmenh).
// Contributor(s): Tony Kalf (maXcomX), Carmen (carmenh).
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 05/05/2026 All                 Bauhaus release  SDK 10.0.26100.4654 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 or higher.
//          Please, read documentation carefully!
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
// =============================================================================
// Source: FactoryX.Code.
// =============================================================================
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
unit RDJ.LibraryScanThread;

interface

uses

  {System}
  System.SysUtils,
  System.Classes,
  {Application}
  RDJ.PlaylistDb,
  RDJ.TrackLibrary,
  RDJ.TrackQuality,
  RDJ.LibraryScanner,
  RDJ.PlaylistTypes,
  RDJ.TagReader;

type

  TRDJLibraryScanThread = class;

  TRDJLibraryScanProgressEvent = procedure(Sender: TObject;
                                           const AFileName: string;
                                           ACurrentIndex: Integer;
                                           ATotalCount: Integer) of object;

  TRDJLibraryScanFinishedEvent = procedure(Sender: TObject;
                                           AProcessedCount: Integer;
                                           ACancelled: Boolean;
                                           const AErrorMsg: string) of object;

  TRDJLibraryScanThread = class(TThread)
  private

    FDbFileName: string;
    FFolder: string;
    FRecursive: Boolean;

    FProcessedCount: Integer;
    FCancelled: Boolean;
    FErrorMsg: string;

    FProgressFileName: string;
    FProgressIndex: Integer;
    FProgressTotal: Integer;

    FOnProgress: TRDJLibraryScanProgressEvent;
    FOnFinished: TRDJLibraryScanFinishedEvent;

    //procedure ScannerProgress(Sender: TObject;
    //                          const AFileName: string;
    //                          ACurrentIndex: Integer;
    //                          ATotalCount: Integer);

    function ScannerCancelCheck(Sender: TObject): Boolean;

    procedure DoProgress();
    procedure DoFinished();

  protected

    procedure Execute(); override;

  public

    constructor Create(const ADbFileName: string;
                       const AFolder: string;
                       const ARecursive: Boolean);

    property OnProgress: TRDJLibraryScanProgressEvent read FOnProgress write FOnProgress;
    property OnFinished: TRDJLibraryScanFinishedEvent read FOnFinished write FOnFinished;
  end;


implementation


constructor TRDJLibraryScanThread.Create(const ADbFileName: string;
                                         const AFolder: string;
                                         const ARecursive: Boolean);
begin

  inherited Create(True);

  FreeOnTerminate := True;

  FDbFileName := ADbFileName;
  FFolder := AFolder;
  FRecursive := ARecursive;

  FProcessedCount := 0;
  FCancelled := False;
  FErrorMsg := '';

  FProgressFileName := '';
  FProgressIndex := 0;
  FProgressTotal := 0;
end;


function TRDJLibraryScanThread.ScannerCancelCheck(Sender: TObject): Boolean;
begin

  Result := Terminated;
end;

{
procedure TRDJLibraryScanThread.ScannerProgress(Sender: TObject;
                                                const AFileName: string;
                                                ACurrentIndex: Integer;
                                                ATotalCount: Integer);
begin
  FProgressFileName := AFileName;
  FProgressIndex := ACurrentIndex;
  FProgressTotal := ATotalCount;

  TThread.Queue(nil,
                procedure
                begin
                  DoProgress;
                end);
end;
}

procedure TRDJLibraryScanThread.DoProgress;
begin

  if Assigned(FOnProgress) then
    FOnProgress(Self,
                FProgressFileName,
                FProgressIndex,
                FProgressTotal);
end;


procedure TRDJLibraryScanThread.DoFinished;
begin

  if Assigned(FOnFinished) then
    FOnFinished(Self,
                FProcessedCount,
                FCancelled,
                FErrorMsg);
end;


procedure TRDJLibraryScanThread.Execute;
const
  COMMIT_BATCH_SIZE = 250;

var
  Db: TRDJPlaylistDb;
  LibraryRef: TRDJTrackLibrary;
  Scanner: TRDJLibraryScanner;
  Files: TStringList;
  Track: TRDJTrack;
  i: Integer;
  BatchCount: Integer;

begin

  inherited;

  Db := nil;
  LibraryRef := nil;
  Scanner := nil;
  Files := nil;

  try
    Db := TRDJPlaylistDb.Create;
    Db.Open(FDbFileName);

    LibraryRef := TRDJTrackLibrary.Create(Db);

    Scanner := TRDJLibraryScanner.Create(LibraryRef);
    Scanner.OnCancelCheck := ScannerCancelCheck;

    Files := Scanner.CollectAudioFiles(FFolder,
                                       FRecursive);

    if Files = nil then
      begin
        FCancelled := True;
        Exit;
      end;

    FProcessedCount := 0;
    BatchCount := 0;

    Db.BeginTx;
    try
      for i := 0 to Files.Count - 1 do
        begin
          if Terminated then
            begin
              FCancelled := True;
              Break;
            end;

          FProgressFileName := Files[i];
          FProgressIndex := i + 1;
          FProgressTotal := Files.Count;

          TThread.Queue(nil,
            procedure
            begin
              DoProgress;
            end);

          if RDJReadTrackInfo(Files[i],
                              Track) then
            begin
              RDJUpdateTrackQuality(Track);
              LibraryRef.AddOrUpdateTrack(Track);
              Inc(FProcessedCount);
              Inc(BatchCount);
            end;

          if BatchCount >= COMMIT_BATCH_SIZE then
            begin
              Db.CommitTx;
              Db.BeginTx;
              BatchCount := 0;
            end;
        end;

      // commit whatever remains in the current batch
      Db.CommitTx;
    except
      Db.RollbackTx;
      raise;
    end;

    if not FCancelled then
      FCancelled := Terminated;

  except
    on E: Exception do
      begin

        FErrorMsg := E.Message;
        FCancelled := Terminated;
      end;
  end;

  FreeAndNil(Files);
  FreeAndNil(Scanner);
  FreeAndNil(LibraryRef);
  FreeAndNil(Db);

  TThread.Synchronize(nil,
                      procedure
                      begin

                        DoFinished;
                      end);
end;

end.
