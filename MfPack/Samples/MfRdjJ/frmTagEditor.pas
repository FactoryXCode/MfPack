// FactoryX
//
// Copyright: FactoryX. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: frmTagEditor.pas
// Kind: Pascal Unit
// Release date: 24-01-2026
// Language: ENU
//
// Revision Version: 3.1.9
// Description: Mp3 (for now) tag editor form.
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
unit frmTagEditor;

interface

uses

  {WinApi}
  WinApi.Windows,
  WinApi.Messages,
  {System}
  System.SysUtils,
  System.Variants,
  System.Classes,
  {Vcl}
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ExtCtrls,
  Vcl.StdCtrls,
  { Application }
  RDJ_Common,
  RDJ.PlaylistTypes,
  RDJ.TagWriter,
  MPxpButton;


type

  TfrmTagEditor = class(TForm)
    pnlBottom: TPanel;
    btnOk: TMPxpButton;
    btnCancel: TMPxpButton;
    lblPath: TLabel;
    edtPath: TEdit;
    lblArtist: TLabel;
    lblTitle: TLabel;
    lblAlbum: TLabel;
    lblAlbumArtist: TLabel;
    lblGenre: TLabel;
    lblComposer: TLabel;
    lblComment: TLabel;
    lblYear: TLabel;
    lblBPM: TLabel;
    lblTrackNo: TLabel;
    lblDiscNo: TLabel;
    lblKey: TLabel;
    lblGainDb: TLabel;
    edtArtist: TEdit;
    edtTitle: TEdit;
    edtAlbum: TEdit;
    edtAlbumArtist: TEdit;
    edtGenre: TEdit;
    edtComposer: TEdit;
    edtYear: TEdit;
    edtTrackNo: TEdit;
    edtDiscNo: TEdit;
    edtBPM: TEdit;
    edtKey: TEdit;
    edtGainDb: TEdit;
    memComment: TMemo;
    procedure btnCancelClick(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure FormShow(Sender: TObject);

  private

    procedure LoadTrackToGui(const ATrack: TRDJTrack);
    procedure SaveGuiToTrack(var ATrack: TRDJTrack);

  public

    class function Execute(var ATrack: TRDJTrack): Boolean;
  end;

//var
//  frmTagEditor: TfrmTagEditor;

implementation

{$R *.dfm}


procedure TfrmTagEditor.btnOkClick(Sender: TObject);
begin

  ModalResult := mrOk;
end;


class function TfrmTagEditor.Execute(var ATrack: TRDJTrack): Boolean;
var
  Frm: TfrmTagEditor;
  ErrorText: string;

begin

  Frm := TfrmTagEditor.Create(nil);
  try

    Frm.LoadTrackToGui(ATrack);

    Result := (Frm.ShowModal = mrOk);
    if Result then
      begin

        Frm.SaveGuiToTrack(ATrack);

        if Trim(ATrack.FullPath) <> '' then
          begin
            if not RDJWriteTrackTags(ATrack.FullPath,
                                     ATrack,
                                     ErrorText) then
              begin
                Application.MessageBox(PChar('Could not write tags.' + sLineBreak + sLineBreak + ErrorText),
                                       'Tag Editor',
                                       MB_ICONERROR or MB_OK);
                Result := False;
              end;
          end;
      end;
  finally
    Frm.Free;
  end;
end;


procedure TfrmTagEditor.FormShow(Sender: TObject);
begin

  // DWM
  //ApplyDarkWindowFrame(Handle);
end;


procedure TfrmTagEditor.LoadTrackToGui(const ATrack: TRDJTrack);
begin

  edtPath.Text := ATrack.FullPath;
  edtArtist.Text := ATrack.Artist;
  edtTitle.Text := ATrack.Title;
  edtAlbum.Text := ATrack.Album;
  edtAlbumArtist.Text := ATrack.AlbumArtist;
  edtGenre.Text := ATrack.Genre;
  edtComposer.Text := ATrack.Composer;
  memComment.Lines.Text := ATrack.Comment;

  if (ATrack.Year <> 0) then
    edtYear.Text := ATrack.Year.ToString
  else
    edtYear.Text := '';

  if (ATrack.TrackNumber <> 0) then
    edtTrackNo.Text := ATrack.TrackNumber.ToString
  else
    edtTrackNo.Text := '';

  if (ATrack.DiscNumber <> 0) then
    edtDiscNo.Text := ATrack.DiscNumber.ToString
  else
    edtDiscNo.Text := '';

  if (ATrack.BPM <> 0) then
    edtBPM.Text := FloatToStr(ATrack.BPM)
  else
    edtBPM.Text := '';

  edtKey.Text := ATrack.MusicalKey;

  if (ATrack.GainDb <> 0) then
    edtGainDb.Text := FloatToStr(ATrack.GainDb)
  else
    edtGainDb.Text := '';
end;


procedure TfrmTagEditor.btnCancelClick(Sender: TObject);
begin

  ModalResult := mrCancel;
end;


procedure TfrmTagEditor.SaveGuiToTrack(var ATrack: TRDJTrack);
begin

  ATrack.Artist := Trim(edtArtist.Text);
  ATrack.Title := Trim(edtTitle.Text);
  ATrack.Album := Trim(edtAlbum.Text);
  ATrack.AlbumArtist := Trim(edtAlbumArtist.Text);
  ATrack.Genre := Trim(edtGenre.Text);
  ATrack.Composer := Trim(edtComposer.Text);
  ATrack.Comment := Trim(memComment.Text);

  ATrack.Year := StrToIntDef(Trim(edtYear.Text),
                             0);
  ATrack.TrackNumber := StrToIntDef(Trim(edtTrackNo.Text),
                                    0);
  ATrack.DiscNumber := StrToIntDef(Trim(edtDiscNo.Text),
                                   0);

  ATrack.BPM := StrToFloatDef(StringReplace(Trim(edtBPM.Text),
                                            ',',
                                            '.',
                                            [rfReplaceAll]),
                                            0.0);
  ATrack.MusicalKey := Trim(edtKey.Text);

  ATrack.GainDb := StrToFloatDef(StringReplace(Trim(edtGainDb.Text),
                                               ',',
                                               '.',
                                               [rfReplaceAll]),
                                               0.0);
end;

end.
