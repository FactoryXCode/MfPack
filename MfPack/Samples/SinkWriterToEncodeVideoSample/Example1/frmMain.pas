// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - MediaFoundation
// Project location: https://sourceforge.net/projects/MfPack
//                   https://github.com/FactoryXCode/MfPack
// Module: SinkWriterClass.pas
// Kind: Pascal / Delphi unit
// Release date: 25-11-2022
// Language: ENU
//
// Revision Version: 3.1.4
// Description: UI for example of how to use the SinkWriter.
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 28/08/2022 All                 PiL release  SDK 10.0.22621.0 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 or later.
//
// Related objects: -
// Related projects: MfPackX314
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.22621.0
//
// Todo: -
//
//==============================================================================
// Source: -
//
// Copyright (c) FactoryX. All rights reserved.
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
unit frmMain;

interface

uses
  {WinApi}
  Winapi.Windows,
  Winapi.Messages,
  {System}
  System.SysUtils,
  System.Variants,
  System.Classes,
  {Vcl}
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfApi,
  WinApi.MediaFoundationApi.MfUtils,
  {Application}
  SinkWriterClass;


type
  TMainForm = class(TForm)
    butExecute: TButton;
    lblInfo: TLabel;
    Label1: TLabel;
    cbxOutputFormat: TComboBox;
    cbxEncodingFormat: TComboBox;
    procedure butExecuteClick(Sender: TObject);
    procedure cbxOutputFormatCloseUp(Sender: TObject);
    procedure cbxEncodingFormatCloseUp(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    { Private declarations }
    tgEncodingFormat: TGuid;
    sEncodingFormat: string;
    sExtension: string;

  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;


implementation

{$R *.dfm}


procedure TMainForm.butExecuteClick(Sender: TObject);
begin
  FSampleSinkWriter := TSampleSinkWriter.Create();

  cbxOutputFormatCloseUp(Self);
  cbxEncodingFormatCloseUp(Self);

  if SUCCEEDED(FSampleSinkWriter.RunSinkWriter(sExtension,
                                               sEncodingFormat,
                                               tgEncodingFormat)) then
    lblInfo.Caption := 'File is succesfully created!'
  else
    lblInfo.Caption := 'Oops, something went wrong :-('
end;

// Choose the output format
// Note that this is a small list. To create more valid formats see:
// See: https://github.com/MicrosoftDocs/win32/blob/docs/desktop-src/medfound/h-264-video-encoder.md
procedure TMainForm.cbxEncodingFormatCloseUp(Sender: TObject);
begin
  if (cbxEncodingFormat.Items.Count > 0) and (cbxEncodingFormat.ItemIndex > -1) then
    begin
      sEncodingFormat := cbxEncodingFormat.Items[cbxEncodingFormat.ItemIndex];
      if sEncodingFormat = 'MFVideoFormat_H264' then
        tgEncodingFormat := MFVideoFormat_H264
      else if sEncodingFormat = 'MFVideoFormat_WMV3' then
        tgEncodingFormat := MFVideoFormat_WMV3
      else if sEncodingFormat = 'MFVideoFormat_I420' then
        tgEncodingFormat := MFVideoFormat_I420
      else if sEncodingFormat = 'MFVideoFormat_IYUV' then
        tgEncodingFormat := MFVideoFormat_IYUV
      else if sEncodingFormat = 'MFVideoFormat_NV12' then
        tgEncodingFormat := MFVideoFormat_NV12
      else if sEncodingFormat = 'MFVideoFormat_YUY2' then
        tgEncodingFormat := MFVideoFormat_YUY2
      else if sEncodingFormat = 'MFVideoFormat_YV12' then
        tgEncodingFormat := MFVideoFormat_YV12
      else // default
        begin
          sExtension := 'mp4';
          tgEncodingFormat := MFVideoFormat_H264;
          sEncodingFormat  := 'MFVideoFormat_H264';
        end;
      lblInfo.Caption := 'Click ''Execute'' to run the sample..'
    end;
end;


procedure TMainForm.cbxOutputFormatCloseUp(Sender: TObject);
begin

  if (cbxOutputFormat.ItemIndex > -1) then
    begin
      sExtension := LowerCase(cbxOutputFormat.Items[cbxOutputFormat.ItemIndex]);
      cbxEncodingFormat.Clear;
      // populate the cbxEncodingFormat with supported formats when ouputformat is selected.
      if sExtension = 'mp4' then
        begin
          cbxEncodingFormat.Items.Append('MFVideoFormat_H264');
          cbxEncodingFormat.ItemIndex := 0;
        end
      else if sExtension = 'wmv' then
        begin

          cbxEncodingFormat.Items.Append('MFVideoFormat_WMV3');
          cbxEncodingFormat.ItemIndex := 0;
        end
      else if sExtension = 'avi' then
        begin
          cbxEncodingFormat.Items.Append('MFVideoFormat_I420');
          cbxEncodingFormat.Items.Append('MFVideoFormat_IYUV');
          cbxEncodingFormat.Items.Append('MFVideoFormat_NV12');
          cbxEncodingFormat.Items.Append('MFVideoFormat_YUY2');
          cbxEncodingFormat.Items.Append('MFVideoFormat_YV12');
        end
      else // default
        begin
          sExtension := 'mp4';
          tgEncodingFormat := MFVideoFormat_H264;
          sEncodingFormat  := 'MFVideoFormat_H264';
        end;
      cbxEncodingFormat.ItemIndex := 0;
      lblInfo.Caption := 'Click ''Execute'' to run the sample..'
    end;
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := False;
  FreeAndNil(FSampleSinkWriter);
  CanClose := True;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin

  cbxOutputFormat.ItemIndex := 0;
  cbxOutputFormatCloseUp(Self);
  cbxEncodingFormatCloseUp(Self);
end;

end.
