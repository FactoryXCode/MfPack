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
// Revision Version: 3.1.3
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
// Related projects: MfPackX313
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
//  The contents of this file are subject to the
//  GNU General Public License v3.0 (the "License");
//  you may not use this file except in
//  compliance with the License. You may obtain a copy of the License at
//  https://www.gnu.org/licenses/gpl-3.0.html
//
// Software distributed under the License is distributed on an "AS IS"
// basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
// License for the specific language governing rights and limitations
// under the License.
//
// Explanatory memorandum:
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
  {Application}
  SinkWriterClass;


type
  TMainForm = class(TForm)
    butExecute: TButton;
    lblInfo: TLabel;
    Label1: TLabel;
    cbxOutputFormat: TComboBox;
    procedure butExecuteClick(Sender: TObject);
    procedure cbxOutputFormatCloseUp(Sender: TObject);
  private
    { Private declarations }
    tgEncodingFormat: TGuid;
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

  if SUCCEEDED(FSampleSinkWriter.RunSinkWriter(sExtension,
                                               tgEncodingFormat)) then
    lblInfo.Caption := 'File is succesfully created!'
  else
    lblInfo.Caption := 'Oops, something went wrong :-('
end;

// Choose the output format
// Note that this is a small list. To create more valid formats see:
// See: https://github.com/MicrosoftDocs/win32/blob/docs/desktop-src/medfound/h-264-video-encoder.md
procedure TMainForm.cbxOutputFormatCloseUp(Sender: TObject);
begin

  if (cbxOutputFormat.ItemIndex > -1) then
    begin
      sExtension := LowerCase(cbxOutputFormat.Items[cbxOutputFormat.ItemIndex]);
      if sExtension = 'mp4' then
        tgEncodingFormat := MFVideoFormat_H264
      else if sExtension = 'wmf' then
        tgEncodingFormat := MFVideoFormat_WMV3
      else if sExtension = 'avi' then
        tgEncodingFormat := MFVideoFormat_IYUV // Supported formats:
                                               // MFVideoFormat_I420
                                               // MFVideoFormat_IYUV
                                               // MFVideoFormat_NV12
                                               // MFVideoFormat_YUY2
                                               // MFVideoFormat_YV12
      else // default
        begin
          sExtension := 'mp4';
          tgEncodingFormat := MFVideoFormat_H264;
        end;
    end;
end;

end.
