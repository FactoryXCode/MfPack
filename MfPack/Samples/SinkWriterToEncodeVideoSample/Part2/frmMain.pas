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
    CheckBox1: TCheckBox;
    Label2: TLabel;
    lbxVideoDimensions: TListBox;
    edVideoLength: TEdit;
    Label3: TLabel;
    odlgBitmapFile: TOpenDialog;
    Label4: TLabel;
    butChooseBmpFile: TButton;
    procedure butExecuteClick(Sender: TObject);
    procedure cbxOutputFormatCloseUp(Sender: TObject);
    procedure cbxEncodingFormatCloseUp(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure lbxVideoDimensionsClick(Sender: TObject);
    procedure edVideoLengthChange(Sender: TObject);
    procedure butChooseBmpFileClick(Sender: TObject);

  private
    { Private declarations }
    tgEncodingFormat: TGuid;
    sEncodingFormat: string;
    sBitmapFile: string;
    sExtension: string;
    dwSelectedVideoHeight: DWord;
    dwSelectedVideoWidth: DWord;
    iVideoLength: UINT32;

  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;


implementation

{$R *.dfm}


procedure TMainForm.butChooseBmpFileClick(Sender: TObject);
begin
  odlgBitmapFile := TOpenDialog.Create(Self);
  odlgBitmapFile.InitialDir := GetCurrentDir();
  if odlgBitmapFile.Execute then
    begin
      sBitmapFile := odlgBitmapFile.FileName;
    end;
end;


procedure TMainForm.butExecuteClick(Sender: TObject);
begin
  if not FileExists(sBitmapFile) then
    begin
      ShowMessage('You did not select a bitmap file');
      Exit;
    end;

  FSampleSinkWriter := TSampleSinkWriter.Create();

  cbxOutputFormatCloseUp(Self);
  cbxEncodingFormatCloseUp(Self);
  edVideoLengthChange(Self);
  FSampleSinkWriter.SaveResizedBitmap := CheckBox1.Checked;
  // We could set the params to it's class properties, but that's up to you.
  if SUCCEEDED(FSampleSinkWriter.RunSinkWriter(sExtension,
                                               sEncodingFormat,
                                               tgEncodingFormat,
                                               sBitmapFile,
                                               iVideoLength,
                                               dwSelectedVideoWidth,
                                               dwSelectedVideoHeight)) then
    begin
      lblInfo.Font.Color := clGreen;
      lblInfo.Caption := 'File is succesfully created!';
    end
  else
    begin
      lblInfo.Font.Color := clRed;
      lblInfo.Caption := 'Oops, something went wrong :-(';
    end;
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


procedure TMainForm.edVideoLengthChange(Sender: TObject);
begin
  iVideoLength := StrToInt(edVideoLength.Text)
end;


procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := False;
  SafeDelete(FSampleSinkWriter);
  CanClose := True;
end;


procedure TMainForm.FormCreate(Sender: TObject);
begin

  cbxOutputFormat.ItemIndex := 0;
  cbxOutputFormatCloseUp(Self);
  cbxEncodingFormatCloseUp(Self);
  lbxVideoDimensions.ItemIndex := 0;
  lbxVideoDimensionsClick(Self);

end;


procedure TMainForm.lbxVideoDimensionsClick(Sender: TObject);
begin

  case lbxVideoDimensions.ItemIndex of
    0:  begin
          dwSelectedVideoHeight := 480;
          dwSelectedVideoWidth  := 640;
        end;
    1:  begin
          dwSelectedVideoHeight := 720;
          dwSelectedVideoWidth  := 1280;
        end;
    2:  begin
          dwSelectedVideoHeight := 1080;
          dwSelectedVideoWidth  := 1920;
        end;
  end;

end;

end.
