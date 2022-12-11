// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - MediaFoundation
// Project location: https://sourceforge.net/projects/MfPack
//                   https://github.com/FactoryXCode/MfPack
// Module: dlgVideoOutput.pas
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
unit dlgVideoOutput;

interface

uses
  Winapi.Windows,
  System.SysUtils,
  System.Classes,
  Vcl.Graphics,
  Vcl.Forms,
  Vcl.Controls,
  Vcl.StdCtrls,
  Vcl.Buttons,
  Vcl.ExtCtrls,
  frmMain,
  WinApi.MediaFoundationApi.MfApi;

type
  TdlgVideoSetttings = class(TForm)
    OKBtn: TButton;
    CancelBtn: TButton;
    Bevel1: TBevel;
    cbxOutputFormat: TComboBox;
    cbxEncodingFormat: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    cbxDimensions: TComboBox;
    edVideoLength: TEdit;
    Label4: TLabel;
    cbSaveResizedBitmap: TCheckBox;
    procedure cbxOutputFormatCloseUp(Sender: TObject);
    procedure cbxEncodingFormatCloseUp(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure cbxDimensionsCloseUp(Sender: TObject);
    procedure cbSaveResizedBitmapClick(Sender: TObject);
    procedure edVideoLengthChange(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  dlgVideoSetttings: TdlgVideoSetttings;

implementation

{$R *.dfm}



procedure TdlgVideoSetttings.cbxOutputFormatCloseUp(Sender: TObject);
begin

  if (cbxOutputFormat.ItemIndex > -1) then
    begin
      MainForm.sExtension := LowerCase(cbxOutputFormat.Items[cbxOutputFormat.ItemIndex]);
      cbxEncodingFormat.Clear;
      // populate the cbxEncodingFormat with supported formats when ouputformat is selected.
      if MainForm.sExtension = 'mp4' then
        begin
          cbxEncodingFormat.Items.Append('MFVideoFormat_H264');
          cbxEncodingFormat.ItemIndex := 0;
        end
      else if MainForm.sExtension = 'wmv' then
        begin
          cbxEncodingFormat.Items.Append('MFVideoFormat_WMV3');
          cbxEncodingFormat.ItemIndex := 0;
        end
      else if MainForm.sExtension = 'avi' then
        begin
          cbxEncodingFormat.Items.Append('MFVideoFormat_I420');
          cbxEncodingFormat.Items.Append('MFVideoFormat_IYUV');
          cbxEncodingFormat.Items.Append('MFVideoFormat_NV12');
          cbxEncodingFormat.Items.Append('MFVideoFormat_YUY2');
          cbxEncodingFormat.Items.Append('MFVideoFormat_YV12');
        end
      else // default
        begin
          MainForm.sExtension := 'mp4';
          MainForm.tgEncodingFormat := MFVideoFormat_H264;
          MainForm.sEncodingFormat  := 'MFVideoFormat_H264';
        end;
      cbxEncodingFormat.ItemIndex := 0;

    end;
end;


procedure TdlgVideoSetttings.edVideoLengthChange(Sender: TObject);
begin
  MainForm.iVideoLength := StrToInt(edVideoLength.Text)
end;


// Optionally you can save the resized bitmap to a file.
procedure TdlgVideoSetttings.cbSaveResizedBitmapClick(Sender: TObject);
begin
  MainForm.bSaveResizedBitmap := cbSaveResizedBitmap.Checked;
end;


procedure TdlgVideoSetttings.FormCreate(Sender: TObject);
begin
  // Set initial states and values.
  cbxOutputFormat.ItemIndex := 0;
  cbxOutputFormatCloseUp(Self);
  cbxEncodingFormatCloseUp(Self);
  cbxDimensions.ItemIndex := 0;
  cbxDimensionsCloseUp(Self);
  edVideoLength.Text := '20';
  edVideoLength.OnChange(Self);
  cbSaveResizedBitmap.Checked := False;
  cbSaveResizedBitmap.OnClick(Self);
end;


procedure TdlgVideoSetttings.cbxDimensionsCloseUp(Sender: TObject);
begin
  case cbxDimensions.ItemIndex of
    0:  begin
          MainForm.dwSelectedVideoHeight := 480;
          MainForm.dwSelectedVideoWidth  := 640;
        end;
    1:  begin
          MainForm.dwSelectedVideoHeight := 720;
          MainForm.dwSelectedVideoWidth  := 1280;
        end;
    2:  begin
          MainForm.dwSelectedVideoHeight := 1080;
          MainForm.dwSelectedVideoWidth  := 1920;
        end;
  end;
end;


// Choose the output format
// Note that this is a small list. To create more valid formats see:
// See: https://github.com/MicrosoftDocs/win32/blob/docs/desktop-src/medfound/h-264-video-encoder.md
procedure TdlgVideoSetttings.cbxEncodingFormatCloseUp(Sender: TObject);
begin
  if (cbxEncodingFormat.Items.Count > 0) and (cbxEncodingFormat.ItemIndex > -1) then
    begin
      MainForm.sEncodingFormat := cbxEncodingFormat.Items[cbxEncodingFormat.ItemIndex];
      if MainForm.sEncodingFormat = 'MFVideoFormat_H264' then
        MainForm.tgEncodingFormat := MFVideoFormat_H264
      else if MainForm.sEncodingFormat = 'MFVideoFormat_WMV3' then
        MainForm.tgEncodingFormat := MFVideoFormat_WMV3
      else if MainForm.sEncodingFormat = 'MFVideoFormat_I420' then
        MainForm.tgEncodingFormat := MFVideoFormat_I420
      else if MainForm.sEncodingFormat = 'MFVideoFormat_IYUV' then
        MainForm.tgEncodingFormat := MFVideoFormat_IYUV
      else if MainForm.sEncodingFormat = 'MFVideoFormat_NV12' then
        MainForm.tgEncodingFormat := MFVideoFormat_NV12
      else if MainForm.sEncodingFormat = 'MFVideoFormat_YUY2' then
        MainForm.tgEncodingFormat := MFVideoFormat_YUY2
      else if MainForm.sEncodingFormat = 'MFVideoFormat_YV12' then
        MainForm.tgEncodingFormat := MFVideoFormat_YV12
      else // default
        begin
          MainForm.sExtension := 'mp4';
          MainForm.tgEncodingFormat := MFVideoFormat_H264;
          MainForm.sEncodingFormat  := 'MFVideoFormat_H264';
        end;
    end;
end;


end.
