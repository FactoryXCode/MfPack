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
// Revision Version: 3.1.5
// Description: UI for example of how to use the SinkWriter.
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX), Renate Schaaf.
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 28/08/2022 All                 PiL release  SDK 10.0.22621.0 (Windows 11)
// 18/05/2023 Renate              Fixed runtime error on selecting multiple bitmaps.
//                                Speedup of bitmap resizing using D2D1_1
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 or later.
//
// Related objects: -
// Related projects: MfPack/Samples/SinkWriterToEncodeVideoSample
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.22621.0
//
// Todo: -
//
//==============================================================================
// Source:
//   https://learn.microsoft.com/en-us/windows/win32/medfound/tutorial--using-the-sink-writer-to-encode-video
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
unit dlgVideoOutput;
interface
uses
  {Winapi}
  Winapi.Windows,
  {System}
  System.SysUtils,
  System.Classes,
  {Vcl}
  Vcl.Graphics,
  Vcl.Forms,
  Vcl.Controls,
  Vcl.StdCtrls,
  Vcl.Buttons,
  Vcl.ExtCtrls,
  Vcl.Dialogs,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfApi,
  {Application}
  SinkWriterClass;
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
    cbSaveResizedBitmap: TCheckBox;
    Label5: TLabel;
    edFps: TEdit;
    Label6: TLabel;
    edBitRate: TEdit;
    edFrameTimeUnits: TEdit;
    Label7: TLabel;
    procedure cbxOutputFormatCloseUp(Sender: TObject);
    procedure cbxEncodingFormatCloseUp(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure cbxDimensionsCloseUp(Sender: TObject);
    procedure cbSaveResizedBitmapClick(Sender: TObject);
    procedure edFpsChange(Sender: TObject);
    procedure edBitRateChange(Sender: TObject);
    procedure edFpsEnter(Sender: TObject);
    procedure edFrameTimeUnitsChange(Sender: TObject);
  private
    { Private declarations }
    //procedure CalculateAverageFrameRate();

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
      FSinkWriter.SinkWriterParams.pwcVideoFileExtension := PWideChar(LowerCase(cbxOutputFormat.Items[cbxOutputFormat.ItemIndex]));
      cbxEncodingFormat.Clear;
      // populate the cbxEncodingFormat with supported formats when ouputformat is selected.
      if FSinkWriter.SinkWriterParams.pwcVideoFileExtension = 'mp4' then
        begin
          cbxEncodingFormat.Items.Append('MFVideoFormat_H264');
          cbxEncodingFormat.ItemIndex := 0;
        end
      else if FSinkWriter.SinkWriterParams.pwcVideoFileExtension = 'wmv' then
        begin
          cbxEncodingFormat.Items.Append('MFVideoFormat_WMV3');
          cbxEncodingFormat.ItemIndex := 0;
        end
      else if FSinkWriter.SinkWriterParams.pwcVideoFileExtension = 'avi' then
        begin
          cbxEncodingFormat.Items.Append('MFVideoFormat_I420');
          cbxEncodingFormat.Items.Append('MFVideoFormat_IYUV');
          cbxEncodingFormat.Items.Append('MFVideoFormat_NV12');
          cbxEncodingFormat.Items.Append('MFVideoFormat_YUY2');
          cbxEncodingFormat.Items.Append('MFVideoFormat_YV12');
        end
      else // default
        begin
          FSinkWriter.SinkWriterParams.pwcVideoFileExtension := 'mp4';
          FSinkWriter.SinkWriterParams.gdEncodingFormat := MFVideoFormat_H264;
          FSinkWriter.SinkWriterParams.sEncodingFormat  := GuidToString(FSinkWriter.SinkWriterParams.gdEncodingFormat);
        end;
      cbxEncodingFormat.ItemIndex := 0;
    end;
end;


//procedure TdlgVideoSetttings.CalculateAverageFrameRate();
//begin
//  if (FSinkWriter.SinkWriterParams.dbFrameRate > 0.0)  and (FSinkWriter.SinkWriterParams.dwFrameTimeUnits >= 10000 ) then
//    FSinkWriter.SinkWriterParams.rtAverageTimePerFrame := (FSinkWriter.SinkWriterParams.dwFrameTimeUnits) div Round(FSinkWriter.SinkWriterParams.dbFrameRate);
//end;


procedure TdlgVideoSetttings.edFpsChange(Sender: TObject);
begin
  if (StrToFloat(edFps.Text) <= 0) then
    ShowMessage('You must enter a number > 0')
  else
    TryStrToFloat(edFps.Text,
                  FSinkWriter.SinkWriterParams.dbFrameRate);
end;


procedure TdlgVideoSetttings.edFpsEnter(Sender: TObject);
begin
  edFps.ShowHint := True;
  // Enter hints per chosen resolution.
  case cbxDimensions.ItemIndex of
    5: begin
         edFps.Hint := '2K supports 12, 24, 25, 29.97, 30, 48, 50, 59.94 and 60 FPS.';
       end;
    7: begin
         edFps.Hint := '4K supports 24, 25, 29.97, 30, 48, 50, 59.94 and 60 FPS.';
       end;
    else
       begin
         edFps.Hint := 'FrameRate in FPS. The less movement the less FPS are needed. Recommended is 12 FPS for animations.';
       end;
  end;
end;


procedure TdlgVideoSetttings.edFrameTimeUnitsChange(Sender: TObject);
begin
  if (StrToInt(edFrameTimeUnits.Text) < 10000 { = 1 ms }) then
    ShowMessage('You must enter a number >= 10000')
  else
    FSinkWriter.SinkWriterParams.dwFrameTimeUnits := StrToInt(edFrameTimeUnits.Text);
end;


procedure TdlgVideoSetttings.edBitRateChange(Sender: TObject);
begin
  FSinkWriter.SinkWriterParams.dwBitRate := StrToInt(edBitRate.Text);
end;


// Optionally you can save the resized bitmap to a file.
procedure TdlgVideoSetttings.cbSaveResizedBitmapClick(Sender: TObject);
begin
  FSinkWriter.SaveResizedBitmap := cbSaveResizedBitmap.Checked;
end;


procedure TdlgVideoSetttings.FormCreate(Sender: TObject);
begin
  if not Assigned(FSinkWriter) then
    Close();
  // Set initial states and values.
  cbxOutputFormat.ItemIndex := cbxOutputFormat.Items.IndexOf(FSinkWriter.SinkWriterParams.pwcVideoFileExtension);
  cbxOutputFormatCloseUp(Self);
  //cbxEncodingFormatCloseUp
  cbxEncodingFormatCloseUp(Self);
  cbxEncodingFormat.ItemIndex := cbxEncodingFormat.Items.IndexOf(FSinkWriter.SinkWriterParams.sEncodingFormat);
  cbxDimensions.Clear;
  // Add common used resolutions
  cbxDimensions.Items.Append('SD    360p  (640 x 360)');
  cbxDimensions.Items.Append('SD    480p  (640 x 480)');
  cbxDimensions.Items.Append('SD    480p  (854 x 480)');  // ! YouTube format
  cbxDimensions.Items.Append('HD    720p  (1280 x 720)');
  cbxDimensions.Items.Append('FHD  1080p  (1920 x 1080)');
  cbxDimensions.Items.Append('2K   1080p  (2048 x 1080)');
  cbxDimensions.Items.Append('QHD  1440p  (2560 x 1440)');
  cbxDimensions.Items.Append('4K   2160p  (3840 x 2160)');
  cbxDimensions.ItemIndex := cbxDimensions.Items.IndexOf(FSinkWriter.SinkWriterParams.sResolutionDescription);
  cbxDimensionsCloseUp(Self);
  edBitRate.Text := IntToStr(FSinkWriter.SinkWriterParams.dwBitRate);
  edFps.Text := FloatToStr(FSinkWriter.SinkWriterParams.dbFrameRate);
  cbSaveResizedBitmap.Checked := FSinkWriter.SaveResizedBitmap;
end;


procedure TdlgVideoSetttings.cbxDimensionsCloseUp(Sender: TObject);
begin
// List of resolutions (Beware that Facebook and Youtube supports their own formats)
//
// SD (Standard Definition)
// 640 x 360 (360p) 4:3
// 640 x 480 (480p) 4:3
//

// HD (High Definition)
// 1280 x 720 (720p) 16:9
//
// Full HD Resolution
// 1920 x 1080 (1080p) 16:9
//
// QHD (Quad High Definition)
// 2560 x 1440 (1440p) 16:9
//
// 2K Resolution
// 2048 x 1080 (1080p) 1:1.77
// Note that this resolution must have a minimum framerate of 12 FPS.
//
// 4K Resolution (UHD/Ultra High Definition)
// 3840 x 2160 (2160p) 1:1.9
// Note that this resolution must have a minimum framerate of 24 FPS.
//
  case cbxDimensions.ItemIndex of
    0:  begin
          FSinkWriter.SinkWriterParams.dwWidth  := 640;
          FSinkWriter.SinkWriterParams.dwHeigth := 360;
          FSinkWriter.SinkWriterParams.sResolutionDescription := 'SD    360p  (640 x 360)';
        end;
    1:  begin
          FSinkWriter.SinkWriterParams.dwWidth  := 640;
          FSinkWriter.SinkWriterParams.dwHeigth := 480;
          FSinkWriter.SinkWriterParams.sResolutionDescription := 'SD    480p  (640 x 480)';
        end;
    2:  begin {YouTube}
          FSinkWriter.SinkWriterParams.dwWidth  := 854;
          FSinkWriter.SinkWriterParams.dwHeigth := 480;
          FSinkWriter.SinkWriterParams.sResolutionDescription := 'SD    480p  (854 x 480)';
        end;
    3:  begin
          FSinkWriter.SinkWriterParams.dwWidth  := 1280;
          FSinkWriter.SinkWriterParams.dwHeigth := 720;
          FSinkWriter.SinkWriterParams.sResolutionDescription := 'HD    720p  (1280 x 720)';
        end;
    4:  begin
          FSinkWriter.SinkWriterParams.dwWidth  := 1920;
          FSinkWriter.SinkWriterParams.dwHeigth := 1080;
          FSinkWriter.SinkWriterParams.sResolutionDescription := 'FHD  1080p  (1920 x 1080)';
        end;
    5:  begin
          FSinkWriter.SinkWriterParams.dwWidth  := 2048;
          FSinkWriter.SinkWriterParams.dwHeigth := 1080;
          FSinkWriter.SinkWriterParams.sResolutionDescription := '2K   1080p  (2048 x 1080)';
          edFPS.Text := '12';  // 12 or 24 FPS
        end;
    6:  begin
          FSinkWriter.SinkWriterParams.dwWidth  := 2560;
          FSinkWriter.SinkWriterParams.dwHeigth := 1440;
          FSinkWriter.SinkWriterParams.sResolutionDescription := 'QHD  1440p  (2560 x 1440)';
        end;
    7:  begin
          FSinkWriter.SinkWriterParams.dwWidth  := 3840;
          FSinkWriter.SinkWriterParams.dwHeigth := 2160;
          FSinkWriter.SinkWriterParams.sResolutionDescription := '4K   2160p  (3840 x 2160)';
          edFPS.Text := '24';  // 24, 25, 29.97, 30, 48, 50, 59.94 and 60 FPS.
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
      FSinkWriter.SinkWriterParams.sEncodingFormat := cbxEncodingFormat.Items[cbxEncodingFormat.ItemIndex];
      if FSinkWriter.SinkWriterParams.sEncodingFormat = 'MFVideoFormat_H264' then
        FSinkWriter.SinkWriterParams.gdEncodingFormat := MFVideoFormat_H264
      else if FSinkWriter.SinkWriterParams.sEncodingFormat = 'MFVideoFormat_WMV3' then
        FSinkWriter.SinkWriterParams.gdEncodingFormat := MFVideoFormat_WMV3
      else if FSinkWriter.SinkWriterParams.sEncodingFormat = 'MFVideoFormat_I420' then
        FSinkWriter.SinkWriterParams.gdEncodingFormat := MFVideoFormat_I420
      else if FSinkWriter.SinkWriterParams.sEncodingFormat = 'MFVideoFormat_IYUV' then
        FSinkWriter.SinkWriterParams.gdEncodingFormat := MFVideoFormat_IYUV
      else if FSinkWriter.SinkWriterParams.sEncodingFormat = 'MFVideoFormat_NV12' then
        FSinkWriter.SinkWriterParams.gdEncodingFormat := MFVideoFormat_NV12
      else if FSinkWriter.SinkWriterParams.sEncodingFormat = 'MFVideoFormat_YUY2' then
        FSinkWriter.SinkWriterParams.gdEncodingFormat := MFVideoFormat_YUY2
      else if FSinkWriter.SinkWriterParams.sEncodingFormat = 'MFVideoFormat_YV12' then
        FSinkWriter.SinkWriterParams.gdEncodingFormat := MFVideoFormat_YV12
      else // default
        begin
          FSinkWriter.SinkWriterParams.pwcVideoFileExtension := 'mp4';
          FSinkWriter.SinkWriterParams.gdEncodingFormat := MFVideoFormat_H264;
          FSinkWriter.SinkWriterParams.sEncodingFormat := 'MFVideoFormat_H264';
        end;
    end;end;
end.
