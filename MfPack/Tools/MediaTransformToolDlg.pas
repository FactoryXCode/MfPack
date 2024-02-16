//
// Copyright: © FactoryX. All rights reserved.
//
// Project: WinApi - Shared
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: MediaTransformToolDlg.pas
// Kind: Pascal / Delphi unit
// Release date: 09-07-2023
// Language: ENU
//
// Revision Version: 3.1.6
// Description: MediaTransform Tool Dialog.
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX).
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 30/01/2024 All                 Morrissey release  SDK 10.0.22621.0 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: -
//
// Related objects: -
// Related projects: MfPackX316
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
// Copyright (c) Microsoft Corporation. All rights reserved.
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

unit MediaTransformToolDlg;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  WinApi.Dbg.MFTUtils;

type
  TdlgMediaTransformTool = class(TForm)
    Bevel1: TBevel;
    mmoList: TMemo;
    butListMFTs: TButton;
    butExit: TButton;
    Bevel2: TBevel;
    cbxChoose: TComboBox;
    procedure butListMFTsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    FListMFTs: TListMFTs;
  public
    { Public declarations }

  end;

var
  dlgMediaTransformTool: TdlgMediaTransformTool;

implementation

{$R *.dfm}



procedure TdlgMediaTransformTool.FormCreate(Sender: TObject);
begin
  FListMFTs := TListMFTs.Create();

  cbxChoose.Items.Clear();
  // Video
  cbxChoose.Items.Append('Video MFT''s for Input:  YUV2, Output: All');
  cbxChoose.Items.Append('Video MFT''s for Input:  YUV2, Output: YUY2');
  cbxChoose.Items.Append('Video MFT''s for Input:  YUV2, Output: RGB24');
  cbxChoose.Items.Append('Video MFT''s for Input:  YUV2, Output: NV11');
  cbxChoose.Items.Append('Video MFT''s for Input:  YUV2, Output: NV12');
  cbxChoose.Items.Append('Video MFT''s for Input:  YUV2, Output: NV21');
  cbxChoose.Items.Append('Video MFT''s for Input:  YUV2, Output: H263');
  cbxChoose.Items.Append('Video MFT''s for Input:  YUV2, Output: H264');
  cbxChoose.Items.Append('Video MFT''s for Input:  YUV2, Output: H265');
  cbxChoose.Items.Append('Video MFT''s for Input:  YUV2, Output: VP8');
  cbxChoose.Items.Append('Video MFT''s for Input:  YUV2, Output: VP9');

  // Audio
  cbxChoose.Items.Append('Audio MFT''s for Input: PCM, Output: All');
  cbxChoose.Items.Append('Audio MFT''s for Input: PCM, Output: PCM');
  cbxChoose.Items.Append('Audio MFT''s for input: PCM, output: MP3');
  cbxChoose.Items.Append('Audio MFT''s for input: PCM, output: FLAC');
  cbxChoose.Items.Append('Audio MFT''s for input: PCM, output: ALAC');
  cbxChoose.Items.Append('Audio MFT''s for input: PCM, output: OPUS');

  case cbxChoose.ItemIndex of
    // Video
    0: FListMFTs.ListTansforms(FListMFTs.videoYuv, nil);
    1: FListMFTs.ListTansforms(FListMFTs.videoYuv, @FListMFTs.videoYUV);
    2: FListMFTs.ListTansforms(FListMFTs.videoYuv, @FListMFTs.videoRGB24);
    3: FListMFTs.ListTansforms(FListMFTs.videoYuv, @FListMFTs.videoNV11);
    4: FListMFTs.ListTansforms(FListMFTs.videoYuv, @FListMFTs.videoNV12);
    5: FListMFTs.ListTansforms(FListMFTs.videoYuv, @FListMFTs.videoNV21);
    6: FListMFTs.ListTansforms(FListMFTs.videoYuv, @FListMFTs.videoH263);
    7: FListMFTs.ListTansforms(FListMFTs.videoYuv, @FListMFTs.videoH264);
    8: FListMFTs.ListTansforms(FListMFTs.videoYuv, @FListMFTs.videoH265);
    9: FListMFTs.ListTansforms(FListMFTs.videoYuv, @FListMFTs.videoVP8);
    10: FListMFTs.ListTansforms(FListMFTs.videoYuv, @FListMFTs.videoVP9);
    // Audio
    11: FListMFTs.ListTansforms(FListMFTs.audioPCM, nil);
    12: FListMFTs.ListTansforms(FListMFTs.audioPCM, @FListMFTs.audioPCM);
    13: FListMFTs.ListTansforms(FListMFTs.audioPCM, @FListMFTs.audioMP3);
    14: FListMFTs.ListTansforms(FListMFTs.audioPCM, @FListMFTs.audioFLAC);
    15: FListMFTs.ListTansforms(FListMFTs.audioPCM, @FListMFTs.audioALAC);
    16: FListMFTs.ListTansforms(FListMFTs.audioPCM, @FListMFTs.audioOPUS);
  end;

end;


procedure TdlgMediaTransformTool.butListMFTsClick(Sender: TObject);
begin
  mmoList.Clear();


end;

end.
