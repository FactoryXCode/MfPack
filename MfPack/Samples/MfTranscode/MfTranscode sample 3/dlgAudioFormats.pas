//
// Copyright: © FactoryX. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: dlgAudioFormats.pas
// Kind: Pascal Unit
// Release date: 24-06-2023
// Language: ENU
//
// Revision Version: 3.1.7
// Description: Audio formats dialog.
//
// Company: FactoryX
// Intiator(s): Tony (maXcomX), Peter (OzShips), Ramyses De Macedo Rodrigues.
// Contributor(s): Tony Kalf (maXcomX)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 30/06/2024 All                 RammStein release  SDK 10.0.26100.0 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 7 or higher.
//
// Related objects: -
// Related projects: MfPackX317
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.26100.0
//
// Todo: -
//
// =============================================================================
// Source: Transcoding Example
//
// Copyright (c) Microsoft Corporation. All rights reserved .
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
unit dlgAudioFormats;

interface

uses
  Winapi.Windows,
  WinApi.Messages,
  System.SysUtils,
  System.Classes,
  Vcl.Graphics,
  Vcl.Forms,
  Vcl.Controls,
  Vcl.StdCtrls,
  Vcl.Buttons,
  Vcl.ExtCtrls,
  Vcl.Grids,
  WinApi.MediaFoundationApi.MfApi,
  WinApi.MediaFoundationApi.MfMetLib,
  WinApi.MediaFoundationApi.MfUtils,
  Common;


type
  TAudioFormatDlg = class(TForm)
    butOk: TButton;
    butCancel: TButton;
    Bevel1: TBevel;
    lblAudioFmt: TLabel;
    butSaveToFile: TButton;
    stxtBitRate: TStaticText;
    stxtSampleRate: TStaticText;
    stxtChannels: TStaticText;
    sgAudioFormats: TStringGrid;
    stxtBitsPerSample: TStaticText;
    procedure butOkClick(Sender: TObject);
    procedure sgAudioFormatsClick(Sender: TObject);
    procedure rbSortAscClick(Sender: TObject);
    procedure rbSortDescClick(Sender: TObject);
    procedure butSaveToFileClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure stxtBitRateClick(Sender: TObject);
    procedure stxtSampleRateClick(Sender: TObject);
    procedure stxtBitsPerSampleClick(Sender: TObject);
    procedure stxtChannelsClick(Sender: TObject);

  private
    { Private declarations }
    procedure Populate();
    procedure ResetAudioFormatArray();
    procedure SortSwitch(var Sender: TStaticText;
                         aTag: Integer;
                         aCol: Integer);
    // Grid sorting methods (Author: Peter Below)
    procedure SortStringgrid(byColumn: LongInt;
                             ascending: Boolean );
  public
    { Public declarations }
    iSelectedFormat: Integer;
    aAudioFmts: TMFAudioFormatArray;

    function GetAudioFormats(const AudioFormat: TGuid): HResult;
    procedure SaveAudioFmtsToFile();

  end;

var
  AudioFormatDlg: TAudioFormatDlg;


implementation

{$R *.dfm}


procedure TAudioFormatDlg.butOkClick(Sender: TObject);
begin
  iSelectedFormat := StrToInt(sgAudioFormats.Cells[4, sgAudioFormats.Row]);
  ModalResult := mrOk;
end;


procedure TAudioFormatDlg.ResetAudioFormatArray();
var
  i: Integer;

begin
  // Reset the array
  if Length(aAudioFmts) > 0 then
    begin
      for i := 0 to Length(aAudioFmts) -1 do
        aAudioFmts[i].Reset;
      SetLength(aAudioFmts,
                0);
    end;
end;


procedure TAudioFormatDlg.sgAudioFormatsClick(Sender: TObject);
begin
  iSelectedFormat := StrToInt(sgAudioFormats.Cells[4, sgAudioFormats.Row]);
end;


procedure TAudioFormatDlg.butSaveToFileClick(Sender: TObject);
begin
  SaveAudioFmtsToFile();
end;


procedure TAudioFormatDlg.FormShow(Sender: TObject);
begin
  sgAudioFormats.Row := 0;
end;


function TAudioFormatDlg.GetAudioFormats(const AudioFormat: TGuid): HResult;
var
  hr: HResult;

begin
  ResetAudioFormatArray();
  // Get the encoder formats from the selected audioformat.
  hr := GetWinAudioEncoderFormats(AudioFormat,
                                  MFT_ENUM_FLAG_ALL, {MFT_ENUM_FLAG_TRANSCODE_ONLY,}
                                  aAudioFmts);
  if SUCCEEDED(hr) then
    Populate()
  else
    begin
      // Show a message
      DebugMsg(SysErrorMessage(hr),
               hr);
      butCancel.Click();
    end;
  Result := hr;
end;


procedure TAudioFormatDlg.SaveAudioFmtsToFile();
var
  i: Integer;
  sTmp: string;
  sl: TStringlist;

begin
  sl :=  TStringlist.Create();

  for i := 0 to Length(aAudioFmts) -1 do
    begin
      sTmp :=        '================== Index: ' + IntToStr(i) + ' ======================' + #13;
      sTmp := sTmp + '            Major Format: ' + aAudioFmts[i].wcMajorFormat + #13;
      sTmp := sTmp + '              Sub Format: ' + aAudioFmts[i].wcSubFormat + #13;
      sTmp := sTmp + '                  FOURCC: ' + aAudioFmts[i].wcFormatTag + #13;
      sTmp := sTmp + '             Description: ' + aAudioFmts[i].wsDescr + #13 + #13;
      sTmp := sTmp + '                Channels: ' + IntToStr(aAudioFmts[i].unChannels) + #13;
      sTmp := sTmp + '      Samples Per Second: ' + IntToStr(aAudioFmts[i].unSamplesPerSec) + #13;
      sTmp := sTmp + 'Float Samples Per Second: ' + IntToStr(aAudioFmts[i].unSamplesPerSec) + #13;
      sTmp := sTmp + '       Samples Per Block: ' + IntToStr(aAudioFmts[i].unSamplesPerBlock) + #13;
      sTmp := sTmp + '   Valid Bits Per Sample: ' + IntToStr(aAudioFmts[i].unValidBitsPerSample) + #13;
      sTmp := sTmp + '         Bits Per Sample: ' + IntToStr(aAudioFmts[i].unBitsPerSample) + #13;
      sTmp := sTmp + '         Block Alignment: ' + IntToStr(aAudioFmts[i].unBlockAlignment) + #13;
      sTmp := sTmp + 'Average Bytes Per Second: ' + IntToStr(aAudioFmts[i].unAvgBytesPerSec) + #13;
      sTmp := sTmp + '             ChannelMask: ' + IntToStr(aAudioFmts[i].unChannelMask) + #13;

      if (aAudioFmts[i].wcSubFormat = 'MFAudioFormat_FLAC') then
        sTmp := sTmp + '         FLAC Extra Data: ' + IntToStr(aAudioFmts[i].unChannelMask) + #13;

      sTmp := sTmp + #13;

      // Calculate bit rate in kbps.
      // Bitrate (kbps) = (Average Bytes Per Sample * 8) / 1000
      sTmp := sTmp + '          Bitrate (kbps): ' + FloatToStr(aAudioFmts[i].unAvgBytesPerSec * 8 / 1000) + #13;

      // Calculate sample rate in khz.
      // Samples Per Second / 1000.
      sTmp := sTmp + '     Sampling Rate (khz): ' + FloatToStr(aAudioFmts[i].unSamplesPerSec / 1000) + #13 + #13;

      sl.Append(sTmp);
    end;

  sl.SaveToFile(Format('Formats %s.txt', [aAudioFmts[0].wcSubFormat]));
  FreeAndNil(sl);
end;


procedure TAudioFormatDlg.Populate();
var
  i: Integer;

begin
  // Clear the grid
  for i := 0 to sgAudioFormats.ColCount - 1 do
    sgAudioFormats.Cols[i].Clear;
  sgAudioFormats.RowCount := 1;

  lblAudioFmt.Caption := Format('%s  %s',[aAudioFmts[0].wcFormatTag, aAudioFmts[0].wsDescr]);

  // We need the following arrayvalues to show in the gridcells.

  // initialize the grid
  sgAudioFormats.ColCount := 5;
  sgAudioFormats.RowCount := 1;

  sgAudioFormats.ColWidths[0] := 100; // kbps
  sgAudioFormats.ColWidths[1] := 100; // Khz
  sgAudioFormats.ColWidths[2] := 100; // Bits per sample
  sgAudioFormats.ColWidths[3] := 100; // Channels
  sgAudioFormats.ColWidths[4] := -1;  // Hide last column

  // List compression formats.

  {$IFDEF ConditionalExpressions}
    {$IF CompilerVersion > 31.0}
       sgAudioFormats.BeginUpdate();
    {$IFEND}
  {$ENDIF}

  sgAudioFormats.RowCount := Length(aAudioFmts);

  for i := 0 to Length(aAudioFmts) -1 do
    begin

      // Calculate the bit rate:
      // Bit rate = Average Bytes Per Second * 8 / 1000
      sgAudioFormats.Cells[0, i] := FloatToStr(aAudioFmts[i].unAvgBytesPerSec * 8 / 1000) + #13;
      // Calculate the sampling rate:
      // Sample Rate (kHz) = Sample Rate (Hz) / 1000
      sgAudioFormats.Cells[1, i] := FloatToStr((aAudioFmts[i].unSamplesPerSec / 1000));
      sgAudioFormats.Cells[2, i] := IntToStr(aAudioFmts[i].unBitsPerSample);
      sgAudioFormats.Cells[3, i] := IntToStr(aAudioFmts[i].unChannels);

      // invisible
      sgAudioFormats.Cells[4, i] := IntToStr(i); // index!

    end;

  {$IFDEF ConditionalExpressions}
    {$IF CompilerVersion > 31.0}
       sgAudioFormats.EndUpdate();
    {$IFEND}
  {$ENDIF}
end;


procedure TAudioFormatDlg.rbSortAscClick(Sender: TObject);
begin
  SortStringgrid(0,
                 True);
end;


procedure TAudioFormatDlg.rbSortDescClick(Sender: TObject);
begin
  SortStringgrid(0,
                 False);
end;


procedure TAudioFormatDlg.SortStringgrid(byColumn: LongInt;
                                         ascending: Boolean );
  // Helpers
  procedure ExchangeGridRows(i: Integer;
                             j: Integer);
  var
    k: Integer;

  begin
    for k := 0 to sgAudioFormats.ColCount -1 Do
      sgAudioFormats.Cols[k].Exchange(i,
                                      j);
  end;

  procedure QuickSort(L: Integer;
                      R: Integer);
  var
    I: Integer;
    J: Integer;
    P: string;

  begin
    repeat
      I := L;
      J := R;
      P := sgAudioFormats.Cells[byColumn, (L + R) shr 1];
      repeat
        while (CompareStr(sgAudioFormats.Cells[byColumn, I],
                          P) < 0) do
          Inc(I);
        while (CompareStr(sgAudioFormats.Cells[byColumn, J],
                          P) > 0) do
          Dec(J);
        if (I <= J) then
          begin
            if (I <> J) Then
              ExchangeGridRows(I,
                               J);
            Inc(I);
            Dec(J);
          end;
      until (I > J);

      if (L < J) then
        QuickSort(L, J);
      L := I;
    until (I >= R);
  end;

  procedure InvertGrid();
  var
    i, j: Integer;

  begin
    i := sgAudioFormats.Fixedrows;
    j := sgAudioFormats.Rowcount -1;
    while (i < j) do
      begin
        ExchangeGridRows(i,
                         j);
        Inc(i);
        Dec(j);
      end; { While }
   end;

begin
  Screen.Cursor := crHourglass;
  sgAudioFormats.Perform(WM_SETREDRAW,
                         0,
                         0);
  try
    QuickSort(sgAudioFormats.FixedRows,
              sgAudioFormats.Rowcount-1 );
    if not ascending Then
      InvertGrid();
  finally
    sgAudioFormats.Perform(WM_SETREDRAW,
                 1,
                 0);
    sgAudioFormats.Refresh;
    Screen.Cursor := crDefault;
  end;
end;


procedure TAudioFormatDlg.SortSwitch(var Sender: TStaticText;
                                     aTag: Integer;
                                     aCol: Integer);
begin
  if Sender.Tag = 0 then
    begin
      SortStringgrid(aCol, True);
      Sender.Tag := 1;
    end
  else
    begin
      SortStringgrid(aCol, False);
      Sender.Tag := 0;
    end;
end;


procedure TAudioFormatDlg.stxtBitRateClick(Sender: TObject);
begin
  SortSwitch(stxtBitRate,
             stxtBitRate.Tag,
             0);
end;


procedure TAudioFormatDlg.stxtBitsPerSampleClick(Sender: TObject);
begin
  SortSwitch(stxtBitsPerSample,
             stxtBitsPerSample.Tag,
             2);
end;


procedure TAudioFormatDlg.stxtChannelsClick(Sender: TObject);
begin
  SortSwitch(stxtSampleRate,
             stxtSampleRate.Tag,
             3);
end;


procedure TAudioFormatDlg.stxtSampleRateClick(Sender: TObject);
begin
  SortSwitch(stxtSampleRate,
             stxtSampleRate.Tag,
             1);
end;


end.
