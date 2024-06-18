// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: dlgAudioFormats.pas
// Kind: Pascal Unit
// Release date: 24-02-2024
// Language: ENU
//
// Revision Version: 3.1.7
// Description: Audio formats dialog.
//
// Company: FactoryX
// Intiator(s): Tony (maXcomX), Peter (OzShips).
// Contributor(s): Tony Kalf (maXcomX)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 19/06/2024 All                 RammStein release  SDK 10.0.22621.0 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 7 or higher.
//
// Related objects: -
// Related projects: MfPackX317
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.22621.0
//
// Todo: -
//
// =============================================================================
// Source: FactoryX.Code Sinkwriter and Transcode Example
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
unit dlgAudioFormats;

interface

uses
  {WinApi}
  Winapi.Windows,
  WinApi.Messages,
  {VCL}
  Vcl.Forms,
  Vcl.Controls,
  Vcl.StdCtrls,
  Vcl.Buttons,
  Vcl.ExtCtrls,
  Vcl.Grids,
  {System}
  System.SysUtils,
  System.Classes,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfApi,
  WinApi.MediaFoundationApi.MfMetLib,
  WinApi.MediaFoundationApi.MfUtils;

const
  CRLF = #13 + #10;


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
    stxtExtraInfo: TStaticText;
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
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure sgAudioFormatsDblClick(Sender: TObject);

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
    fAudioCodecDescription: TStringList;

    function GetAudioFormats(const AudioFormat: TGuid): HResult;
    procedure GetFormatDescription(bGetAll: Boolean = False);
    procedure SaveAudioFmtsToFile();

  end;

var
  AudioFormatDlg: TAudioFormatDlg;


implementation

{$R *.dfm}


procedure TAudioFormatDlg.butOkClick(Sender: TObject);
begin
  iSelectedFormat := StrToInt(sgAudioFormats.Cells[4, sgAudioFormats.Row]);
  GetFormatDescription();
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


  if (aAudioFmts[iSelectedFormat].wcSubFormat = 'MFAudioFormat_AAC') then
    begin
      //
      stxtExtraInfo.Caption := '             AAC PayLoad: ' + IntToStr(aAudioFmts[iSelectedFormat].unAACPayload) + CRLF +
                               ' AAC PayLoad Description: ' + string(aAudioFmts[iSelectedFormat].wsAACPayloadDescription) + CRLF +
                               '       AAC Profile Level: ' + IntToStr(aAudioFmts[iSelectedFormat].unAACProfileLevel) + CRLF +
                               ' AAC Profile Description: ' + string(aAudioFmts[iSelectedFormat].wsAACProfileLevelDescription);
    end;
end;


procedure TAudioFormatDlg.sgAudioFormatsDblClick(Sender: TObject);
begin
  sgAudioFormatsClick(Self);
  butOkClick(Self);
end;


procedure TAudioFormatDlg.butSaveToFileClick(Sender: TObject);
begin
  SaveAudioFmtsToFile();
end;


procedure TAudioFormatDlg.FormCreate(Sender: TObject);
begin
  {$IFDEF DEBUG}
  butSaveToFile.Visible := True;
  {$ELSE}
  butSaveToFile.Visible := False;
  {$ENDIF}
end;


procedure TAudioFormatDlg.FormDestroy(Sender: TObject);
begin
  fAudioCodecDescription.Free;
end;


procedure TAudioFormatDlg.FormShow(Sender: TObject);
begin
  sgAudioFormats.Row := 0;
  fAudioCodecDescription :=  TStringlist.Create();
end;


function TAudioFormatDlg.GetAudioFormats(const AudioFormat: TGuid): HResult;
var
  hr: HResult;
begin
  ResetAudioFormatArray();
  // Get the encoder formats from the selected audioformat.
  hr := GetWinAudioEncoderFormats(AudioFormat,
                                  (MFT_ENUM_FLAG_ALL and (not MFT_ENUM_FLAG_FIELDOFUSE)) or
                                  MFT_ENUM_FLAG_SORTANDFILTER,
                                  aAudioFmts);
  if SUCCEEDED(hr) then
    Populate()
  else
    butCancel.Click();

  Result := hr;
end;


procedure TAudioFormatDlg.GetFormatDescription(bGetAll: Boolean = False);
var
  i: Integer;
  iBegin: Integer;
  iEnd: Integer;
  // Channel matrix.
  aLayout: string;
  aChannels: string;

begin
  fAudioCodecDescription.Clear();
  stxtExtraInfo.Caption := '';

  if bGetAll then
    begin
      iBegin := 0;
      iEnd := Length(aAudioFmts) -1;
    end
  else
    begin
      iBegin := iSelectedFormat;
      iEnd := iSelectedFormat;
    end;

  for i := iBegin to iEnd do
    begin

      if bGetAll then
        begin
          fAudioCodecDescription.Append(CRLF);
          fAudioCodecDescription.Append('================== Index: ' + IntToStr(i + 1) + ' ======================' + CRLF);
        end;

      fAudioCodecDescription.Append('            Major Format: ' + WideCharToString(aAudioFmts[i].wcMajorFormat));
      fAudioCodecDescription.Append('              Sub Format: ' + WideCharToString(aAudioFmts[i].wcSubFormat));
      fAudioCodecDescription.Append('                  FOURCC: ' + WideCharToString(aAudioFmts[i].wcFormatTag));
      fAudioCodecDescription.Append(CRLF);
      fAudioCodecDescription.Append('             Description: ' + WideCharToString(aAudioFmts[i].wsDescr));
      fAudioCodecDescription.Append(CRLF);

      case aAudioFmts[i].unChannels of
        1: fAudioCodecDescription.Append(Format('                Channels: %d (Mono)', [aAudioFmts[i].unChannels]));
        2: fAudioCodecDescription.Append(Format('                Channels: %d (Stereo)', [aAudioFmts[i].unChannels]));
        3: fAudioCodecDescription.Append(Format('                Channels: %d (2.1)', [aAudioFmts[i].unChannels]));
        5: fAudioCodecDescription.Append(Format('                Channels: %d (4.1)', [aAudioFmts[i].unChannels]));
        6: fAudioCodecDescription.Append(Format('                Channels: %d (5.1)', [aAudioFmts[i].unChannels]));
        7: fAudioCodecDescription.Append(Format('                Channels: %d (6.1)', [aAudioFmts[i].unChannels]));
        8: fAudioCodecDescription.Append(Format('                Channels: %d (7.1)', [aAudioFmts[i].unChannels]));
        else
          fAudioCodecDescription.Append(Format('                Channels: %d', [aAudioFmts[i].unChannels]));
      end; //case

      GetSpeakersLayOut(aAudioFmts[i].unChannelMask,
                        aLayout,
                        aChannels);

      fAudioCodecDescription.Append('          Channel Matrix: ' + aLayout);

      fAudioCodecDescription.Append('       Sample Rate (kHz): ' + FloatToStrF((aAudioFmts[i].unSamplesPerSec / 1000), ffGeneral, 4, 3));
      fAudioCodecDescription.Append(' Float Sample Rate (kHz): ' + FloatToStrF((aAudioFmts[i].dblFloatSamplePerSec / 1000), ffGeneral, 4, 3));
      fAudioCodecDescription.Append('       Samples Per Block: ' + IntToStr(aAudioFmts[i].unSamplesPerBlock));
      fAudioCodecDescription.Append('   Valid Bits Per Sample: ' + IntToStr(aAudioFmts[i].unValidBitsPerSample));
      fAudioCodecDescription.Append('         Bits Per Sample: ' + IntToStr(aAudioFmts[i].unBitsPerSample));
      fAudioCodecDescription.Append('         Block Alignment: ' + IntToStr(aAudioFmts[i].unBlockAlignment));
      fAudioCodecDescription.Append('          Bitrate (kbps): ' + FloatToStrF((aAudioFmts[i].unAvgBytesPerSec * 8) / 1000, ffGeneral, 4, 3));

      fAudioCodecDescription.Append(CRLF);

      if (aAudioFmts[i].wcSubFormat = 'MFAudioFormat_AAC') then
        begin
          fAudioCodecDescription.Append('             AAC PayLoad: ' + IntToStr(aAudioFmts[i].unAACPayload));
          fAudioCodecDescription.Append(' AAC PayLoad Description: ' + string(aAudioFmts[i].wsAACPayloadDescription));
          fAudioCodecDescription.Append('       AAC Profile Level: ' + IntToStr(aAudioFmts[i].unAACProfileLevel));
          fAudioCodecDescription.Append(' AAC Profile Description: ' + string(aAudioFmts[i].wsAACProfileLevelDescription));
        end;
    end;
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
