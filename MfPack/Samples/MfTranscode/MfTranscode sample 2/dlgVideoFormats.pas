//
// Copyright: © FactoryX. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: dlgVideoFormats.pas
// Kind: Pascal Unit
// Release date: 24-06-2023
// Language: ENU
//
// Revision Version: 3.1.7
// Description: Video formats dialog.
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
unit dlgVideoFormats;

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
  Vcl.ExtCtrls,
  Vcl.Grids,
  {MediaFoundation}
  WinApi.MediaFoundationApi.MfMetLib,
  WinApi.MfPack.VideoStandardsCheat;


type
  TVideoFormatDlg = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Bevel1: TBevel;
    butOk: TButton;
    butCancel: TButton;
    Label4: TLabel;
    cbxFormats: TComboBox;
    Label1: TLabel;
    StaticText3: TStaticText;
    StaticText4: TStaticText;
    sgVideoFormats: TStringGrid;
    cbxFrameRates: TComboBox;
    Label2: TLabel;
    cbxKeepOriginal: TCheckBox;
    Bevel2: TBevel;
    procedure FormShow(Sender: TObject);
    procedure cbxFormatsCloseUp(Sender: TObject);
    procedure butOkClick(Sender: TObject);
    procedure sgVideoFormatsClick(Sender: TObject);
    procedure cbxFrameRatesCloseUp(Sender: TObject);
    procedure cbxKeepOriginalClick(Sender: TObject);

  private
    { Private declarations }
    iVideoHeight: Integer;
    iVideoWidth: Integer;
    aFrameRate: string;
    aVideoResolutionType: string;
    pCont: TStreamContentsArray;

    function GetMediaContents(aSourceFile: TFileName): HResult;
    function GetSourceVideoProperties(): HResult;
    procedure GetFormats();
    procedure GetResolutions();
    procedure GetFrameRates();


  public
    { Public declarations }

    aFileName: TFileName;
    iSelectedResolution: Integer;
    iSelectedFrameRate: Integer;
  end;

var
  VideoFormatDlg: TVideoFormatDlg;

implementation

{$R *.dfm}

uses
  WinApi.MediaFoundationApi.MfIdl,
  WinApi.MediaFoundationApi.MfObjects;


function TVideoFormatDlg.GetMediaContents(aSourceFile: TFileName): HResult;
var
  hr: HResult;
  pSource: IMFMediaSource;
  pPresentationDescriptor: IMFPresentationDescriptor;

begin

  if not FileExists(aSourceFile) then
    begin
      Result := E_INVALIDARG;
      Exit;
    end;

  // Create the media source.
  hr := CreateObjectFromUrl(aSourceFile,
                            pSource);

  if SUCCEEDED(hr) then
    hr := pSource.CreatePresentationDescriptor(pPresentationDescriptor);

  if SUCCEEDED(hr) then
    hr := GetStreamContents(pPresentationDescriptor,
                            pSource,
                            pCont);

  Result := hr;
end;


function TVideoFormatDlg.GetSourceVideoProperties(): HResult;
var
  i: Integer;

begin

  Result := E_FAIL;

  for i := 0 to Length(pCont) -1 do
    begin
      if pCont[i].bSelected and (pCont[i].idStreamMediaType = mtVideo) then
        begin
          iVideoHeight := pCont[i].video_FrameSizeHeigth;
          iVideoWidth := pCont[i].video_FrameSizeWidth;
          aFrameRate := Format('%f', [pCont[i].video_FrameRateNumerator / pCont[i].video_FrameRateDenominator]);
          Result := S_OK;
          Break;
        end;
    end;

  aVideoResolutionType := FVideoStandardsCheat.GetResolutionType(iVideoWidth,
                                                                 iVideoHeight);
end;


procedure TVideoFormatDlg.butOkClick(Sender: TObject);
begin
  if cbxKeepOriginal.Checked then
    iSelectedResolution := -1
  else
    iSelectedResolution := StrToInt(sgVideoFormats.Cells[2, sgVideoFormats.Row]);

  iSelectedFrameRate := cbxFrameRates.ItemIndex;
  ModalResult := mrOk;
end;


procedure TVideoFormatDlg.cbxFormatsCloseUp(Sender: TObject);
begin
  GetResolutions();
end;


procedure TVideoFormatDlg.cbxFrameRatesCloseUp(Sender: TObject);
begin
  iSelectedFrameRate := cbxFrameRates.ItemIndex;
  FVideoStandardsCheat.SetFrameRateByIndex(iSelectedFrameRate);
end;


procedure TVideoFormatDlg.cbxKeepOriginalClick(Sender: TObject);
begin
  sgVideoFormats.Enabled := not cbxKeepOriginal.Checked;
end;


procedure TVideoFormatDlg.FormShow(Sender: TObject);
begin
  // Get the media contents from the source mediafile.
  if SUCCEEDED(GetMediaContents(aFileName)) then

    if SUCCEEDED(GetSourceVideoProperties()) then
      begin
        GetFormats();
        GetFrameRates();
      end;
end;


procedure TVideoFormatDlg.GetFormats();
var
  i: Integer;
  tRes: string;

begin
try
  tRes := aVideoResolutionType;
  cbxFormats.Clear();
  // NOTE: FVideoStandardsCheat is created in the TransCoder class as a global class.
  // FVideoStandardsCheat := TVideoStandardsCheat.Create();


  for i := 0 to Length(FVideoStandardsCheat.Resolutions) - 1 do
    begin
      if (tRes = FVideoStandardsCheat.Resolutions[i].Resolution) then
        begin
          cbxFormats.Items.Append(string(FVideoStandardsCheat.Resolutions[i].Resolution));
          Break;
        end;
      // Remember
      //tRes := FVideoStandardsCheat.Resolutions[i].Resolution;
    end;

finally
  cbxFormats.ItemIndex := 0;
  GetResolutions();
end;
end;


procedure TVideoFormatDlg.GetFrameRates();
var
  i: Integer;

begin
  cbxFrameRates.Items.Clear;
  for i := 0 to Length(FVideoStandardsCheat.FrameRates) - 1 do
    cbxFrameRates.Items.Append(FVideoStandardsCheat.FrameRates[i].sFrameRate);

  // Select the FrameRate of the original.
  for i := 0 to cbxFrameRates.Items.Count - 1 do
    begin
      if (cbxFrameRates.Items[i] = aFrameRate) then
        begin
          cbxFrameRates.ItemIndex := i;
          iSelectedFrameRate := i;
          Break;
        end;
    end;
end;


procedure TVideoFormatDlg.sgVideoFormatsClick(Sender: TObject);
begin
  if not cbxKeepOriginal.Checked then
    begin
      iSelectedResolution := StrToInt(sgVideoFormats.Cells[2, sgVideoFormats.Row]);
      FVideoStandardsCheat.SetResolutionByIndex(iSelectedResolution);
    end
  else
    iSelectedResolution := -1;
end;


procedure TVideoFormatDlg.GetResolutions();
var
  i, j: Integer;

begin
  // Clear the grid.
  for i := 0 to sgVideoFormats.ColCount - 1 do
    sgVideoFormats.Cols[i].Clear;

  // We need the following arrayvalues to show in the gridcells.

  // Initialize the grid.
  sgVideoFormats.ColCount := 3;
  sgVideoFormats.RowCount := 0;

  sgVideoFormats.ColWidths[0] := 120; // kbps.
  sgVideoFormats.ColWidths[1] := 120; // Khz.
  sgVideoFormats.ColWidths[2] := -1;  // Hide last column .

  // List compression formats.

  {$IFDEF ConditionalExpressions}
    {$IF CompilerVersion > 31.0}
       sgVideoFormats.BeginUpdate();
    {$IFEND}
  {$ENDIF}

  j := -1;
  for i := 0 to Length(FVideoStandardsCheat.Resolutions) - 1 do
    if (cbxFormats.Items[cbxFormats.ItemIndex] = string(FVideoStandardsCheat.Resolutions[i].Resolution)) then
      begin
        inc(j);
        sgVideoFormats.RowCount := j;
        sgVideoFormats.Cells[0, j] := Format('%d x %d', [FVideoStandardsCheat.Resolutions[i].iWidth, FVideoStandardsCheat.Resolutions[i].iHeight]);
        sgVideoFormats.Cells[1, j] := string(FVideoStandardsCheat.Resolutions[i].StrAspectRatio);
        // Store the index in the invisible column.
        sgVideoFormats.Cells[2, j] := IntToStr(i); // = array index!
      end;


  // Select the format of the original.
  for i := 0 to sgVideoFormats.RowCount - 1 do
    begin
      if sgVideoFormats.Cells[0, j] = Format('%d x %d', [iVideoHeight, iVideoWidth]) then
        begin
          sgVideoFormats.Row := j;
          iSelectedResolution := StrToInt(sgVideoFormats.Cells[2, j]);
          Break;
        end;
    end;

  {$IFDEF ConditionalExpressions}
    {$IF CompilerVersion > 31.0}
       sgVideoFormats.EndUpdate();
    {$IFEND}
  {$ENDIF}

end;

end.
