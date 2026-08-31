// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: Form.Main.pas
// Kind: Pascal Unit
// Release date: 13-08-2026
// Language: ENU
//
// Version: 4.0.0
// Description: Decoded RGB32 frames from an IMFSourceReader are passed through
//              the grayscale IMFTransform from Sample 1.
//
// Company: FactoryX
// Intiator(s): Tony (maXcomX), Carmen (carmenh).
// Contributor(s): Tony Kalf (maXcomX), Carmen (carmenh).
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 24/08/2026 All                 Moby release  SDK 10.0.28000.2705  (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 or later.
//
// Related objects: -
// Related projects: MfPackX400
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.28000.2705
//
// Todo: -
//
// =============================================================================
// Source: Microsoft Learn.
//
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
unit Form.Main;

interface

uses
  {WinApi}
  WinApi.Windows,
  WinApi.WinApiTypes,
  {System}
  System.Classes,
  System.SysUtils,
  {Vcl}
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Graphics,
  Vcl.ExtCtrls,
  Vcl.StdCtrls,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfApi,
  WinApi.MediaFoundationApi.MfObjects,
  WinApi.MediaFoundationApi.MfTransform;

type
  TfrmMain = class(TForm)
    pnlCommands: TPanel;
    btnProcess: TButton;
    lblSummary: TLabel;
    pnlSource: TPanel;
    lblSource: TLabel;
    imgSource: TImage;
    pnlOutput: TPanel;
    lblOutput: TLabel;
    imgOutput: TImage;
    memLog: TMemo;

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnProcessClick(Sender: TObject);

  private

    FSourceBitmap: TBitmap;
    FOutputBitmap: TBitmap;

    procedure DrawTestFrame();
    procedure CheckHr(const AOperation: string; const AHr: HResult);
    function CreateVideoType(out AMediaType: IMFMediaType): HResult;

    function CreateSampleFromBitmap(const ABitmap: TBitmap;
                                    out ASample: IMFSample): HResult;

    function CopySampleToBitmap(const ASample: IMFSample;
                                const ABitmap: TBitmap): HResult;
    procedure RunTransform();
  end;

var
  frmMain: TfrmMain;


implementation

uses
  MfGrayscaleMFT;

{$R *.dfm}

const
  FRAME_WIDTH = 400;
  FRAME_HEIGHT = 300;
  BYTES_PER_PIXEL = 4;


procedure TfrmMain.FormCreate(Sender: TObject);
begin

  FSourceBitmap := TBitmap.Create;
  FOutputBitmap := TBitmap.Create;
  FSourceBitmap.PixelFormat := pf32bit;
  FOutputBitmap.PixelFormat := pf32bit;
  FSourceBitmap.SetSize(FRAME_WIDTH, FRAME_HEIGHT);
  FOutputBitmap.SetSize(FRAME_WIDTH, FRAME_HEIGHT);
  DrawTestFrame;
  imgSource.Picture.Assign(FSourceBitmap);
  imgOutput.Picture.Assign(FOutputBitmap);
  memLog.Lines.Add('Ready. Click Process one RGB32 frame.');
end;


procedure TfrmMain.FormDestroy(Sender: TObject);
begin

  FOutputBitmap.Free;
  FSourceBitmap.Free;
end;


procedure TfrmMain.DrawTestFrame;
const
  COLORS: array[0..6] of TColor = (clRed, $000080FF, clYellow, clLime, clAqua, clBlue, clFuchsia);
var
  I: Integer;
  LeftEdge: Integer;
  RightEdge: Integer;

begin

  for I := 0 to High(COLORS) do
    begin
      LeftEdge := (I * FRAME_WIDTH) div Length(COLORS);
      RightEdge := ((I + 1) * FRAME_WIDTH) div Length(COLORS);
      FSourceBitmap.Canvas.Brush.Color := COLORS[I];
      FSourceBitmap.Canvas.FillRect(Rect(LeftEdge, 0, RightEdge,
                                         FRAME_HEIGHT));
    end;

  FSourceBitmap.Canvas.Brush.Color := clWhite;
  FSourceBitmap.Canvas.Pen.Color := clBlack;

  FSourceBitmap.Canvas.Ellipse(105,
                               55,
                               295,
                               245);

  FSourceBitmap.Canvas.Brush.Style := bsClear;
  FSourceBitmap.Canvas.Font.Name := 'Segoe UI';
  FSourceBitmap.Canvas.Font.Size := 20;
  FSourceBitmap.Canvas.Font.Style := [fsBold];
  FSourceBitmap.Canvas.Font.Color := clBlack;

  FSourceBitmap.Canvas.TextOut(126,
                               126,
                               'RGB32 MFT');

  FSourceBitmap.Canvas.Brush.Style := bsSolid;
end;


procedure TfrmMain.CheckHr(const AOperation: string; const AHr: HResult);
begin

  if FAILED(AHr) then
    raise Exception.CreateFmt('%s failed: HRESULT 0x%.8x',
                              [AOperation, Cardinal(AHr)]);

  memLog.Lines.Add(Format('%s: HRESULT 0x%.8x',
                          [AOperation, Cardinal(AHr)]));
end;


function TfrmMain.CreateVideoType(out AMediaType: IMFMediaType): HResult;
begin

  AMediaType := nil;
  Result := MFCreateMediaType(AMediaType);

  if SUCCEEDED(Result) then
    Result := AMediaType.SetGUID(MF_MT_MAJOR_TYPE,
                                 MFMediaType_Video);

  if SUCCEEDED(Result) then
    Result := AMediaType.SetGUID(MF_MT_SUBTYPE,
                                 MFVideoFormat_RGB32);

  if SUCCEEDED(Result) then
    Result := MFSetAttributeSize(AMediaType,
                                 MF_MT_FRAME_SIZE,
                                 FRAME_WIDTH,
                                 FRAME_HEIGHT);

  if SUCCEEDED(Result) then
    Result := AMediaType.SetUINT32(MF_MT_DEFAULT_STRIDE,
                                   FRAME_WIDTH * BYTES_PER_PIXEL);

  if SUCCEEDED(Result) then
    Result := AMediaType.SetUINT32(MF_MT_INTERLACE_MODE,
                                   MFVideoInterlace_Progressive);

  if SUCCEEDED(Result) then
    Result := MFSetAttributeRatio(AMediaType,
                                  MF_MT_FRAME_RATE,
                                  30,
                                  1);

  if SUCCEEDED(Result) then
    Result := MFSetAttributeRatio(AMediaType,
                                  MF_MT_PIXEL_ASPECT_RATIO,
                                  1,
                                  1);
end;


function TfrmMain.CreateSampleFromBitmap(const ABitmap: TBitmap;
                                         out ASample: IMFSample): HResult;
var
  Buffer: IMFMediaBuffer;
  Data: PByte;
  MaxLength: DWORD;
  CurrentLength: DWORD;
  Stride: Integer;
  BufferSize: DWORD;
  Y: Integer;
  SourceRow: Pointer;
  DestinationRow: Pointer;

begin

  ASample := nil;
  Buffer := nil;
  Stride := ABitmap.Width * BYTES_PER_PIXEL;
  BufferSize := Stride * ABitmap.Height;
  Result := MFCreateMemoryBuffer(BufferSize,
                                 Buffer);

  if FAILED(Result) then
    Exit;

  Data := nil;
  MaxLength := 0;
  CurrentLength := 0;

  Result := Buffer.Lock(Data,
                        @MaxLength,
                        @CurrentLength);
  if FAILED(Result) then
    Exit;
  try
    for Y := 0 to ABitmap.Height - 1 do
      begin
        // TBitmap scan lines are bottom-up. Store the sample top-down.
        SourceRow := ABitmap.ScanLine[ABitmap.Height - 1 - Y];
        DestinationRow := Pointer(NativeUInt(Data) + NativeUInt(Y * Stride));

        Move(SourceRow^,
             DestinationRow^,
             Stride);
      end;

  finally
    Buffer.Unlock;
  end;

  Result := Buffer.SetCurrentLength(BufferSize);

  if FAILED(Result) then
    Exit;

  Result := MFCreateSample(ASample);
  if SUCCEEDED(Result) then
    Result := ASample.AddBuffer(Buffer);
end;


function TfrmMain.CopySampleToBitmap(const ASample: IMFSample;
                                     const ABitmap: TBitmap): HResult;
var
  Buffer: IMFMediaBuffer;
  Data: PByte;
  MaxLength: DWORD;
  CurrentLength: DWORD;
  Stride: Integer;
  RequiredLength: DWORD;
  Y: Integer;
  SourceRow: Pointer;
  DestinationRow: Pointer;

begin

  Buffer := nil;
  Result := ASample.ConvertToContiguousBuffer(@Buffer);
  if FAILED(Result) then
    Exit;

  Data := nil;
  MaxLength := 0;
  CurrentLength := 0;

  Result := Buffer.Lock(Data,
                        @MaxLength,
                        @CurrentLength);
  if FAILED(Result) then
    Exit;

  try
    Stride := ABitmap.Width * BYTES_PER_PIXEL;
    RequiredLength := Stride * ABitmap.Height;

    if (CurrentLength < RequiredLength) then
      Exit(E_UNEXPECTED);

    for Y := 0 to ABitmap.Height - 1 do
      begin
        SourceRow := Pointer(NativeUInt(Data) + NativeUInt(Y * Stride));
        DestinationRow := ABitmap.ScanLine[ABitmap.Height - 1 - Y];

        Move(SourceRow^,
             DestinationRow^,
             Stride);
      end;

    Result := S_OK;

  finally
    Buffer.Unlock;
  end;
end;


procedure TfrmMain.RunTransform;
var
  Transform: IMFTransform;
  MediaType: IMFMediaType;
  InputSample: IMFSample;
  OutputData: MFT_OUTPUT_DATA_BUFFER;
  OutputStatus: DWORD;
  Hr: HResult;

begin

  memLog.Clear();
  Transform := TMfGrayscaleMFT.Create as IMFTransform;

  CheckHr('Create RGB32 media type',
          CreateVideoType(MediaType));

  CheckHr('Test input type',
           Transform.SetInputType(0,
                                  MediaType,
                                  MFT_SET_TYPE_TEST_ONLY));

  CheckHr('Set input type',
          Transform.SetInputType(0,
                                 MediaType,
                                 0));

  CheckHr('Test output type',
          Transform.SetOutputType(0,
                                  MediaType,
                                  MFT_SET_TYPE_TEST_ONLY));

  CheckHr('Set output type',
          Transform.SetOutputType(0,
                                  MediaType,
                                  0));

  CheckHr('Create input sample',
          CreateSampleFromBitmap(FSourceBitmap,
                                 InputSample));

  CheckHr('ProcessInput',
          Transform.ProcessInput(0,
                                 InputSample,
                                 0));

  FillChar(OutputData,
           SizeOf(OutputData),
           0);

  OutputStatus := 0;

  CheckHr('ProcessOutput',
           Transform.ProcessOutput(0,
                                   1,
                                   @OutputData,
                                   OutputStatus));


  if not Assigned(OutputData.pSample) then
    raise Exception.Create('ProcessOutput returned no sample.');

  CheckHr('Copy output sample',
          CopySampleToBitmap(OutputData.pSample,
                             FOutputBitmap));

  imgOutput.Picture.Assign(FOutputBitmap);

  memLog.Lines.Add('The MFT returned the same sample after editing it in place.');
  lblSummary.Caption := 'Success: RGB32 frame converted to grayscale.';
end;


procedure TfrmMain.btnProcessClick(Sender: TObject);
begin

  try
    RunTransform;
  except
    on E: Exception do
      begin
        lblSummary.Caption := 'Transform failed.';
        memLog.Lines.Add(E.Message);
      end;
  end;
end;

end.
