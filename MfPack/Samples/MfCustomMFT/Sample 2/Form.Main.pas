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
  WinApi.Messages,
  WinApi.WinApiTypes,
  {System}
  System.Classes,
  System.SysUtils,
  {ActiveX}
  WinApi.ActiveX.PropIdl,
  {Vcl}
  Vcl.ComCtrls,
  Vcl.Controls,
  Vcl.Dialogs,
  Vcl.ExtCtrls,
  Vcl.Forms,
  Vcl.Graphics,
  Vcl.StdCtrls,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfApi,
  WinApi.MediaFoundationApi.MfIdl,
  WinApi.MediaFoundationApi.MfObjects,
  WinApi.MediaFoundationApi.MfReadWrite,
  WinApi.MediaFoundationApi.MfTransform;

type
  TfrmMain = class(TForm)
    pnlCommands: TPanel;
    btnOpen: TButton;
    tbPosition: TTrackBar;
    lblPosition: TLabel;
    lblSummary: TLabel;
    pnlSource: TPanel;
    lblSource: TLabel;
    imgSource: TImage;
    pnlOutput: TPanel;
    lblOutput: TLabel;
    imgOutput: TImage;
    memLog: TMemo;
    dlgOpenVideo: TOpenDialog;

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnOpenClick(Sender: TObject);
    procedure tbPositionChange(Sender: TObject);

  private
    FReader: IMFSourceReader;
    FReaderType: IMFMediaType;
    FTransform: IMFTransform;
    FSourceBitmap: TBitmap;
    FOutputBitmap: TBitmap;
    FWidth: UINT32;
    FHeight: UINT32;
    FStride: Integer;
    FDuration: LONGLONG;
    FTrackBarWindowProc: TWndMethod;

    procedure TrackBarWindowProc(var Message: TMessage);
    procedure CheckHr(const AOperation: string; const AHr: HResult);
    procedure CloseVideo();
    procedure ConfigureForCurrentType;
    procedure OpenVideo(const AFileName: string);
    procedure ReadAndTransformFrame(const ARequestedTime: LONGLONG);
    procedure SeekToTrackBarPosition();
    procedure UpdatePositionLabel();
    function CopySampleToBitmap(const ASample: IMFSample;
                                const ABitmap: TBitmap): HResult;
  end;

var
  frmMain: TfrmMain;


implementation

uses
  MfGrayscaleMFT;

{$R *.dfm}

const
  BYTES_PER_PIXEL = 4;

procedure TfrmMain.FormCreate(Sender: TObject);
begin

  FSourceBitmap := TBitmap.Create;
  FOutputBitmap := TBitmap.Create;
  FSourceBitmap.PixelFormat := pf32bit;
  FOutputBitmap.PixelFormat := pf32bit;
  tbPosition.Enabled := False;
  tbPosition.Min := 0;
  tbPosition.Max := 1000;
  FTrackBarWindowProc := tbPosition.WindowProc;
  tbPosition.WindowProc := TrackBarWindowProc;

  dlgOpenVideo.Filter := 'Video files|*.mp4;*.m4v;*.mov;*.wmv;*.avi|All files|*.*';
  memLog.Lines.Add('Open a video, move the trackbar, and release its thumb.');
end;


procedure TfrmMain.FormDestroy(Sender: TObject);
begin

  if Assigned(FTrackBarWindowProc) then
    tbPosition.WindowProc := FTrackBarWindowProc;

  CloseVideo();
  FOutputBitmap.Free;
  FSourceBitmap.Free;
end;


procedure TfrmMain.CheckHr(const AOperation: string; const AHr: HResult);
begin

  if FAILED(AHr) then
    raise Exception.CreateFmt('%s failed: HRESULT 0x%.8x',
                              [AOperation, Cardinal(AHr)]);
end;


procedure TfrmMain.CloseVideo();
begin

  if Assigned(FTransform) then
    begin
      FTransform.ProcessMessage(MFT_MESSAGE_COMMAND_FLUSH, 0);
      FTransform.ProcessMessage(MFT_MESSAGE_NOTIFY_END_OF_STREAM, 0);
      FTransform.ProcessMessage(MFT_MESSAGE_NOTIFY_END_STREAMING, 0);
    end;

  FTransform := nil;
  FReaderType := nil;
  FReader := nil;
  FWidth := 0;
  FHeight := 0;
  FStride := 0;
  FDuration := 0;
  tbPosition.Enabled := False;
  tbPosition.Position := 0;
  UpdatePositionLabel();
end;


procedure TfrmMain.ConfigureForCurrentType;
var
  StrideValue: UINT32;

begin

  FReaderType := nil;
  CheckHr('GetCurrentMediaType',
          FReader.GetCurrentMediaType(MF_SOURCE_READER_FIRST_VIDEO_STREAM,
                                      @FReaderType));
  CheckHr('MFGetAttributeSize',
          MFGetAttributeSize(FReaderType,
                             MF_MT_FRAME_SIZE,
                             FWidth,
                             FHeight));

  FStride := Integer(FWidth) * BYTES_PER_PIXEL;

  if SUCCEEDED(FReaderType.GetUINT32(MF_MT_DEFAULT_STRIDE,
                                     StrideValue)) then
    FStride := Integer(StrideValue);

  if (FStride = 0) then
    FStride := Integer(FWidth) * BYTES_PER_PIXEL;

  FSourceBitmap.SetSize(FWidth,
                        FHeight);

  FOutputBitmap.SetSize(FWidth,
                        FHeight);

  FTransform := TMfGrayscaleMFT.Create as IMFTransform;

  CheckHr('SetInputType',
          FTransform.SetInputType(0,
                                  FReaderType,
                                  0));

  CheckHr('SetOutputType',
          FTransform.SetOutputType(0,
                                   FReaderType,
                                   0));

  CheckHr('Begin streaming',
          FTransform.ProcessMessage(MFT_MESSAGE_NOTIFY_BEGIN_STREAMING,
                                    0));

  CheckHr('Start of stream',
          FTransform.ProcessMessage(MFT_MESSAGE_NOTIFY_START_OF_STREAM,
                                    0));

  memLog.Lines.Add(Format('Decoded format: RGB32, %dx%d, stride %d.',
                          [FWidth, FHeight, FStride]));
end;


procedure TfrmMain.OpenVideo(const AFileName: string);
var
  Attributes: IMFAttributes;
  RequestedType: IMFMediaType;
  DurationValue: PROPVARIANT;

begin

  CloseVideo();
  memLog.Clear();

  CheckHr('MFCreateAttributes',
          MFCreateAttributes(Attributes,
                             1));

  CheckHr('Enable Source Reader video processing',
    Attributes.SetUINT32(MF_SOURCE_READER_ENABLE_VIDEO_PROCESSING,
                         1));

  CheckHr('MFCreateSourceReaderFromURL',
          MFCreateSourceReaderFromURL(PWideChar(AFileName),
                                      Attributes,
                                      FReader));

  CheckHr('Deselect streams',
          FReader.SetStreamSelection(MF_SOURCE_READER_ALL_STREAMS,
                                     False));

  CheckHr('Select first video stream',
          FReader.SetStreamSelection(MF_SOURCE_READER_FIRST_VIDEO_STREAM,
                                     True));

  CheckHr('MFCreateMediaType',
          MFCreateMediaType(RequestedType));

  CheckHr('Set requested major type',
          RequestedType.SetGUID(MF_MT_MAJOR_TYPE,
                                MFMediaType_Video));

  CheckHr('Request decoded RGB32',
          RequestedType.SetGUID(MF_MT_SUBTYPE,
                                MFVideoFormat_RGB32));

  CheckHr('SetCurrentMediaType',
          FReader.SetCurrentMediaType(MF_SOURCE_READER_FIRST_VIDEO_STREAM,
                                      0,
                                      RequestedType));

  ConfigureForCurrentType();

  PropVariantInit(DurationValue);

  try
    CheckHr('Get duration',
            FReader.GetPresentationAttribute(MF_SOURCE_READER_MEDIASOURCE,
                                             MF_PD_DURATION,
                                             DurationValue));

    if (DurationValue.vt <> VT_UI8) then
      raise Exception.Create('The video source did not report a duration.');

    FDuration := DurationValue.hVal.QuadPart;

  finally
    PropVariantClear(DurationValue);
  end;

  if (FDuration <= 0) then
    raise Exception.Create('The video duration is zero.');

  tbPosition.Position := 0;
  tbPosition.Enabled := True;
  UpdatePositionLabel;
  lblSummary.Caption := ExtractFileName(AFileName);

  memLog.Lines.Add(Format('Duration: %.3f seconds.',
                          [FDuration / 10000000.0]));

  memLog.Lines.Add('Release the trackbar thumb to seek and process a frame.');
end;


function TfrmMain.CopySampleToBitmap(const ASample: IMFSample;
                                     const ABitmap: TBitmap): HResult;
var
  Buffer: IMFMediaBuffer;
  Data: PByte;
  MaxLength: DWORD;
  CurrentLength: DWORD;
  RowBytes: Integer;
  MemoryStride: Integer;
  RequiredLength: UInt64;
  Y: Integer;
  SourceRow: Pointer;
  DestinationRow: Pointer;

begin

  Data := nil;
  MaxLength := 0;
  CurrentLength := 0;
  RowBytes := 0;

  Result := Buffer.Lock(Data,
                        @MaxLength,
                        @CurrentLength);
  if FAILED(Result) then
    Exit;

  try
    MemoryStride := FStride;

    if (MemoryStride < 0) then
      MemoryStride := -MemoryStride;

    RequiredLength := UInt64(MemoryStride) * UInt64(FHeight);

    if (MemoryStride < RowBytes) or (UInt64(CurrentLength) < RequiredLength) then
      Exit(E_UNEXPECTED);

    for Y := 0 to Integer(FHeight) - 1 do
      begin
        SourceRow := Pointer(NativeUInt(Data) + NativeUInt(Y * MemoryStride));

        // RGB32 uses a bottom-up buffer for a positive stride and a top-down
        // buffer for a negative stride. TBitmap.ScanLine[0] is the bottom
        // row, so positive stride maps directly and negative stride reverses.
        if (FStride > 0) then
          DestinationRow := ABitmap.ScanLine[Y]
        else
          DestinationRow := ABitmap.ScanLine[Integer(FHeight) - 1 - Y];

        Move(SourceRow^,
             DestinationRow^,
             RowBytes);
      end;

    Result := S_OK;

  finally
    Buffer.Unlock;
  end;
end;


procedure TfrmMain.ReadAndTransformFrame(const ARequestedTime: LONGLONG);
var
  Flags: DWORD;
  TimeStamp: LONGLONG;
  InputSample: IMFSample;
  OutputData: MFT_OUTPUT_DATA_BUFFER;
  OutputStatus: DWORD;
  FramesSkipped: Cardinal;

begin

  FramesSkipped := 0;

  repeat
    Flags := 0;
    TimeStamp := 0;
    InputSample := nil;

    CheckHr('ReadSample',
            FReader.ReadSample(MF_SOURCE_READER_FIRST_VIDEO_STREAM,
                               0,
                               nil,
                               @Flags,
                               @TimeStamp,
                               @InputSample));

    if ((Flags and MF_SOURCE_READERF_ERROR) <> 0) then
      raise Exception.Create('The Source Reader reported a streaming error.');

    if (Flags and MF_SOURCE_READERF_CURRENTMEDIATYPECHANGED) <> 0 then
      begin
        memLog.Lines.Add('The decoded media type changed. Reconfiguring.');
        ConfigureForCurrentType;
      end;

    if (Flags and (MF_SOURCE_READERF_ENDOFSTREAM) <> 0) then
      begin
        lblSummary.Caption := 'End of video.';
        memLog.Lines.Add('End of stream.');
        Exit;
      end;

    if Assigned(InputSample) and (TimeStamp < ARequestedTime) then
      begin
        Inc(FramesSkipped);
        InputSample := nil;
      end;
  until Assigned(InputSample);

  // Copy before ProcessInput: this is an in-place transform.
  CheckHr('Copy original frame',
          CopySampleToBitmap(InputSample,
                             FSourceBitmap));

  imgSource.Picture.Assign(FSourceBitmap);

  CheckHr('ProcessInput',
          FTransform.ProcessInput(0,
                                  InputSample,
                                  0));

  FillChar(OutputData,
           SizeOf(OutputData),
           0);
  OutputStatus := 0;

  CheckHr('ProcessOutput',
          FTransform.ProcessOutput(0,
                                   1,
                                   @OutputData,
                                   OutputStatus));

  if not Assigned(OutputData.pSample) then
    raise Exception.Create('ProcessOutput returned no sample.');

  CheckHr('Copy grayscale frame',
          CopySampleToBitmap(OutputData.pSample,
                             FOutputBitmap));
  imgOutput.Picture.Assign(FOutputBitmap);

  lblSummary.Caption := Format('Frame at %.3f seconds',
    [TimeStamp / 10000000.0]);

  memLog.Lines.Add(Format('Frame at %.3f s: Source Reader -> ProcessInput -> ProcessOutput ' +
                          '(%d earlier frames skipped).',
    [TimeStamp / 10000000.0, FramesSkipped]));
end;


procedure TfrmMain.UpdatePositionLabel();
var
  RequestedTime: LONGLONG;

begin

  RequestedTime := 0;

  if (FDuration > 0) and (tbPosition.Max > 0) then
    RequestedTime := ((FDuration - 1) * tbPosition.Position) div tbPosition.Max;

  lblPosition.Caption := Format('%.3f / %.3f s',
                                [RequestedTime / 10000000.0, FDuration / 10000000.0]);
end;


procedure TfrmMain.SeekToTrackBarPosition();
var
  RequestedTime: LONGLONG;
  PositionValue: PROPVARIANT;

begin

  if not Assigned(FReader) or (FDuration <= 0) then
    Exit;

  RequestedTime := ((FDuration - 1) * tbPosition.Position) div tbPosition.Max;
  if Assigned(FTransform) then
    CheckHr('Flush transform',
            FTransform.ProcessMessage(MFT_MESSAGE_COMMAND_FLUSH,
                                      0));

  PropVariantInit(PositionValue);

  try
    PositionValue.vt := VT_I8;
    PositionValue.hVal.QuadPart := RequestedTime;

    CheckHr('SetCurrentPosition',
            FReader.SetCurrentPosition(GUID_NULL,
                                       PositionValue));

  finally
    PropVariantClear(PositionValue);
  end;

  ReadAndTransformFrame(RequestedTime);
end;


procedure TfrmMain.btnOpenClick(Sender: TObject);
begin

  if not dlgOpenVideo.Execute then
    Exit;

  try
    OpenVideo(dlgOpenVideo.FileName);
  except
    on E: Exception do
      begin
        CloseVideo;
        lblSummary.Caption := 'Could not open the video.';
        memLog.Lines.Add(E.Message);
      end;
  end;
end;


procedure TfrmMain.tbPositionChange(Sender: TObject);
begin

  UpdatePositionLabel();
end;


procedure TfrmMain.TrackBarWindowProc(var message: TMessage);
begin

  FTrackBarWindowProc(message);

  if (message.Msg <> WM_LBUTTONUP) then
    Exit;

  try
    SeekToTrackBarPosition();
  except
    on E: Exception do
      begin
        if Assigned(FTransform) then
          FTransform.ProcessMessage(MFT_MESSAGE_COMMAND_FLUSH,
                                    0);

        lblSummary.Caption := 'Frame processing failed.';
        memLog.Lines.Add(E.Message);
      end;
  end;
end;

end.
