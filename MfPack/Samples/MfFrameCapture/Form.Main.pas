// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - MediaFoundation
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module:  Form.Main.pas
// Kind: Pascal Unit
// Release date: 22-09-2021
// Language: ENU
//
// Revision Version: 3.1.7
//
// Description:
//   This unit is the application mainform.
//
// Organisation: FactoryX
// Initiator(s): Ciaran
// Contributor(s): Ciaran, Tony (maXcomX)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 19/06/2024 All                 RammStein release  SDK 10.0.22621.0 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 (2H20) or later.
//
// Related objects: -
// Related projects: MfPackX317/Samples/MFFrameCapture
//
// Compiler version: 23 up to 35
// SDK version: 10.0.22621.0
//
// Todo: -
//
//==============================================================================
// Source: -
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
  {Winapi}
  WinApi.Windows,
  WinApi.ComBaseApi,
  WinApi.ActiveX.ObjBase,
  {System}
  System.TimeSpan,
  System.SysUtils,
  System.Classes,
  {VCL}
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ExtCtrls,
  Vcl.StdCtrls,
  Vcl.ComCtrls,
  Vcl.Samples.Spin,
  Vcl.Menus,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfApi,
  WinApi.MediaFoundationApi.MfUtils,
  {Application}
  FileCapture,
  FileCapture.Asynchronous,
  FileCapture.Synchronous,
  Support;

type

  TInternalTrackBar = class(TTrackbar)
  published
    property OnMouseDown;
    property OnMouseUp;
  end;

  TFrmMain = class(TForm)
    picFrame: TImage;
    pnlTop: TPanel;
    edtVideoFile: TEdit;
    btnBrowse: TButton;
    lblVideo: TLabel;
    memLog: TMemo;
    lblLog: TLabel;
    fdOpenVideo: TOpenDialog;
    btnClearLog: TButton;
    tbVideoPosition: TTrackbar;
    lblCurrentPosition: TLabel;
    btnClose: TButton;
    btnOpen: TButton;
    spAccuracy: TSpinEdit;
    lblMs: TLabel;
    lblAccuracy: TLabel;
    lblFramesToSkip: TLabel;
    spMaxSkipFrames: TSpinEdit;
    pnlFrameCapture: TPanel;
    lblPosition: TLabel;
    cboMethod: TComboBox;
    lblMethod: TLabel;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    mnEdit: TMenuItem;
    mnLogLevel: TMenuItem;
    mnDebugLevel: TMenuItem;
    mnInfoLevel: TMenuItem;
    mnWarningLevel: TMenuItem;
    mnErrorLevel: TMenuItem;
    btnSaveImage: TButton;
    sdSaveFrame: TSaveDialog;
    procedure HandleBrowseClick(Sender: TObject);
    procedure HandleClearLogClick(Sender: TObject);
    procedure HandleFormCreate(Sender: TObject);
    procedure HandleFormDestroy(Sender: TObject);
    procedure HandleTrackbarChange(Sender: TObject);
    procedure HandleCloseVideoClick(Sender: TObject);
    procedure HandleOpenClick(Sender: TObject);
    procedure HandleExitClick(Sender: TObject);
    procedure HandleLogLevelChange(Sender: TObject);
    procedure HandleMethodChanged(Sender: TObject);
    procedure HandleSaveImageClick(Sender: TObject);

  private
    FFormatSettings: TFormatSettings;
    FLogLevel: TLogType;
    FDefaultVideoPath: string;
    FFrequency: int64;
    FCaptureStart: int64;
    FLastFrameTime : TTimeSpan;

    FCapture: TFileCapture;
    FCaptureMethod: TCaptureMethod;

    function OpenSource(const AURL: string): Boolean;
    function GetMemoryUsed: string;

    procedure Log(const AText: string; ALogType: TLogType);
    procedure GetVideoFrame;
    procedure UpdateCapturePositionDisplay;
    procedure CloseSource;
    procedure ClearImage;
    procedure UpdateEnabledStates;
    procedure HandleTrackbarMouseUp(Sender: TObject;
                                    Button: TMouseButton;
                                    Shift: TShiftState; X, Y: Integer);
    procedure BeginBusy;
    procedure EndBusy;
    procedure UpdateLogLevelMenu;
    procedure SetDefaults;
    procedure SetCaptureMethod(const AValue: TCaptureMethod);
    procedure DestroyCapture;
    procedure HandleFrameFound(ABitmap: TBitmap;
                               ATimeStamp: TTimeSpan);
    function GetDefaultSaveName: string;
  public
    property CaptureMethod: TCaptureMethod read FCaptureMethod write SetCaptureMethod;
  end;

var
  FrmMain: TFrmMain;

implementation

uses
  {System}
  System.IOUtils,
  System.Math,
  PngImage,
  Vcl.Imaging.jpeg;

{$r *.dfm}

procedure TFrmMain.HandleFormCreate(Sender: TObject);
begin
  CoInitializeEx(Nil,
                 COINIT_APARTMENTTHREADED);

  FFrequency := 0;
  FCaptureStart := 0;
  FLastFrameTime := TTimeSpan.Zero;

  SetDefaults;

  FFormatSettings := TFormatSettings.Create;

  if FAILED(MFStartup(MF_VERSION, 0)) then
    begin
      MessageBox(0,
                 lpcwstr('Your computer does not support this Media Foundation API version' + IntToStr(MF_VERSION) + '.'),
                 lpcwstr('MFStartup Failure!'),
                 MB_ICONSTOP);
      Application.Terminate;
    end;

  TInternalTrackBar(tbVideoPosition).OnMouseUp := HandleTrackbarMouseUp;
  UpdateLogLevelMenu;

  cboMethod.ItemIndex := Ord(FCaptureMethod);

  if (FDefaultVideoPath <> '') and TFile.Exists(FDefaultVideoPath) then
    OpenSource(FDefaultVideoPath);
end;


procedure TFrmMain.HandleFormDestroy(Sender: TObject);
begin
  CloseSource;
  DestroyCapture;
  MFShutdown();
  CoUnInitialize();
end;


procedure TFrmMain.SetCaptureMethod(const AValue: TCaptureMethod);
begin
  FCaptureMethod := AValue;

  DestroyCapture;

  if FCaptureMethod = cmSync then
    FCapture := TFileCaptureSync.Create
  else
    FCapture := TFileCaptureAsync.Create;

  FCapture.OnFrameFound := HandleFrameFound;
  FCapture.OnLog := Log;
end;


procedure TFrmMain.DestroyCapture;
begin
  if Assigned(FCapture) then
    FreeAndNil(FCapture);
end;


procedure TFrmMain.SetDefaults;
begin
  FLogLevel := ltInfo;
  CaptureMethod := cmASync;

  // Set a default video to open
  FDefaultVideoPath := '';
end;


procedure TFrmMain.HandleMethodChanged(Sender: TObject);
var
  sLastURL: string;

begin
  if Sender is TComboBox then
    begin
      if FCapture.SourceOpen then
        sLastURL := FCapture.URL;

      ClearImage;

      CaptureMethod := TCaptureMethod(TComboBox(Sender).ItemIndex);
      if sLastURL <> '' then
        OpenSource(sLastURL);
    end;
end;


function TFrmMain.OpenSource(const AURL: string): Boolean;
var
  oPreviousRounding: TRoundingMode;

begin
  ClearImage;

  Log(Format('Opening video: "%s"', [AURL]), ltInfo);
  try
    Result := FCapture.OpenSource(AURL);
  finally
    Log(Format('Opened video. Width: %d. Height: %d. Duration: %s. Supports Seek: %s', [FCapture.VideoInfo.iVideoWidth,
                                                                                        FCapture.VideoInfo.iVideoHeight,
                                                                                        TimeSpanToDisplay(FCapture.Duration),
                                                                                        BoolToStr(FCapture.SupportsSeek)]),
                                                                                        ltInfo);
  end;

  if Result then
    begin
      edtVideoFile.Text := FCapture.URL;
      tbVideoPosition.Position := 0;

      oPreviousRounding := GetRoundMode;
    try
      SetRoundMode(rmDown);
      // Set trackbar max range, maybe in frames?
      tbVideoPosition.Max := Round(FCapture.Duration.TotalSeconds);
    finally
      SetRoundMode(oPreviousRounding);
    end;

    UpdateCapturePositionDisplay;
    end;

  UpdateEnabledStates;
end;


procedure TFrmMain.HandleLogLevelChange(Sender: TObject);
begin
  if Sender is TMenuItem then
    begin
      FLogLevel := TLogType(TMenuItem(Sender).Tag);
      UpdateLogLevelMenu;
    end;
end;


procedure TFrmMain.UpdateLogLevelMenu;
var
  i: Integer;

begin
  for i := 0 to mnLogLevel.Count - 1 do
    mnLogLevel.Items[i].Checked := Ord(FLogLevel) = mnLogLevel.Items[i].Tag;
end;


procedure TFrmMain.HandleTrackbarMouseUp(Sender: TObject;
                                         Button: TMouseButton;
                                         Shift: TShiftState; X, Y: Integer);
begin
  GetVideoFrame;
end;


procedure TFrmMain.HandleOpenClick(Sender: TObject);
begin
  ClearImage;
  CloseSource;
  OpenSource(edtVideoFile.Text);
end;


procedure TFrmMain.HandleSaveImageClick(Sender: TObject);
var
  pPng: TPngImage;
  pJpg: TJPEGImage;
  //sFilter: string;

begin
  if not picFrame.Picture.Bitmap.Empty then
    begin
      sdSaveFrame.FileName := GetDefaultSaveName;

     // Tip:
     // Stores a filter with all available image extensions and is compatible with the savedialog filter property
     // sFilter := GraphicFilter(TGraphic);

      if sdSaveFrame.Execute then
        begin
          case sdSaveFrame.FilterIndex of
            {BMP}
            1: begin
                 picFrame.Picture.SaveToFile(sdSaveFrame.FileName);
               end;
            {PNG}
            2: begin
                 pPng := TPngImage.Create;
                 pPng.Assign(picFrame.Picture.Bitmap);
                 pPng.SaveToFile(sdSaveFrame.FileName);

                 if Assigned(pPng) then
                   pPng.Free;
               end;
            {JPG}
            3: begin
                 pJpg := TJPEGImage.Create;
                 // Adjust performance, compression etc.
                 pJpg.Performance := jpBestQuality;
                 pJpg.ProgressiveEncoding := True;
                 pJpg.ProgressiveDisplay := True;
                 //pJpg.CompressionQuality := 30;
                 pJpg.Compress;
                 pJpg.Assign(picFrame.Picture.Bitmap);
                 pjpg.SaveToFile(sdSaveFrame.FileName);

                 if Assigned(pJpg) then
                   pJpg.Free;
               end;
          end;
        end;
    end;
end;


function TFrmMain.GetDefaultSaveName : string;
begin
  Result := 'Capture_' + TimeSpanToDisplay(FLastFrameTime, True).Replace(':', '.');
end;


procedure TFrmMain.HandleBrowseClick(Sender: TObject);
begin
  if fdOpenVideo.Execute then
    OpenSource(fdOpenVideo.Filename);
end;


procedure TFrmMain.GetVideoFrame;
var
  oRequestedFramePosition: TTimeSpan;
begin
  if FCapture.SourceOpen then
    begin
      ClearImage;
      oRequestedFramePosition := TTimeSpan.Create(0,
                                                  0,
                                                  tbVideoPosition.Position);

      Log('Requesting image...',
          ltInfo);
      BeginBusy;
try
      QueryPerformanceFrequency(FFrequency);
      QueryPerformanceCounter(FCaptureStart);

      FCapture.MaxFramesToSkip := spMaxSkipFrames.Value;
      FCapture.Accuracy := spAccuracy.Value;

      FCapture.RequestFrame(oRequestedFramePosition);
finally
      EndBusy;
end;
    end;
end;


procedure TFrmMain.HandleFrameFound(ABitmap: TBitmap;
                                    ATimeStamp: TTimeSpan);
var
  iCaptureEnd: int64;

begin
  QueryPerformanceCounter(iCaptureEnd);
  Log(Format('Image found in %f milliseconds. Time Stamp: %s. Frames Skipped: %d',
             [(iCaptureEnd - FCaptureStart) / FFrequency * 1000,
             TimeSpanToDisplay(ATimeStamp,
                               True),
                               FCapture.FramesSkipped]),
                               ltInfo);

  try
    pnlFrameCapture.Caption := '';
    picFrame.Picture.Bitmap.Assign(ABitmap);
  finally
    FreeAndNil(ABitmap);
  end;

  FLastFrameTime := ATimeStamp;

  UpdateEnabledStates;
end;


procedure TFrmMain.BeginBusy;
begin
  if FCaptureMethod = cmSync then
    Screen.Cursor := crHourGlass;
end;

procedure TFrmMain.EndBusy;
begin
  if FCaptureMethod = cmSync then
    Screen.Cursor := crDefault;
end;


procedure TFrmMain.HandleClearLogClick(Sender: TObject);
begin
  memLog.Lines.Clear;
end;


procedure TFrmMain.HandleCloseVideoClick(Sender: TObject);
begin
  CloseSource;
  ClearImage;
end;


procedure TFrmMain.HandleExitClick(Sender: TObject);
begin
  Application.Terminate;
end;


procedure TFrmMain.HandleTrackbarChange(Sender: TObject);
begin
  UpdateCapturePositionDisplay;
end;


procedure TFrmMain.UpdateCapturePositionDisplay;
begin
  lblCurrentPosition.Caption := TimeSpanToDisplay(TTimeSpan.Create(0,
                                                                   0,
                                                                   tbVideoPosition.Position)) + ' / ' +
                                                                   TimeSpanToDisplay(FCapture.Duration, True);
end;


procedure TFrmMain.Log(const AText: string;
                       ALogType: TLogType);
begin
  if ALogType >= FLogLevel then
    memLog.Lines.Add(FormatDateTime('yyyy/mm/dd HH:mm:ss.zzz',
                                    Now,
                                    FFormatSettings) + cTab + ALogType.AsDisplay + cTab + 'Memory Used: ' + GetMemoryUsed + cTab + AText);
end;

function TFrmMain.GetMemoryUsed: string;
begin
  Result := ToSize(Round(ProcessMemoryUsage / 1024));
end;


procedure TFrmMain.CloseSource;
begin
  FCapture.CloseSource;
  UpdateEnabledStates;
end;


procedure TFrmMain.UpdateEnabledStates;
begin
  tbVideoPosition.Enabled := FCapture.SourceOpen and FCapture.SupportsSeek;

  if not FCapture.SourceOpen then
  begin
    lblCurrentPosition.Caption := 'N/A';
    lblCurrentPosition.Invalidate;
    tbVideoPosition.Min := 1;
    tbVideoPosition.Max := 1;
    tbVideoPosition.Position := 1;
  end;

  btnClose.Enabled := FCapture.SourceOpen;
  btnOpen.Enabled := not FCapture.SourceOpen;
  btnSaveImage.Enabled := Assigned(picFrame.Picture.Bitmap) and not picFrame.Picture.Bitmap.Empty;
end;


procedure TFrmMain.ClearImage;
begin
  pnlFrameCapture.Caption := 'No Image. Waiting frame capture...';
  picFrame.Picture := Nil;
end;

end.
