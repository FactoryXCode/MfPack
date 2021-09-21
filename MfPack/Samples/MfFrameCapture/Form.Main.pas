// Version 2.0
unit Form.Main;

interface

uses
  {Winapi}
  Winapi.ComBaseApi,
  Winapi.Windows,
  Winapi.ActiveX.ObjBase,
  WinApi.MediaFoundationApi.MfApi,
  Winapi.MediaFoundationApi.MfUtils,
  {System}
  System.TimeSpan,
  System.SysUtils,
  System.Variants,
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
    picFrame : TImage;
    pnlTop : TPanel;
    edtVideoFile : TEdit;
    btnBrowse : TButton;
    lblVideo : TLabel;
    memLog : TMemo;
    lblLog : TLabel;
    fdOpenVideo : TOpenDialog;
    btnClearLog : TButton;
    tbVideoPosition : TTrackbar;
    lblCurrentPosition : TLabel;
    btnClose : TButton;
    btnOpen : TButton;
    spAccuracy : TSpinEdit;
    lblMs : TLabel;
    lblAccuracy : TLabel;
    lblFramesToSkip : TLabel;
    spMaxSkipFrames : TSpinEdit;
    pnlFrameCapture : TPanel;
    lblPosition : TLabel;
    cboMethod : TComboBox;
    lblMethod : TLabel;
    MainMenu1 : TMainMenu;
    File1 : TMenuItem;
    mnEdit : TMenuItem;
    mnLogLevel : TMenuItem;
    mnDebugLevel : TMenuItem;
    mnInfoLevel : TMenuItem;
    mnWarningLevel : TMenuItem;
    mnErrorLevel : TMenuItem;
    procedure HandleBrowseClick(Sender : TObject);
    procedure HandleClearLogClick(Sender : TObject);
    procedure HandleFormCreate(Sender : TObject);
    procedure HandleFormDestroy(Sender : TObject);
    procedure HandleTrackbarChange(Sender : TObject);
    procedure HandleCloseVideoClick(Sender : TObject);
    procedure HandleOpenClick(Sender : TObject);
    procedure HandleExitClick(Sender : TObject);
    procedure HandleLogLevelChange(Sender : TObject);
    procedure HandleMethodChanged(Sender : TObject);
  private
    FFormatSettings : TFormatSettings;
    FLogLevel : TLogType;
    FDefaultVideoPath : string;
    FMethod : TCaptureMethod;

    FFrequency : int64;
    FCaptureStart : int64;

    FCapture : TFileCapture;
    FCaptureMethod : TCaptureMethod;

    function OpenSource(const AURL : string) : Boolean;
    function GetMemoryUsed : string;

    procedure Log(const AText : string; ALogType : TLogType);
    procedure GetVideoFrame;
    procedure UpdateCapturePositionDisplay;
    procedure CloseSource;
    procedure ClearImage;
    procedure UpdateEnabledStates;
    procedure HandleTrackbarMouseUp(Sender : TObject; Button : TMouseButton; Shift : TShiftState; X, Y : Integer);
    procedure BeginBusy;
    procedure EndBusy;
    procedure UpdateLogLevelMenu;
    procedure SetDefaults;
    procedure SetCaptureMethod(const AValue : TCaptureMethod);
    procedure DestroyCapture;
    procedure HandleFrameFound(ABitmap : Vcl.Graphics.TBitmap; ATimeStamp : TTimeSpan);
  public
    property CaptureMethod : TCaptureMethod read FCaptureMethod write SetCaptureMethod;
  end;

var
  FrmMain : TFrmMain;

implementation

uses
  System.IOUtils, System.Types, System.Math;

{$r *.dfm}

procedure TFrmMain.HandleFormCreate(Sender : TObject);
begin
  CoInitializeEx(nil, COINIT_APARTMENTTHREADED);

  FFrequency := 0;
  FCaptureStart := 0;

  SetDefaults;

  FFormatSettings := TFormatSettings.Create;

  if FAILED(MFStartup(MF_VERSION, 0)) then
  begin
    MessageBox(0, lpcwstr('Your computer does not support this Media Foundation API version' + IntToStr(MF_VERSION) + '.'),
      lpcwstr('MFStartup Failure!'), MB_ICONSTOP);
    Application.Terminate;
  end;

  TInternalTrackBar(tbVideoPosition).OnMouseUp := HandleTrackbarMouseUp;
  UpdateLogLevelMenu;

  cboMethod.ItemIndex := Ord(FMethod);

  if (FDefaultVideoPath <> '') and TFile.Exists(FDefaultVideoPath) then
    OpenSource(FDefaultVideoPath);
end;

procedure TFrmMain.HandleFormDestroy(Sender : TObject);
begin
  CloseSource;

  DestroyCapture;

  MFShutdown();
  CoUnInitialize();
end;

procedure TFrmMain.SetCaptureMethod(const AValue : TCaptureMethod);
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
  begin
    FCapture.Free;
    FCapture := nil;
  end;
end;

procedure TFrmMain.SetDefaults;
begin
  FLogLevel := ltInfo;
  CaptureMethod := cmSync;

  // Set a default video to open
  FDefaultVideoPath := '';
end;

procedure TFrmMain.HandleMethodChanged(Sender : TObject);
var
  sLastURL : string;
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

function TFrmMain.OpenSource(const AURL : string) : Boolean;
var
  oPreviousRounding : TRoundingMode;
begin
  ClearImage;

  Log(Format('Opening video: "%s"', [AURL]), ltInfo);
  try
    Result := FCapture.OpenSource(AURL);
  finally
    Log(Format('Opened video. Width: %d. Height: %d. Duration: %s. Supports Seek: %s', [FCapture.VideoInfo.iVideoWidth,
      FCapture.VideoInfo.iVideoHeight, TimeSpanToDisplay(FCapture.Duration), BoolToStr(FCapture.SupportsSeek, True)]), ltInfo);
  end;

  if Result then
  begin
    edtVideoFile.Text := FCapture.URL;
    tbVideoPosition.Position := 0;

    oPreviousRounding := GetRoundMode;
    try
      SetRoundMode(rmDown);
      tbVideoPosition.Max := Round(FCapture.Duration.TotalSeconds);
    finally
      SetRoundMode(oPreviousRounding);
    end;

    UpdateCapturePositionDisplay;
  end;

  UpdateEnabledStates;
end;

procedure TFrmMain.HandleLogLevelChange(Sender : TObject);
begin
  if Sender is TMenuItem then
  begin
    FLogLevel := TLogType(TMenuItem(Sender).Tag);
    UpdateLogLevelMenu;
  end;
end;

procedure TFrmMain.UpdateLogLevelMenu;
var
  i : Integer;
begin
  for i := 0 to mnLogLevel.Count - 1 do
    mnLogLevel.Items[i].Checked := Ord(FLogLevel) = mnLogLevel.Items[i].Tag;
end;

procedure TFrmMain.HandleTrackbarMouseUp(Sender : TObject; Button : TMouseButton; Shift : TShiftState; X, Y : Integer);
begin
  GetVideoFrame;
end;

procedure TFrmMain.HandleOpenClick(Sender : TObject);
begin
  ClearImage;
  CloseSource;
  OpenSource(edtVideoFile.Text);
end;

procedure TFrmMain.HandleBrowseClick(Sender : TObject);
begin
  if fdOpenVideo.Execute then
    OpenSource(fdOpenVideo.Filename);
end;

procedure TFrmMain.GetVideoFrame;
var
  oRequestedFramePosition : TTimeSpan;
begin
  if FCapture.SourceOpen then
  begin
    ClearImage;
    oRequestedFramePosition := TTimeSpan.Create(0, 0, tbVideoPosition.Position);

    FCapture.SetPosition(oRequestedFramePosition);
    Log('Requesting image...', ltInfo);

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

procedure TFrmMain.HandleFrameFound(ABitmap : Vcl.Graphics.TBitmap; ATimeStamp : TTimeSpan);
var
  iCaptureEnd : int64;
begin
  QueryPerformanceCounter(iCaptureEnd);
  Log(Format('Image found in %f milliseconds. Time Stamp: %s. Frames Skipped: %d', [(iCaptureEnd - FCaptureStart) / FFrequency * 1000,
    TimeSpanToDisplay(ATimeStamp, True), FCapture.FramesSkipped]), ltInfo);

  try
    pnlFrameCapture.Caption := '';
    picFrame.Picture.Bitmap.Assign(ABitmap);
  finally
    FreeAndNil(ABitmap);
  end;
end;

procedure TFrmMain.BeginBusy;
begin
  if FMethod = cmSync then
    Screen.Cursor := crHourGlass;
end;

procedure TFrmMain.EndBusy;
begin
  if FMethod = cmSync then
    Screen.Cursor := crDefault;
end;

procedure TFrmMain.HandleClearLogClick(Sender : TObject);
begin
  memLog.Lines.Clear;
end;

procedure TFrmMain.HandleCloseVideoClick(Sender : TObject);
begin
  CloseSource;
  ClearImage;
end;

procedure TFrmMain.HandleExitClick(Sender : TObject);
begin
  Application.Terminate;
end;

procedure TFrmMain.HandleTrackbarChange(Sender : TObject);
begin
  UpdateCapturePositionDisplay;
end;

procedure TFrmMain.UpdateCapturePositionDisplay;
begin
  lblCurrentPosition.Caption := TimeSpanToDisplay(TTimeSpan.Create(0, 0, tbVideoPosition.Position)) + ' / ' +
    TimeSpanToDisplay(FCapture.Duration);
end;

procedure TFrmMain.Log(const AText : string; ALogType : TLogType);
begin
  if ALogType >= FLogLevel then
    memLog.Lines.Add(FormatDateTime('yyyy/mm/dd HH:mm:ss.zzz', Now, FFormatSettings) + cTab + ALogType.AsDisplay + cTab + 'Memory Used: ' +
      GetMemoryUsed + cTab + AText);
end;

function TFrmMain.GetMemoryUsed : string;
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
end;

procedure TFrmMain.ClearImage;
begin
  pnlFrameCapture.Caption := 'No Image. Waiting frame capture...';
  picFrame.Picture := nil;
end;

end.
