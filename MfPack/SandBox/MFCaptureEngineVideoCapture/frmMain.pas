unit frmMain;

interface

uses
  {WinApi}
  Winapi.Windows,
  Winapi.Messages,
  WinApi.ComBaseApi,
  Winapi.ShlObj,
  Winapi.KnownFolders,
  WinApi.WinApiTypes,
  Winapi.CommCtrl,
  WinApi.WinError,
  {System}
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.Services.Dbt,
  {VCL}
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.Menus,
  Vcl.AppEvnts,
  Vcl.ExtCtrls,
  Vcl.ComCtrls,
  Vcl.StdCtrls,
  {ActiveX}
  WinApi.ActiveX.ObjBase,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfApi,
  WinApi.MediaFoundationApi.MfIdl,
  WinApi.MediaFoundationApi.MfObjects,
  WinApi.MediaFoundationApi.MfUtils,
  WinApi.MediaFoundationApi.MfError,
  WinApi.MediaFoundationApi.MfMetLib,
  {Application}
  CaptureEngine,
  SampleConverter_V1,
  dlgChooseDevice,
  DeviceExplorer,
  Utils;

{$DEFINE LOGDEBUG}

type
  TMainWindow = class(TForm)
    MainMenu: TMainMenu;
    Capture1: TMenuItem;
    mnuStartPreview: TMenuItem;
    mnuChooseDevice: TMenuItem;
    mnuStartRecording: TMenuItem;
    mnuTakePhoto: TMenuItem;
    SaveFileDlg: TSaveDialog;
    Bevel1: TBevel;
    N1: TMenuItem;
    Exit1: TMenuItem;
    StatusBar: TStatusBar;
    pnlPreview: TPanel;
    pnlSnapShot: TPanel;
    pbCapture: TPaintBox;
    butSaveToFile: TButton;
    Bevel2: TBevel;
    butTakePhoto: TButton;
    procedure FormCreate(Sender: TObject);
    procedure mnuChooseDeviceClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure mnuStartPreviewClick(Sender: TObject);
    procedure mnuTakePhotoClick(Sender: TObject);
    procedure butTakePhotoClick(Sender: TObject);
    procedure mnuStartRecordingClick(Sender: TObject);

  private
    { Private declarations }
    hPreview: HWND;
    hCaptureHandle: HDC;
    hStatus: HWND;
    bRecording: Boolean;
    bPreviewing: Boolean;
    bImageCleared: Boolean;
    bmLastCapturedFrame: TBitmap;
    pSelectedDevice: IMFActivate;
    iSelectedDevice: Integer;
    iSelectedFormat: Integer;
    FMemoryStream: TMemoryStream;

    function CreateDeviceExplorer(): HResult;

    // Messages
    procedure OnSize(var message: TWMSize); message WM_SIZE;
    procedure OnDeviceChange(var AMessage: TMessage); message WM_DEVICECHANGE;

    // Update menuitems and status
    procedure UpdateUI();
    // process sample in main thread
    function ProcessSample(aSample: IMFSample): HResult;
    procedure PaintLastCapture();
    procedure GetPaintArea(AImage: TBitmap;
                           var AWidth: Integer;
                           var AHeight: Integer;
                           var ATop: Integer;
                           var ALeft: Integer);
  public
    { Public declarations }
    procedure WndProc(var Msg: TMessage); override;

  end;

var
  MainWindow: TMainWindow;


implementation

{$R *.dfm}


// Messages

// OnSize
procedure TMainWindow.OnSize(var message: TWMSize);
var
  crD: MFVideoNormalizedRect;
  pcrD: PMFVideoNormalizedRect;
  statusRect: TRECT;
  iCX: Integer;
  iCY: Integer;

begin
  if not Visible then
    Exit;

  if (message.SizeType = SIZE_RESTORED) or (message.SizeType = SIZE_MAXIMIZED) then
    begin
      // Resize the status bar.
      hStatus := StatusBar.Handle;
      SendMessageW(hStatus,
                   WM_SIZE,
                   0,
                   0);

      // Resize the preview window.
      SendMessageW(hStatus,
                   SB_GETRECT,
                   0,
                   LPARAM(@statusRect));

      iCX := message.Width;
      iCY := message.Height;

      iCY := iCY - (statusRect.bottom - statusRect.top);
      MoveWindow(hPreview,
                 0,
                 0,
                 iCX,
                 iCY,
                 True);
     end;  // Set video size

  if Assigned(FCaptureManager) then
    begin
      crD.left := 0;
      crD.top := 0;
      crD.right := pbCapture.ClientWidth;
      crD.bottom := pbCapture.ClientHeight;
      pcrD := @crD;

      FCaptureManager.UpdateVideo(pcrD);
    end;

  inherited;
end;


//------------------------------------------------------------------------------
//  OnDeviceChange
//
//  Handles WM_DEVICECHANGE messages. (see messagehook method)
//------------------------------------------------------------------------------
procedure TMainWindow.OnDeviceChange(var AMessage: TMessage);
var
  bDeviceLost: BOOL;
  PDevBroadcastHeader: PDEV_BROADCAST_HDR;
  pDevBroadCastIntf: PDEV_BROADCAST_DEVICEINTERFACE;
  pwDevSymbolicLink: PWideChar;

begin

  if not Assigned(FCaptureManager) then
    Exit;

  bDeviceLost := False;

  // Check if the current device was lost.
  if (AMessage.WParam = DBT_DEVICEREMOVECOMPLETE) then
    begin
      // Check if the current video capture device was lost.

      if PDEV_BROADCAST_HDR(AMessage.LParam).dbch_devicetype <> DBT_DEVTYP_DEVICEINTERFACE then
        Exit;

      // Get the symboliclink of the lost device and check.
      PDevBroadcastHeader := PDEV_BROADCAST_HDR(AMessage.LParam);
      pDevBroadCastIntf := PDEV_BROADCAST_DEVICEINTERFACE(PDevBroadcastHeader);
      // Note: Since Windows 8 the value of dbcc_name is no longer the devicename, but the symboliclink of the device.
      pwDevSymbolicLink := PChar(@pDevBroadCastIntf^.dbcc_name);
      bDeviceLost := False;
      if StrIComp(PWideChar(FDeviceExplorer.DeviceSymbolicLink),
                  PWideChar(pwDevSymbolicLink)) = 0 then
        bDeviceLost := True;
    end;

  if (bDeviceLost = True) then
    begin
      SafeRelease(FCaptureManager);
      ErrMsg(Format('Lost capture device %s.' + #13 + 'Please select another device or reconnect.', [FDeviceExplorer.DeviceDisplayName]),
             DBT_DEVICEREMOVECOMPLETE);
    end;
end;


// WndProc
procedure TMainWindow.WndProc(var Msg: TMessage);
var
  psample: IMFSample;
  hr: HResult;

begin

  case Msg.Msg of

    // We did recieve a sample from TCaptureManager.TCaptureEngineOnSampleCallback.OnSample
    WM_RECIEVED_SAMPLE_FROM_CALLBACK:
      begin
        psample := IMFSample(Pointer(Msg.WParam));

        // process the sample
        ProcessSample(pSample);
      end;

    WM_APP_CAPTURE_EVENT:
      begin
        hr := FCaptureManager.OnCaptureEvent(Msg.WParam,
                                             Msg.LParam);
        WaitForSingleObject(Handle,
                            INFINITE);

      end;


    // Any other messages are passed to DefWindowProc, which tells Windows to handle the message.
    else
      msg.Result := DefWindowProc(Handle,
                                  Msg.Msg,
                                  Msg.WParam,
                                  Msg.LParam);
  end;

  inherited WndProc(Msg);

end;


// ProcessSample
function TMainWindow.ProcessSample(aSample: IMFSample): HResult;
var
  hr: HResult;

begin
  hr := FCaptureManager.FSampleConverter.UpdateConverter();

  hr := FCaptureManager.FSampleConverter.DataFromSample(aSample,
                                                        FDeviceExplorer.DeviceProperties[FDeviceExplorer.DeviceIndex].aVideoFormats[0],
                                                        FMemoryStream);

  // stream returned, let's assign to preview

  Result := hr;
end;


procedure TMainWindow.PaintLastCapture();
var
  iWidth: Integer;
  iHeight: Integer;
  iTop: Integer;
  iLeft: Integer;
  iTimerStart: int64;
  iTimerEnd: int64;

begin
  if not bImageCleared and Assigned(bmLastCapturedFrame) and not bmLastCapturedFrame.Empty then
    begin

      SetStretchBltMode(pbCapture.Canvas.Handle,HALFTONE);
      SetBrushOrgEx(pbCapture.Canvas.Handle,
                    0,
                    0,
                    nil);

      // Fill the background
      //if FDisplayingMessage then
      //  begin
      //    pbCapture.Canvas.Brush.Color := clBtnFace;
      //    pbCapture.Canvas.FillRect(pbCapture.BoundsRect);
      //
      //    FDisplayingMessage := False;
      //  end;

      // Scale and center the image
      GetPaintArea(bmLastCapturedFrame,
                   iWidth,
                   iHeight,
                   iTop,
                   iLeft);

        // Stretch draw the whole image
        StretchBlt(pbCapture.Canvas.Handle,
                   iLeft,
                   iTop,
                   iWidth,
                   iHeight,
                   bmLastCapturedFrame.Canvas.Handle,
                   0,
                   0,
                   bmLastCapturedFrame.Width,
                   bmLastCapturedFrame.Height,
                   SRCCOPY);
    end;
end;


procedure TMainWindow.GetPaintArea(AImage: TBitmap;
                                   var AWidth: Integer;
                                   var AHeight: Integer;
                                   var ATop: Integer;
                                   var ALeft: Integer);
var
  iRatio: Double;
  iHeightRatio: Double;
  iWidthRatio: Double;

begin
  iHeightRatio := pbCapture.Height / AImage.Height;
  iWidthRatio := pbCapture.Width / AImage.Width;
  if iHeightRatio > iWidthRatio then
    iRatio := Min(1,
                  iWidthRatio)
  else
    iRatio := Min(1,
                  iHeightRatio);

  AWidth := Round(AImage.Width * iRatio);
  AHeight := Round(AImage.Height * iRatio);
  ATop := (pbCapture.Height - AHeight) div 2;
  ALeft := (pbCapture.Width - AWidth) div 2;
end;


// butTakePhotoClick
procedure TMainWindow.butTakePhotoClick(Sender: TObject);
begin
  mnuTakePhotoClick(Self);
end;


// FormCloseQuery
procedure TMainWindow.FormCloseQuery(Sender: TObject;
                                     var CanClose: Boolean);
begin
  CanClose := False;
  SafeRelease(pSelectedDevice);
  FreeAndNil(FDeviceExplorer);
  SafeDelete(FCaptureManager);
  PostQuitMessage(0);
  MFShutdown();
  CoUnInitialize();
  CanClose := True;
end;


// FormCreate
procedure TMainWindow.FormCreate(Sender: TObject);
var
  hr: HResult;

label
  Done;

begin
  iSelectedDevice := -1;
  iSelectedFormat := -1;

  // Initialize COM
  CoInitializeEx(nil,
                 COINIT_APARTMENTTHREADED);
  // Startup Media Foundation
  if FAILED(MFStartup(MF_VERSION,
                      0)) then
    begin
      MessageBox(0,
                 lpcwstr('Your computer does not support this Media Foundation API version' + IntToStr(MF_VERSION) + '.'),
                 lpcwstr('MFStartup Failure!'),
                 MB_ICONSTOP);
      goto Done;
   end;

  // Initialize the deviceexlorer and captureengine

  hPreview := pnlPreview.Handle;
  hCaptureHandle := pbCapture.Canvas.Handle;

  // Create DeviceExplorer properties of the capture devices.
  hr := CreateDeviceExplorer();
  if FAILED(hr) then
    goto Done;

  // Create the capture engine
  hr := TCaptureManager.CreateCaptureEngine(Handle,
                                            FCaptureManager);
  if FAILED(hr) then
    begin
      ErrMsg('AfterConstruction: Can not create CaptureManager. The application will be closed.',
             hr);

      goto Done;
    end;

  UpdateUI();

Done:
  if FAILED(hr) then
    Application.Terminate;
end;


// CreateDeviceExplorer
// Callers: FormCreate
function TMainWindow.CreateDeviceExplorer(): HResult;
var
  hr: HResult;

label
  Done;

begin

  // Destroy and Create DeviceParam class that holds the Activate pointers and other properties of the selected device.
  if Assigned(FDeviceExplorer) then
    SafeDelete(FDeviceExplorer);

  FDeviceExplorer := TDeviceExplorer.Create(hr);

  if FAILED(hr) then
    goto Done;

  // check for valid capture device(s)
  if (FDeviceExplorer.DevicesCount = 0) then
    begin
      hr := MF_E_NO_CAPTURE_DEVICES_AVAILABLE;
      ShowMessage(format('No capture devices found on this system (hr = %d)',
                         [hr]));
      goto Done;
    end;
Done:
  Result := hr;
end;


// mnuChooseDeviceClick
procedure TMainWindow.mnuChooseDeviceClick(Sender: TObject);
var
  hr: HResult;
  count: Integer;

label
  Done;

begin
  hr := S_OK;
  // Create DeviceExplorer properties of the capture devices.
  if not Assigned(FDeviceExplorer) then
    hr := CreateDeviceExplorer();
  if FAILED(hr) then
    goto Done;

  if Assigned(FCaptureManager) then
    begin
      if FCaptureManager.IsPreviewing then
        FCaptureManager.StopPreview();
      if FCaptureManager.IsRecording then
        FCaptureManager.StopRecording();
    end;

  iSelectedDevice := -1;
  iSelectedFormat := -1;
  UpdateUI();

  // Createe the dialog if it's not allready done.
  if not Assigned(ChooseDeviceDlg) then
    begin
      Application.CreateForm(TChooseDeviceDlg,
                             ChooseDeviceDlg);
      ChooseDeviceDlg.Visible := False;
    end;

  // Ask the user to select one.
  if ChooseDeviceDlg.ShowModal = 1212 then
    begin

      iSelectedDevice := ChooseDeviceDlg.SelectedDevice;
      iSelectedFormat := ChooseDeviceDlg.SelectedFormat;
      hPreview := pnlPreview.Handle;
      SafeRelease(pSelectedDevice);

      // Set DeviceParam properties
      hr := FDeviceExplorer.SetCurrentDeviceProperties(iSelectedDevice,
                                                       iSelectedFormat,
                                                       False);
      if FAILED(hr) then
        goto Done;

      pSelectedDevice := FDeviceExplorer.DeviceActivationObject;

      hr := FCaptureManager.InitializeCaptureManager(hPreview,
                                                     Handle,
                                                     IUnknown(pSelectedDevice));
      if FAILED(hr) then
        goto Done;
    end
  else
    begin
      // User canceled device selection.
      hr := S_OK;
    end;

Done:
  if FAILED(hr) then
    begin
      ErrMsg(ERR_INITIALIZE + ' The application will be closed.',
             hr);
      Application.Terminate();
    end;
  UpdateUI();
end;


// UpdateUI
procedure TMainWindow.UpdateUI();
var
  bEnableRecording: Boolean;
  bEnablePhoto: Boolean;
  bEnablePreview: Boolean;

begin
  bEnablePhoto := False;
  bEnableRecording := False;
  bEnablePreview := ((iSelectedDevice > -1) and (iSelectedFormat > -1));


  if not Assigned(FCaptureManager) then
    begin
      StatusBar.SimpleText := 'Please select a device.';
      Exit;
    end;

  if not FCaptureManager.IsRecording then
    begin
      bRecording := FCaptureManager.IsRecording;

      if bRecording then
        mnuStartRecording.Caption := 'Stop Recording'
      else
        mnuStartRecording.Caption := 'Start Recording';
    end;

  if FCaptureManager.IsPreviewing then
    begin
      bPreviewing := FCaptureManager.IsPreviewing;
      if bPreviewing then
        begin
          mnuStartPreview.Caption := 'Stop Preview';
          bEnableRecording := True;
          bEnablePhoto := True;
        end
      else
        begin
          mnuStartPreview.Caption := 'Start Preview';
        end;
    end;

  if bRecording then
    StatusBar.SimpleText := 'Recording'
  else if FCaptureManager.IsPreviewing then
    StatusBar.SimpleText := 'Previewing'
  else
    begin
      StatusBar.SimpleText := 'Please select a device.';
      bEnableRecording := False;

    end;

  if (FCaptureManager.IsPreviewing or FCaptureManager.IsPhotoPending) then
    bEnablePhoto := False;

  mnuStartRecording.Enabled := bEnableRecording;
  mnuTakePhoto.Enabled := bEnablePhoto;
  mnuStartPreview.Enabled := bEnablePreview;
end;


// mnuStartPreviewClick
procedure TMainWindow.mnuStartPreviewClick(Sender: TObject);
var
  hr: HResult;

begin
  if Assigned(FCaptureManager) then
    begin

      hr := FCaptureManager.StartPreview();
      if FAILED(hr) then
        begin
          ErrMsg('mnuStartPreviewClick ' + ERR_PREVIEW,
                 hr);
          Exit;
        end;
      UpdateUI();
    end;
end;


// mnuStartRecordingClick
procedure TMainWindow.mnuStartRecordingClick(Sender: TObject);
begin
  FCaptureManager.StartRecording(nil);
end;


// mnuTakePhotoClick
procedure TMainWindow.mnuTakePhotoClick(Sender: TObject);
var
  hr: HResult;

label
  Done;

begin
  hr := E_FAIL;

  if Assigned(FCaptureManager) then
    hr := FCaptureManager.TakePhoto(ssoCallBack);
  if FAILED(hr) then
    goto Done;

Done:
  if FAILED(hr) then
    ErrMsg('mnuTakePhotoClick: ERR_PHOTO',
            hr)
  else
    UpdateUI();
end;


end.
