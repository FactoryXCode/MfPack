// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - MediaFoundation
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: frmMain.pas
// Kind: Pascal Unit
// Release date: 18-11-2022
// Language: ENU
//
// Revision Version: 3.1.4
//
// Description:
//   Main form.
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX)
// Contributor(s): Ciaran, Tony (maXcomX)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 28/08/2022 All                 PiL release  SDK 10.0.22621.0 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 (2H20) or later.
//
// Related objects: -
// Related projects: MfPackX314/Samples/MFCaptureEngineVideoCapture
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
  System.Math,
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
  WinApi.MediaFoundationApi.MfCaptureEngine,
  {Application}
  CaptureEngine,
  SampleConverter,
  dlgChooseDevice,
  DeviceExplorer,
  Utils;

type
  TMainWindow = class(TForm)
    MainMenu: TMainMenu;
    Capture1: TMenuItem;
    mnuStartPreview: TMenuItem;
    mnuChooseDevice: TMenuItem;
    mnuStartRecording: TMenuItem;
    SaveFileDlg: TSaveDialog;
    N1: TMenuItem;
    Exit1: TMenuItem;
    pnlPreview: TPanel;
    pnlSnapShot: TPanel;
    pbCapture: TPaintBox;
    pnlInfo: TPanel;
    pnlControls: TPanel;
    butSaveToFile: TButton;
    butTakePhoto: TButton;
    procedure FormCreate(Sender: TObject);
    procedure mnuChooseDeviceClick(Sender: TObject);
    procedure mnuStartPreviewClick(Sender: TObject);
    procedure butTakePhotoClick(Sender: TObject);
    procedure mnuStartRecordingClick(Sender: TObject);
    procedure butSaveToFileClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure Exit1Click(Sender: TObject);

  private
    { Private declarations }
    hPreview: HWND;
    hCaptureHandle: HDC;
    bRecording: Boolean;
    bPreviewing: Boolean;
    bImageCleared: Boolean;
    bmCapturedFrame: TMfpBitmap;
    pSelectedDevice: IMFActivate;
    iSelectedDevice: Integer;
    iSelectedFormat: Integer;
    iSampleTime: Int64;
    FSampleConverter: TSampleConverter;

    function CreateCaptureObjects(): HResult;
    procedure DestroyCaptureObjects();
    function CreateDeviceExplorer(): HResult;

    // Windows messages
    procedure OnSize(var message: TWMSize); message WM_SIZE;
    procedure OnDeviceChange(var AMessage: TMessage); message WM_DEVICECHANGE;

    // Captureengine message handlers
    // These methods handle messages  from the CptureEngine.
    procedure OnCaptureEvent(var AMessage: TMessage); message WM_APP_CAPTURE_EVENT;
    procedure OnRecievedSample(var AMessage: TMessage); message WM_RECIEVED_SAMPLE_FROM_CALLBACK;
    procedure OnHandledCaptureEvent(var AMessage: TMessage); message WM_APP_CAPTURE_EVENT_HANDLED;
    // Another option to handle the CaptureEngine messages is to use the WndProc method.
    // procedure WndProc(var Msg: TMessage); override;

    // Update menuitems and status
    procedure UpdateUI();

    // Process sample in main thread
    function ProcessSample(const aSample: IMFSample): HResult;
    procedure PaintCapture(bm: TMfpBitmap);
    procedure GetPaintArea(AImage: TMfpBitmap;
                           var AWidth: Integer;
                           var AHeight: Integer;
                           var ATop: Integer;
                           var ALeft: Integer);
    function GetDefaultSaveName: string;

  public
    { Public declarations }

    // Destroy all objects and reinitialize them.
    procedure ReInitializeCaptureObjects();

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

begin
  inherited;

  if not Visible then
    Exit;

  if Assigned(FCaptureManager) then
    begin
      crD.left := 0;
      crD.top := 0;
      crD.right := pbCapture.ClientWidth;
      crD.bottom := pbCapture.ClientHeight;
      pcrD := @crD;

      FCaptureManager.UpdateVideo(pcrD);
    end;
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

      if (PDEV_BROADCAST_HDR(AMessage.LParam).dbch_devicetype <> DBT_DEVTYP_DEVICEINTERFACE) then
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
      SafeDelete(FCaptureManager);
      ErrMsg(Format('Lost capture device %s.' + #13 + 'Please select another device or reconnect.', [FDeviceExplorer.DeviceDisplayName]),
             DBT_DEVICEREMOVECOMPLETE);
    end;
end;


//------------------------------------------------------------------------------
//  OnCaptureEvent
//
//  Handles WM_APP_CAPTURE_EVENT messages.
//------------------------------------------------------------------------------
procedure TMainWindow.OnCaptureEvent(var AMessage: TMessage);
begin
  FCaptureManager.OnCaptureEvent(AMessage.WParam,
                                 AMessage.LParam);

  WaitForSingleObject(Handle,
                      INFINITE);
end;


//------------------------------------------------------------------------------
//  OnRecievedSample
//
//  Handles WM_RECIEVED_SAMPLE_FROM_CALLBACK messages.
//------------------------------------------------------------------------------
procedure TMainWindow.OnRecievedSample(var AMessage: TMessage);
var
  psample: IMFSample;
begin
  psample := IMFSample(Pointer(AMessage.WParam));
  // process the sample
  ProcessSample(pSample);
end;


//------------------------------------------------------------------------------
//  OnHandledCaptureEvent
//
//  Handles WM_APP_CAPTURE_EVENT_HANDLED messages.
//------------------------------------------------------------------------------
procedure TMainWindow.OnHandledCaptureEvent(var AMessage: TMessage);
begin
  if SUCCEEDED(HResult(AMessage.WParam)) then
    UpdateUI()
  else
    ErrMsg('CaptureManager reported a failure.',
           HResult(AMessage.WParam));
end;


// using WndProc
{
procedure TMainWindow.WndProc(var Msg: TMessage);
var
  psample: IMFSample;

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

        FCaptureManager.OnCaptureEvent(Msg.WParam,
                                       Msg.LParam);

        WaitForSingleObject(Handle,
                            INFINITE);
      end;

    // When a capture event is processed by the capture engine, update the statusbar and menu.
    WM_APP_CAPTURE_EVENT_HANDLED:
      begin
        if SUCCEEDED(HResult(Msg.WParam)) then
          UpdateUI()
        else
          ErrMsg('CaptureManager repported a failure.',
                 HResult(Msg.WParam));

      end;

    // Any other messages are passed to DefWindowProc, which tells Windows to handle the message.
    else
      msg.Result := DefWindowProc(Handle,
                                  Msg.Msg,
                                  Msg.WParam,
                                  Msg.LParam);
  end;

  inherited WndProc(Msg);
end; }


// ProcessSample
function TMainWindow.ProcessSample(const aSample: IMFSample): HResult;
var
  hr: HResult;
  FMemoryStream: TMemoryStream;

label
  done;
begin

  hr := FSampleConverter.UpdateConverter(g_pDXGIMan,
                                         FDeviceExplorer.DeviceProperties[FDeviceExplorer.DeviceIndex].aVideoFormats[FDeviceExplorer.FormatIndex].mfMediaType);
  if FAILED(hr) then
    goto done;

  hr := FSampleConverter.DataFromSample(aSample,
                                        FDeviceExplorer.DeviceProperties[FDeviceExplorer.DeviceIndex].aVideoFormats[FDeviceExplorer.FormatIndex],
                                        FMemoryStream);
  if FAILED(hr) then
    goto done;

  if Assigned(FMemoryStream) then
    begin
      if Assigned(bmCapturedFrame) then
        SafeDelete(bmCapturedFrame);
      // stream returned, let's assign to preview
      bmCapturedFrame := TMfpBitmap.Create;
      bmCapturedFrame.PixelFormat := pf32bit;
      // Set streampointer to start.
      FMemoryStream.Position := 0;

      if bmCapturedFrame.CanLoadFromStream(FMemoryStream) then
        begin
          bmCapturedFrame.LoadFromStream(FMemoryStream);
          if not bmCapturedFrame.Empty then
            begin
              hr := aSample.GetSampleTime(iSampleTime);
              if FAILED(hr) then
                goto done;

              PaintCapture(bmCapturedFrame);
            end
          else
            begin
              hr := E_FAIL;
              goto done;
            end;
        end
      else
        goto done;

    end
  else
    hr := E_POINTER;

done:
  FMemoryStream.Destroy();
  Result := hr;
end;


procedure TMainWindow.PaintCapture(bm: TMfpBitmap);
var
  iWidth: Integer;
  iHeight: Integer;
  iTop: Integer;
  iLeft: Integer;


begin
  if not bImageCleared and Assigned(bm) and not bm.Empty then
    begin
      // Clear the area
      pbCapture.Canvas.Brush.Style := bsSolid;
      pbCapture.Canvas.Brush.Color := clBlack;
      pbCapture.Canvas.FillRect(pbCapture.Canvas.ClipRect);

      SetStretchBltMode(pbCapture.Canvas.Handle,
                        HALFTONE);
      SetBrushOrgEx(pbCapture.Canvas.Handle,
                    0,
                    0,
                    nil);

      // Scale and center the image
      GetPaintArea(bm,
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
                 bm.Canvas.Handle,
                 0,
                 0,
                 bm.Width,
                 bm.Height,
                 SRCCOPY);
      butSaveToFile.Enabled := True;
    end
  else
    butSaveToFile.Enabled := False;
end;


procedure TMainWindow.GetPaintArea(AImage: TMfpBitmap;
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
  if (iHeightRatio > iWidthRatio) then
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


procedure TMainWindow.butSaveToFileClick(Sender: TObject);
begin

  SaveFileDlg.FileName := GetDefaultSaveName();
  if SaveFileDlg.Execute then
    begin
      SaveImage(bmCapturedFrame,
                SaveFileDlg.FileName,
                TImageType(Ord(SaveFileDlg.FilterIndex - 1)));
    end;

end;


function TMainWindow.GetDefaultSaveName(): string;
begin

  Result := 'Capture_' + HnsTimeToStr(iSampleTime,
                                      '_',
                                      False);
end;


// butTakePhotoClick
procedure TMainWindow.butTakePhotoClick(Sender: TObject);
var
  hr: HResult;

label
  Done;

begin
  hr := E_FAIL;

  if Assigned(FCaptureManager) then
    hr := FCaptureManager.TakePhoto(ssoCallBack,
                                    FDeviceExplorer.DeviceProperties[FDeviceExplorer.DeviceIndex].aVideoFormats[FDeviceExplorer.FormatIndex].mfMediaType);
  if FAILED(hr) then
    goto Done;

Done:
  if FAILED(hr) then
    ErrMsg('butTakePhotoClick: ' + ERR_PHOTO,
            hr);
end;

// Reinitialize all objects
procedure TMainWindow.ReInitializeCaptureObjects();
begin
  DestroyCaptureObjects();
  if FAILED(CreateCaptureObjects()) then
    begin
      ErrMsg('AfterConstruction: Can not create CaptureManager. The application will be closed.',
             E_FAIL);
      Exit;
    end;
end;


procedure TMainWindow.DestroyCaptureObjects();
begin
  if Assigned(bmCapturedFrame) then
    FreeAndNil(bmCapturedFrame);

  if Assigned(FCaptureManager) then
     FreeAndNil(FCaptureManager);

  if Assigned(pSelectedDevice) then
    SafeRelease(pSelectedDevice);

  if Assigned(FDeviceExplorer) then
     FreeAndNil(FDeviceExplorer);

  if Assigned(FSampleConverter) then
     FreeAndNil(FSampleConverter);
end;


function TMainWindow.CreateCaptureObjects(): HResult;
var
  hr: HResult;
label
  done;

begin

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

  // Create the sampleconverter
  FSampleConverter := TSampleConverter.Create();

  UpdateUI();
done:
  Result := hr;
end;


// FormCreate
procedure TMainWindow.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := False;

  DestroyCaptureObjects();

  MFShutdown();
  CoUnInitialize();
  CanClose := True;
end;


procedure TMainWindow.FormCreate(Sender: TObject);
var
  hr: HResult;

label
  Done;

begin

  // Initialize COM
  CoInitializeEx(nil,
                 COINIT_APARTMENTTHREADED);
  // Startup Media Foundation
  hr := MFStartup(MF_VERSION,
                  0);

  if FAILED(hr) then
    begin
      MessageBox(0,
                 lpcwstr('Your computer does not support this Media Foundation API version' + IntToStr(MF_VERSION) + '.'),
                 lpcwstr('MFStartup Failure!'),
                 MB_ICONSTOP);
      goto Done;
   end;

  iSelectedDevice := -1;
  iSelectedFormat := -1;
  hPreview := pnlPreview.Handle;
  hCaptureHandle := pbCapture.Canvas.Handle;

  //Create objects
  hr := CreateCaptureObjects();

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

label
  Done;

begin

  // Stop the current manager
  if Assigned(FCaptureManager) then
    begin
      if FCaptureManager.IsPreviewing then
        FCaptureManager.StopPreview();
      if FCaptureManager.IsRecording then
        FCaptureManager.StopRecording();
    end;

  // Create the dialog if it's not allready done.
  if not Assigned(ChooseDeviceDlg) then
    begin
      Application.CreateForm(TChooseDeviceDlg,
                             ChooseDeviceDlg);
      ChooseDeviceDlg.Visible := False;
    end;

  // Ask the user to select one.
  if (ChooseDeviceDlg.ShowModal = 1212) then
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

      SafeRelease(pSelectedDevice);
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
      pnlInfo.Caption := 'Please select a device.';
      Exit;
    end;

  if not FCaptureManager.IsInitialized then
    pnlInfo.Caption := 'Please select a device.'
  else
    pnlInfo.Caption := 'Please select ''Start Preview''';

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
      mnuStartPreview.Caption := 'Stop Preview';
      mnuStartPreview.Tag := 1;
      bEnableRecording := True;
      bEnablePhoto := True;
    end
  else
    begin
      mnuStartPreview.Caption := 'Start Preview';
      mnuStartPreview.Tag := 0;
    end;

  if bRecording then
    pnlInfo.Caption := 'Recording'
  else if FCaptureManager.IsPreviewing then
    pnlInfo.Caption := 'Previewing'
  else
    begin
      bEnableRecording := False;
    end;

  if (FCaptureManager.IsPreviewing or FCaptureManager.IsPhotoPending) then
    bEnablePhoto := False;

  mnuStartRecording.Enabled := bEnableRecording;
  mnuStartPreview.Enabled := bEnablePreview;

end;


// mnuStartPreviewClick
// Dont' call UpdateUI() when adressing the capture engine,
// this method will be called when the capture engine OnCaptureEvent is handled.
procedure TMainWindow.mnuStartPreviewClick(Sender: TObject);
var
  hr: HResult;

begin
  if Assigned(FCaptureManager) then
    begin
      if (mnuStartPreview.Tag = 0) then
        begin
          hr := FCaptureManager.StartPreview();
          if FAILED(hr) then
            begin
              butTakePhoto.Enabled := False;
              ErrMsg('mnuStartPreviewClick ' + ERR_PREVIEW,
                     hr);
              Exit;
            end;
          butSaveToFile.Enabled := False;
          butTakePhoto.Enabled := True;
        end
      else
        begin
          hr := FCaptureManager.StopPreview();
          if FAILED(hr) then
            ErrMsg('mnuStartPreviewClick ' + ERR_STOP_PREVIEW,
                   hr);
        end;
    end;
end;


// mnuStartRecordingClick
procedure TMainWindow.mnuStartRecordingClick(Sender: TObject);
begin
  FCaptureManager.StartRecording(nil);
end;


procedure TMainWindow.Exit1Click(Sender: TObject);
begin
  Close();
end;

end.
