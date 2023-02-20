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
// 20/02/2023 Tony                Fixed switching camera issue that results in Access Denied error.
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
    bRecording: Boolean;
    bPreviewing: Boolean;
    bImageCleared: Boolean;
    bmCapturedFrame: TMfpBitmap;
    iSelectedDevice: Integer;
    iSelectedFormat: Integer;
    lSampleTime: LongLong;
    FSampleConverter: TSampleConverter;
    pSelectedDevice: IMFActivate;

    procedure DestroyCaptureObjects();
    function CreateDeviceExplorer(): HResult;

    // Windows messages
    procedure OnSize(var message: TWMSize); message WM_SIZE;
    procedure OnDeviceChange(var AMessage: TMessage); message WM_DEVICECHANGE;


    // Update menuitems and status
    procedure UpdateUI();

    // Process sample in main thread
    function ProcessSample(aSample: IMFSample): HResult;
    procedure PaintCapture(bm: TMfpBitmap);
    procedure GetPaintArea(AImage: TMfpBitmap;
                           var AWidth: Integer;
                           var AHeight: Integer;
                           var ATop: Integer;
                           var ALeft: Integer);

  public
    { Public declarations }
    // Captureengine message handler
    procedure WndProc(var Message: TMessage); override;

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


// WndProc
procedure TMainWindow.WndProc(var message: TMessage);
var
  hr: HResult;
  pSample: IMFSample;

begin

  case message.Msg of

    // We did recieve a sample from TCaptureManager.TCaptureEngineOnSampleCallback.OnSample
    WM_RECIEVED_SAMPLE_FROM_CALLBACK:
      begin
        pSample := IMFSample(Pointer(message.WParam));
        if Assigned(pSample) then
          // process the sample
          ProcessSample(pSample);
      end;

    WM_APP_CAPTURE_EVENT:
      begin
        hr := FCaptureManager.OnCaptureEvent(message.WParam,
                                             message.LParam);
        if FAILED(hr) then
          begin
            ErrMsg('Capturemanager.OnCaptureEvent reported an error',
                   hr);
          end;
      end;

    // When a capture event is processed by the capture engine, update the statusbar and menu.
    WM_APP_CAPTURE_EVENT_HANDLED:
      begin

        if SUCCEEDED(HResult(message.WParam)) then
          UpdateUI()
        else
          ErrMsg('CaptureManager repported a failure.',
                 HResult(message.WParam));

      end;

    // Any other messages are passed to DefWindowProc, which tells Windows to handle the message.
    else
      message.Result := DefWindowProc(Handle,
                                      message.Msg,
                                      message.WParam,
                                      message.LParam);
  end;

  inherited WndProc(message);
end;


// ProcessSample
function TMainWindow.ProcessSample(aSample: IMFSample): HResult;
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
              hr := aSample.GetSampleTime(lSampleTime);
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
  if Assigned(FMemoryStream) then
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

  SaveFileDlg.FileName := Format('Capture_%s', [HnsTimeToStr(lSampleTime,
                                                             '_',
                                                             False)]);
  if SaveFileDlg.Execute then
    begin
      SaveImage(bmCapturedFrame,
                SaveFileDlg.FileName,
                TImageType(Ord(SaveFileDlg.FilterIndex - 1)));
    end;

end;


// butTakePhotoClick
procedure TMainWindow.butTakePhotoClick(Sender: TObject);
var
  hr: HResult;
label
  done;

begin
  hr := E_FAIL;

  if Assigned(FCaptureManager) then
    hr := FCaptureManager.TakePhoto(ssoCallBack,
                                    FDeviceExplorer.DeviceProperties[FDeviceExplorer.DeviceIndex].aVideoFormats[FDeviceExplorer.FormatIndex].mfMediaType);
  if FAILED(hr) then
    goto Done;

done:
  if FAILED(hr) then
    ErrMsg('butTakePhotoClick: ' + ERR_PHOTO,
            hr);
end;


procedure TMainWindow.DestroyCaptureObjects();
begin
  if Assigned(bmCapturedFrame) then
    SafeDelete(bmCapturedFrame);

  if Assigned(FCaptureManager) then
    begin
      FCaptureManager.ResetCaptureManager();
      SafeDelete(FCaptureManager);
    end;

  FChooseDeviceParam.Reset();

end;


// FormCreate
procedure TMainWindow.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := False;

  DestroyCaptureObjects();

  if Assigned(FSampleConverter) then
     FreeAndNil(FSampleConverter);

  if Assigned(FDeviceExplorer) then
     begin
       FreeAndNil(FDeviceExplorer);
     end;

  MFShutdown();
  CoUnInitialize();
  CanClose := True;
end;


procedure TMainWindow.FormCreate(Sender: TObject);
var
  hr: HResult;
label
  done;

begin

  // Initialize COM
  CoInitializeEx(nil,
                 COINIT_APARTMENTTHREADED or COINIT_DISABLE_OLE1DDE);
  // Startup Media Foundation
  hr := MFStartup(MF_VERSION,
                  MFSTARTUP_FULL);

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

  // Create DeviceExplorer
  hr := CreateDeviceExplorer();
  // There is no need to shutdown the application, because a user can still add devices.
  if (hr = MF_E_NO_CAPTURE_DEVICES_AVAILABLE) then
    begin
      hr := S_OK;
      goto done;
    end;
  // All other failures will result in termination.
  if FAILED(hr) then
    begin
      MessageBox(0,
                 lpcwstr('Failed to create the DeviceExplorer, Resultcode = (' + IntToStr(hr) + ').'),
                 lpcwstr('DeviceExplorer Failure!'),
                 MB_ICONSTOP);
      goto done;
    end;

  // Create the capture engine
  FCaptureManager := TCaptureManager.Create(Handle);

  if not Assigned(FCaptureManager) then
    begin
      ErrMsg('AfterConstruction: Can not create CaptureManager. The application will be closed.',
             E_POINTER);
      goto done;
    end;

  hr := FCaptureManager.InitializeCaptureManager(hPreview,
                                                 Handle,
                                                 pSelectedDevice);
  if SUCCEEDED(hr) then
    // Create the sampleconverter
    FSampleConverter := TSampleConverter.Create()
  else
    hr := E_POINTER;

done:
  if FAILED(hr) then
    Application.Terminate;
end;


// CreateDeviceExplorer
// Callers: FormCreate
function TMainWindow.CreateDeviceExplorer(): HResult;
var
  hr: HResult;
label
  done;

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
done:
  Result := hr;
end;


// mnuChooseDeviceClick
procedure TMainWindow.mnuChooseDeviceClick(Sender: TObject);
var
  hr: HResult;
  pAttributes: IMFAttributes;
  i: Integer;

label
  done;

begin

  // Stop the current manager
  if Assigned(FCaptureManager) then
    begin
      if FCaptureManager.IsPreviewing then
        FCaptureManager.StopPreview();
      if FCaptureManager.IsRecording then
        FCaptureManager.StopRecording();
    end;

  hr := MFCreateAttributes(pAttributes,
                           1);

  hr := pAttributes.SetGUID(MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE,
                            MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE_VIDCAP_GUID);

  hr := MFEnumDeviceSources(pAttributes,
                            FChooseDeviceParam.ppDevices,
                            FChooseDeviceParam.count);


  // Ask the user to select one.
  if (ChooseDeviceDlg.ShowModal = 1212) then
    begin
      {$POINTERMATH ON}
      pSelectedDevice := FChooseDeviceParam.ppDevices[FChooseDeviceParam.SelectedDevice];
      {$POINTERMATH OFF}
      iSelectedDevice := FChooseDeviceParam.SelectedDevice;
      iSelectedFormat := FChooseDeviceParam.SelectedFormat;
      hPreview := pnlPreview.Handle;

      // Set DeviceExplorer properties
      hr := FDeviceExplorer.SetCurrentDeviceProperties(iSelectedDevice,
                                                       iSelectedFormat);
      if FAILED(hr) then
        goto Done;

      {$POINTERMATH ON}

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

done:

  if (hr = MF_E_NO_CAPTURE_DEVICES_AVAILABLE) or (FChooseDeviceParam.Count = 0) then
    ShowMessage(format('No capture devices found on this system (hr = %d)',
                       [hr]))
  else if FAILED(hr) then
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
  else if FCaptureManager.IsPhotoPending then
    bEnablePhoto := False
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

  mnuStartRecording.Enabled := bEnableRecording;
  mnuStartPreview.Enabled := bEnablePreview;
  //pnlControls.Enabled := bEnablePhoto;
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
          mnuStartPreview.Tag := 1;
        end
      else
        begin
          hr := FCaptureManager.StopPreview();
          mnuStartPreview.Tag := 0;
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
