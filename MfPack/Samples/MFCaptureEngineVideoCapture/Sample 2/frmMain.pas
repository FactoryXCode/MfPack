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
// Revision Version: 3.1.7
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
// 30/06/2024 All                 RammStein release  SDK 10.0.26100.0 (Windows 11)
// 28/06/2024 Tony                Solved some issues when recapturing with same formats.
// 27/07/2024 Tony                Solved issue with incorrect videocapture format.
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 (2H20) or later.
//
// Related objects: -
// Related projects: MfPackX317/Samples/MFCaptureEngineVideoCapture
//
// Compiler version: 23 up to 35
// SDK version: 10.0.26100.0
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
  WinApi.KsMedia,
  {System}
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.UITypes,
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
  {DirectShow}
  WinApi.StrmIf,
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
  TStartMode = (smVirgin,
                smReUse);

  TMainWindow = class(TForm)
    MainMenu: TMainMenu;
    Capture1: TMenuItem;
    mnuStartPreview: TMenuItem;
    mnuChooseDevice: TMenuItem;
    mnuStartRecording: TMenuItem;
    dlgSaveSnapShot: TSaveDialog;
    N1: TMenuItem;
    Exit1: TMenuItem;
    pnlSnapShot: TPanel;
    pbCapture: TPaintBox;
    pnlControls: TPanel;
    butSaveToFile: TButton;
    butTakePhoto: TButton;
    chkNoPreview: TCheckBox;
    pnlInfo: TPanel;
    dlgSaveVideo: TSaveDialog;
    Options1: TMenuItem;
    mnuSetVideoOutputFormat: TMenuItem;
    pnlCameraAndVideoControl: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    lblRotation: TLabel;
    cbxCameraControlProperty: TComboBox;
    cbxVideoControlProperty: TComboBox;
    cbxCameraControlFlags: TComboBox;
    cbxVideoControlFlags: TComboBox;
    cboRotation: TComboBox;
    cbxCameraValues: TComboBox;
    cbxVideoValues: TComboBox;
    mnuCameraVideoSettings: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure mnuChooseDeviceClick(Sender: TObject);
    procedure mnuStartPreviewClick(Sender: TObject);
    procedure butTakePhotoClick(Sender: TObject);
    procedure mnuStartRecordingClick(Sender: TObject);
    procedure butSaveToFileClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure Exit1Click(Sender: TObject);
    procedure mnuSetVideoOutputFormatClick(Sender: TObject);
    procedure cbxCameraControlPropertySelect(Sender: TObject);
    procedure mnuCameraVideoSettingsClick(Sender: TObject);
    procedure cbxCameraControlFlagsSelect(Sender: TObject);
    procedure cbxVideoControlFlagsSelect(Sender: TObject);
    procedure cbxVideoValuesSelect(Sender: TObject);
    procedure cbxCameraValuesSelect(Sender: TObject);

  private
    { Private declarations }
    hPreview: HWND;
    ptrDevNotify: HDEVNOTIFY; // Devicenotify pointer.
    pvPreviewMode: TStartMode;
    pvStartRecordMode: TStartMode;
    bRecording: Boolean;
    bPreviewing: Boolean;
    bImageCleared: Boolean;
    bPhotoPending: Boolean;
    bPhotoMade: Boolean;
    bCaptureManInitialized: Boolean;

    bDeviceLost: Bool;
    bmCapturedFrame: TMfpBitmap;
    iSelectedDevice: Integer;
    iSelectedFormat: Integer;
    pSelectedDevice: IMFActivate;
    lSampleTime: LongLong;
    FSampleConverter: TSampleConverter;
    pvVideoFile: TFileName;

    // Camera & video control.
    fSelectedCamaraControl: TCameraPropSet;
    fSelectedVideoControl: TVideoPropSet;

    procedure DestroyCaptureObjects();
    function CreateDeviceExplorer(): HResult;

    // Update menu items and status.
    procedure UpdateUI(const pGuidType: TGUID);

    // Process sample in main thread
    function ProcessSample(aSample: IMFSample): HResult;
    procedure PaintCapture(bm: TMfpBitmap);
    procedure GetPaintArea(AImage: TMfpBitmap;
                           var AWidth: Integer;
                           var AHeight: Integer;
                           var ATop: Integer;
                           var ALeft: Integer);

    function CreateNewFileName(const ext: string): TFileName;

    // Camera and video control.
    procedure SetCbxValues(pCbx: TComboBox;
                           iContrPropIndex,
                           iMax,
                           iMin,
                           iStep,
                           iDefault,
                           iFlags: Integer;
                           pCtl: LONG);

    function SetCameraProperties(pIndex: Integer): HRESULT;
    function SetVideoProperties(pIndex: Integer): HRESULT;

    procedure SetCameraPropFlags();
    procedure SetVideoPropFlags();

    // Messages
    procedure OnSize(var message: TWMSize); message WM_SIZE;
    procedure OnRecievedSample(var AMessage: TMessage); message WM_RECIEVED_SAMPLE_FROM_CALLBACK;
    procedure OnCaptureEvent(var AMessage: TMessage); message WM_APP_CAPTURE_EVENT;
    procedure OnCaptureEventHandled(var AMessage: TMessage); message WM_APP_CAPTURE_EVENT_HANDLED;
    procedure OnDeviceChange(var AMessage: TMessage); message WM_DEVICECHANGE;

  public
    { Public declarations }

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


procedure TMainWindow.OnRecievedSample(var AMessage: TMessage);
var
  pSample: IMFSample;
begin
  pSample := IMFSample(Pointer(AMessage.WParam));
  // process the sample
  if Assigned(pSample) then
    ProcessSample(pSample);
end;


procedure TMainWindow.OnCaptureEvent(var AMessage: TMessage);
var
  hr: HResult;

begin
  hr := FCaptureManager.OnCaptureEvent(AMessage.WParam,
                                       AMessage.LParam);
  if FAILED(hr) then
    begin
      ErrMsg(Format('Capturemanager.OnCaptureEvent reported error: %d', [hr]),
             hr);
    end;
end;


procedure TMainWindow.OnCaptureEventHandled(var AMessage: TMessage);
var
  amsg: TGUID;

begin
  amsg := PGUID(AMessage.LParam)^;

  if SUCCEEDED(HResult(AMessage.WParam)) then
    UpdateUI(amsg)
  else
    ErrMsg('CaptureManager reported a failure.',
           HResult(AMessage.WParam));
end;


procedure TMainWindow.OnDeviceChange(var AMessage: TMessage);
var
  PDevBroadcastHeader: PDEV_BROADCAST_HDR;
  pDevBroadCastIntf: PDEV_BROADCAST_DEVICEINTERFACE;
  pwcDevSymbolicLink: PWideChar;
  pwcCurrentDeviceSymLink: PWideChar;
  hr: HResult;
  strCurrentDeviceName: string;

begin
  if (AMessage.WParam = DBT_DEVICEREMOVECOMPLETE) then
    begin
      // remember the current device that is activated
      strCurrentDeviceName := WideCharToString(FDeviceExplorer.DeviceDisplayName);
      pwcCurrentDeviceSymLink := FDeviceExplorer.DeviceSymbolicLink;

      // Check for added/removed devices, regardless of whether
      // the application is capturing video at this time.
      if FAILED(CreateDeviceExplorer()) then
        Exit;

      // Check if the current video capture device was lost.
      if (PDEV_BROADCAST_HDR(AMessage.LParam).dbch_devicetype <> DBT_DEVTYP_DEVICEINTERFACE) then
        Exit;

      // Get the symboliclink of the lost device and check.
      PDevBroadcastHeader := PDEV_BROADCAST_HDR(AMessage.LParam);
      pDevBroadCastIntf := PDEV_BROADCAST_DEVICEINTERFACE(PDevBroadcastHeader);

      // Note: Since Windows 8 the value of dbcc_name is no longer the devicename, but the symboliclink of the device.
      // Dereference the struct's field dbcc_name (array [0..0] of WideChar) for a readable string.
      pwcDevSymbolicLink := PChar(@pDevBroadCastIntf^.dbcc_name);

      hr := S_OK;
      bDeviceLost := False;

      if Assigned(FCaptureManager) then
        if FCaptureManager.IsPreviewing or FCaptureManager.IsRecording then
          begin
            if (StrIComp(pwcCurrentDeviceSymLink,
                         pwcDevSymbolicLink) = 0) then
              bDeviceLost := True;

            if (FAILED(hr) or bDeviceLost) then
              begin
                MessageDlg(Format('Lost capture device: %s', [strCurrentDeviceName]),
                           mtError,
                           mbOKCancel,
                           0);
                iSelectedDevice := -1;
                iSelectedFormat := -1;
              end;
          end;
    end;
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
        FreeAndNil(bmCapturedFrame);
      // stream returned, let's assign to preview.
      bmCapturedFrame := TMfpBitmap.Create;
      bmCapturedFrame.PixelFormat := pf32bit;
      // Set streampointer to start.
      FMemoryStream.Position := 0;

      if bmCapturedFrame.CanLoadFromStream(FMemoryStream) then
        begin
          bmCapturedFrame.LoadFromStream(FMemoryStream);
          if not bmCapturedFrame.Empty then
            begin
              hr := aSample.GetSampleTime(@lSampleTime);
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

  dlgSaveSnapShot.FileName := Format('Capture_%s', [HnsTimeToStr(lSampleTime,
                                                    '_',
                                                    False)]);
  if dlgSaveSnapShot.Execute then
    begin
      SaveImage(bmCapturedFrame,
                dlgSaveSnapShot.FileName,
                TImageType(Ord(dlgSaveSnapShot.FilterIndex - 1)));
    end;
end;


// butTakePhotoClick
procedure TMainWindow.butTakePhotoClick(Sender: TObject);
var
  hr: HResult;
  SnapShotOption: TSnapShotOptions;

label
  done;

begin

  hr := E_FAIL;

  if chkNoPreview.Checked then
    SnapShotOption := ssoFile
  else
    SnapShotOption := ssoCallBack;

  if Assigned(FCaptureManager) then
    hr := FCaptureManager.TakePhoto(SnapShotOption,
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
    FreeAndNil(bmCapturedFrame);

  if Assigned(FCaptureManager) then
    begin
      FCaptureManager.ResetCaptureManager();
      FCaptureManager.Free;
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

  UnRegisterForDeviceNotification(ptrDevNotify);
  ptrDevNotify := nil;

  CanClose := True;
end;


procedure TMainWindow.FormCreate(Sender: TObject);
var
  hr: HResult;
label
  done;

begin

  iSelectedDevice := -1;
  iSelectedFormat := -1;
  bDeviceLost := False;
  hPreview := Handle;

  // Create DeviceExplorer here to check for suitable devices.
  hr := CreateDeviceExplorer();
  // There is no need to shutdown the application, because a user can still add devices.
  if (hr = MF_E_NO_CAPTURE_DEVICES_AVAILABLE) then
    begin
      MessageBox(0,
                 lpcwstr('Currently there are no devices found: Please insert a device.'),
                 lpcwstr('No capture devices found.'),
                 MB_ICONWARNING);
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
                                                 pSelectedDevice,
                                                 False);
  if SUCCEEDED(hr) then
    // Create the sampleconverter
    FSampleConverter := TSampleConverter.Create()
  else
    hr := E_POINTER;

  //
  if not RegisterForDeviceNotification(Handle,
                                       ptrDevNotify) then
    hr := E_FAIL;

  //
  bRecording := False;
  bPreviewing := False;
  bImageCleared := False;
  bPhotoPending := False;
  bPhotoMade := False;
  bCaptureManInitialized := False;

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
    FreeAndNil(FDeviceExplorer);

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

label
  done;

begin

  // Stop the current manager
  if Assigned(FCaptureManager) then
    begin
      if bRecording then
        FCaptureManager.StopRecording();
      if bPreviewing then
        FCaptureManager.StopPreview();
    end;


  hr := MFCreateAttributes(pAttributes,
                           1);
  if FAILED(hr) then
    goto done;

  hr := pAttributes.SetGUID(MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE,
                            MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE_VIDCAP_GUID);
  if FAILED(hr) then
    goto done;

  hr := MFEnumDeviceSources(pAttributes,
                            FChooseDeviceParam.ppDevices,
                            FChooseDeviceParam.count);
  if FAILED(hr) then
    goto done;

  // Get a new instance of the device explorer
  hr := CreateDeviceExplorer();
  if FAILED(hr) then
    goto done;

  // Ask the user to select one.
  if (ChooseDeviceDlg.ShowModal = 1212) then
    begin
      {$POINTERMATH ON}
      pSelectedDevice := FChooseDeviceParam.ppDevices[FChooseDeviceParam.SelectedDevice];
      {$POINTERMATH OFF}
      iSelectedDevice := FChooseDeviceParam.SelectedDevice;
      iSelectedFormat := FChooseDeviceParam.SelectedFormat;
      hPreview := Handle;

      // Set DeviceExplorer properties.
      hr := FDeviceExplorer.SetCurrentDeviceProperties(iSelectedDevice,
                                                       iSelectedFormat);
      if FAILED(hr) then
        goto Done;

      hr := FCaptureManager.InitializeCaptureManager(hPreview,
                                                     Handle,
                                                     IUnknown(pSelectedDevice),
                                                     True);
      if FAILED(hr) then
        goto Done;

      // Get the device properties.
      hr := SetCameraProperties(4);  // Exposure.
      if FAILED(hr) then
        begin
          MessageBox(0,
                     lpcwstr('Set camera properties failed with error code: ' + IntToStr(hr) + ''),
                     lpcwstr('Camera properties Failure!'),
                     MB_ICONSTOP);
          Exit;
        end;

      hr := SetVideoProperties(0); // Brightness.

      if FAILED(hr) then
        begin
          MessageBox(0,
                     lpcwstr('Set video properties failed with error code: ' + IntToStr(hr) + ''),
                     lpcwstr('Video properties Failure!'),
                     MB_ICONSTOP);
        end;

      pvPreviewMode := smVirgin;
      pvStartRecordMode := smVirgin;
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

  mnuCameraVideoSettings.Enabled := SUCCEEDED(hr);

end;


// UpdateUI
procedure TMainWindow.UpdateUI(const pGuidType: TGUID);
begin

  if (pGuidType = MF_CAPTURE_ENGINE_INITIALIZED) then
    begin
      bCaptureManInitialized := True;
      pnlInfo.Caption := 'Please select ''Choose Device '' or ''Start Preview''';
      mnuStartPreview.Caption := 'Start Preview';
      mnuStartRecording.Caption := 'Start Recording';
      mnuStartPreview.Tag := 0;
      mnuStartRecording.Tag := 0;
      pnlControls.Enabled := False;
    end
  else if (pGuidType = MF_CAPTURE_ENGINE_PREVIEW_STARTED) then
    begin
      mnuStartPreview.Caption := 'Stop Preview';
      pnlInfo.Caption := 'Previewing.';
      bPreviewing := True;
      mnuStartPreview.Enabled := True;
      mnuStartRecording.Enabled := True;
      mnuChooseDevice.Enabled := False;
      pnlControls.Enabled := True;

    end
  else if (pGuidType = MF_CAPTURE_ENGINE_PREVIEW_STOPPED) then
    begin
      mnuStartPreview.Caption := 'Start Preview';
      pnlInfo.Caption := 'Stopped Previewing.';
      bPreviewing := False;
      mnuStartPreview.Enabled := True;
      mnuStartRecording.Enabled := False;
      mnuChooseDevice.Enabled := True;
      pnlControls.Enabled := False;
    end
  else if (pGuidType = MF_CAPTURE_ENGINE_RECORD_STARTED) then
    begin
      mnuStartRecording.Caption := 'Stop Recording';
      pnlInfo.Caption := Format('Recording to file %s', [pvVideoFile]);
      bRecording := True;
      pnlControls.Enabled := True;
      mnuStartRecording.Enabled := True;
      pnlControls.Enabled := True;
    end
  else if (pGuidType = MF_CAPTURE_ENGINE_RECORD_STOPPED) then
    begin
      mnuStartRecording.Caption := 'Start Recording';
      pnlInfo.Caption := Format('Stopped recording to file %s', [pvVideoFile]);
      Sleep(1000);
      pnlInfo.Caption := 'Previewing.';
      bRecording := False;
      mnuStartRecording.Enabled := True;
      pnlControls.Enabled := bPreviewing;
    end
  else if (pGuidType = MF_CAPTURE_ENGINE_PHOTO_TAKEN) then
    begin
      pnlInfo.Caption := 'Photo has been made.';
      bPhotoMade := True;
      pnlControls.Enabled := True;
    end
  else if (pGuidType = MF_CAPTURE_SINK_PREPARED) then
    begin
      // Stub.
      // pnlInfo.Caption := 'The capture sink has been initialized.';
    end
  else if (pGuidType = MF_CAPTURE_ENGINE_ERROR) then
    begin
      // The capture engine will be destroyed.
      // So, at this point we have to reinitialize the capture engine again.
      Exit;
    end
  else
    begin
      // We got an unknown message from the captureEngine.
    end;

  if bDeviceLost then
    begin
      if bRecording then
        FCaptureManager.StopRecording()
      else if bPreviewing then
        FCaptureManager.StopPreview;

      mnuStartRecording.Caption := 'Start Recording';
      mnuStartPreview.Tag := 0;
      mnuStartRecording.Enabled := False;
      mnuStartPreview.Enabled := False;
      pnlInfo.Caption := 'Please select a device.';
      pnlControls.Enabled := False;
      bDeviceLost := False;
      pvStartRecordMode := smVirgin;
      pvPreviewMode := smVirgin;
      Exit;
    end;
end;


procedure TMainWindow.mnuSetVideoOutputFormatClick(Sender: TObject);
begin
  dlgSaveVideo.FileName := CreateNewFileName('.mp4');
  if dlgSaveVideo.Execute then
    pvVideoFile := dlgSaveVideo.FileName;
end;


// mnuStartPreviewClick
// Dont' call UpdateUI() when adressing the capture engine,
// this method will be called when the capture engine OnCaptureEvent is handled.
procedure TMainWindow.mnuStartPreviewClick(Sender: TObject);
var
  hr: HResult;

begin

  hr := S_OK;

  if Assigned(FCaptureManager) then
    begin

      if (pvPreviewMode = smVirgin) then
        // Set video preview and recording formats.
        hr := FCaptureManager.SetVideoFormat();
        if FAILED(hr) then
          begin
            ErrMsg('mnuStartPreviewClick ' + ERR_OUTPUT_MEDIATYPE_SET,
                   hr);
            Exit;
          end;

      if (mnuStartPreview.Tag = 0) then
        begin
          if (pvPreviewMode = smVirgin) then
            begin
              hr := FCaptureManager.StartPreview(True);
              if FAILED(hr) then
                begin
                  butTakePhoto.Enabled := False;
                  ErrMsg('mnuStartPreviewClick ' + ERR_PREVIEW,
                         hr);
                  Exit;
                end;
              pvPreviewMode := smReUse;
            end
          else
            begin
              hr := FCaptureManager.StartPreview(False);
              if FAILED(hr) then
                begin
                  butTakePhoto.Enabled := False;
                  ErrMsg('mnuStartPreviewClick ' + ERR_PREVIEW,
                         hr);
                  Exit;
                end;
            end;

            butSaveToFile.Enabled := False;
            butTakePhoto.Enabled := True;
            mnuStartPreview.Tag := 1;

        end
      else
        begin
          hr := S_OK;
          // Stop recording if recording is active.
          if FCaptureManager.IsRecording then
            hr := FCaptureManager.StopRecording();

          if SUCCEEDED(hr) then
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
var
  hr: HResult;

begin

  if Assigned(FCaptureManager) then
    if FCaptureManager.IsRecording then
      begin
        FCaptureManager.StopRecording();
        bRecording := False;
      end
    else
      begin
        if (pvVideoFile = '') or FileExists(pvVideoFile) then
          pvVideoFile := CreateNewFileName('.mp4');

        if (pvStartRecordMode = smVirgin) then
          begin
            hr := FCaptureManager.StartRecording(StrToPWideChar(pvVideoFile),
                                                 False);  // Sinkwriter needs to be initialized.
            pvStartRecordMode := smReUse;
          end
        else
          hr := FCaptureManager.StartRecording(StrToPWideChar(pvVideoFile),
                                               True); // Sinkwriter is reusing its configuration.
        if FAILED(hr) then
          begin
            ErrMsg('mnuStartRecordingClick ' + ERR_RECORD,
                   hr);
            Exit;
          end;
        bRecording := True;
      end;
end;


function TMainWindow.CreateNewFileName(const ext: string): TFileName;
var
  i: Integer;
  sfileName: string;

begin
  i := 0;
  sfileName := 'VideoCapture' + ext;

  if not FileExists(sFileName) then
    begin
      Result := sfileName;
      Exit;
    end
  else
    while FileExists(sFileName) do
      begin
        sFileName := Format('VideoCapture(%d)%s', [i, ext]);
        Inc(i);
      end;
  Result := sFileName;
end;


procedure TMainWindow.Exit1Click(Sender: TObject);
begin
  Close();
end;


procedure TMainWindow.SetCameraPropFlags();
begin

  case cbxCameraControlFlags.ItemIndex of
    0: fSelectedCamaraControl.cpFlags := LONG(KSPROPERTY_CAMERACONTROL_FLAGS_AUTO);
    1: fSelectedCamaraControl.cpFlags := LONG(KSPROPERTY_CAMERACONTROL_FLAGS_MANUAL);
    2: fSelectedCamaraControl.cpFlags := LONG(KSPROPERTY_CAMERACONTROL_FLAGS_ASYNCHRONOUS);
    3: fSelectedCamaraControl.cpFlags := LONG(KSPROPERTY_CAMERACONTROL_FLAGS_ABSOLUTE);
    4: fSelectedCamaraControl.cpFlags := LONG(KSPROPERTY_CAMERACONTROL_FLAGS_RELATIVE);
  end;
end;


procedure TMainWindow.SetVideoPropFlags();
begin

  case cbxVideoControlFlags.ItemIndex of
    0: fSelectedVideoControl.cpFlags := LONG(KSPROPERTY_VIDEOPROCAMP_FLAGS_AUTO);
    1: fSelectedVideoControl.cpFlags := LONG(KSPROPERTY_VIDEOPROCAMP_FLAGS_MANUAL);
  end;
end;


procedure TMainWindow.SetCbxValues(pCbx: TComboBox;
                                   iContrPropIndex,
                                   iMax,
                                   iMin,
                                   iStep,
                                   iDefault,
                                   iFlags: Integer;
                                   pCtl: LONG);
var
  steps,
  val,
  i: Integer;

begin

  pCbx.Clear;

  Steps := (iMax - iMin) div iStep + 1; // +1 to include the maximum value in the combo box.
  val := iMin;

  for i := 0 to Steps - 1 do
    begin

      pCbx.Items.Append(IntToStr(val));
      inc(val,
          iStep);
    end;

  // Set the default value.
  i := pCbx.Items.IndexOf(IntToStr(iDefault));

  if (i <> -1) then
    pCbx.ItemIndex := i // If found, set the default index.
  else
    pCbx.ItemIndex := 0; // Fallback to the first item if default value not found.

  // Set the rest.
  pCbx.Enabled := True;

  if (pCbx.Name = 'cbxCameraValues') then
    begin
      fSelectedCamaraControl.cpCameraControlProperty := pCtl;
      cbxCameraControlProperty.ItemIndex := iContrPropIndex;
      cbxCameraControlFlags.ItemIndex := iFlags;
    end
  else // Video.
    begin
      fSelectedVideoControl.cpVideoProcAmpProperty := pCtl;
      cbxVideoControlProperty.ItemIndex := iContrPropIndex;
      cbxVideoControlFlags.ItemIndex := iFlags;
    end;
end;


function TMainWindow.SetCameraProperties(pIndex: Integer): HRESULT;
begin

  Result := S_OK;

  if not Assigned(FCaptureManager) then
    Exit(E_POINTER);

  cbxCameraValues.Clear;
  cbxCameraValues.Items.Append('X'); // Not supported.
  cbxCameraValues.ItemIndex := 0;
  cbxCameraValues.Enabled := False;

  {
   In this order:
   Pan      = 0
   Tilt     = 1
   Roll     = 2
   Zoom     = 3
   Exposure = 4
   Iris     = 5
   Focus    = 6
  }

  case pIndex of
    0: begin

         if FCaptureManager.CameraProperties.CameraCtl_Pan.IsSupported then
           begin

             SetCbxValues(cbxCameraValues,
                          pIndex,
                          FCaptureManager.CameraProperties.CameraCtl_Pan.prMax,
                          FCaptureManager.CameraProperties.CameraCtl_Pan.prMin,
                          FCaptureManager.CameraProperties.CameraCtl_Pan.prDelta,
                          FCaptureManager.CameraProperties.CameraCtl_Pan.prDefault,
                          FCaptureManager.CameraProperties.CameraCtl_Pan.prflags,
                          LONG(CameraControl_Pan));
           end;
       end;

    1: begin

         if FCaptureManager.CameraProperties.CameraCtl_Tilt.IsSupported then
           begin

             SetCbxValues(cbxCameraValues,
                          pIndex,
                          FCaptureManager.CameraProperties.CameraCtl_Tilt.prMax,
                          FCaptureManager.CameraProperties.CameraCtl_Tilt.prMin,
                          FCaptureManager.CameraProperties.CameraCtl_Tilt.prDelta,
                          FCaptureManager.CameraProperties.CameraCtl_Tilt.prDefault,
                          FCaptureManager.CameraProperties.CameraCtl_Tilt.prflags,
                          LONG(CameraControl_Tilt));
           end;
       end;

    2: begin

         if FCaptureManager.CameraProperties.CameraCtl_Roll.IsSupported then
           begin

             SetCbxValues(cbxCameraValues,
                          pIndex,
                          FCaptureManager.CameraProperties.CameraCtl_Roll.prMax,
                          FCaptureManager.CameraProperties.CameraCtl_Roll.prMin,
                          FCaptureManager.CameraProperties.CameraCtl_Roll.prDelta,
                          FCaptureManager.CameraProperties.CameraCtl_Roll.prDefault,
                          FCaptureManager.CameraProperties.CameraCtl_Roll.prflags,
                          LONG(CameraControl_Roll));
           end;
       end;

    3: begin

         if FCaptureManager.CameraProperties.CameraCtl_Zoom.IsSupported then
           begin

             SetCbxValues(cbxCameraValues,
                          pIndex,
                          FCaptureManager.CameraProperties.CameraCtl_Zoom.prMax,
                          FCaptureManager.CameraProperties.CameraCtl_Zoom.prMin,
                          FCaptureManager.CameraProperties.CameraCtl_Zoom.prDelta,
                          FCaptureManager.CameraProperties.CameraCtl_Zoom.prDefault,
                          FCaptureManager.CameraProperties.CameraCtl_Zoom.prflags,
                          LONG(CameraControl_Zoom));
           end;
       end;

    4: begin

         if FCaptureManager.CameraProperties.CameraCtl_Exposure.IsSupported then
           begin
             SetCbxValues(cbxCameraValues,
                          pIndex,
                          FCaptureManager.CameraProperties.CameraCtl_Exposure.prMax,
                          FCaptureManager.CameraProperties.CameraCtl_Exposure.prMin,
                          FCaptureManager.CameraProperties.CameraCtl_Exposure.prDelta,
                          FCaptureManager.CameraProperties.CameraCtl_Exposure.prDefault,
                          FCaptureManager.CameraProperties.CameraCtl_Exposure.prflags,
                          LONG(CameraControl_Exposure));
           end;
       end;

    5: begin

         if FCaptureManager.CameraProperties.CameraCtl_Iris.IsSupported then
           begin
             SetCbxValues(cbxCameraValues,
                          pIndex,
                          FCaptureManager.CameraProperties.CameraCtl_Iris.prMax,
                          FCaptureManager.CameraProperties.CameraCtl_Iris.prMin,
                          FCaptureManager.CameraProperties.CameraCtl_Iris.prDelta,
                          FCaptureManager.CameraProperties.CameraCtl_Iris.prDefault,
                          FCaptureManager.CameraProperties.CameraCtl_Iris.prflags,
                          LONG(CameraControl_Iris));
           end;
       end;

    6: begin

         if FCaptureManager.CameraProperties.CameraCtl_Focus.IsSupported then
           begin

             SetCbxValues(cbxCameraValues,
                          pIndex,
                          FCaptureManager.CameraProperties.CameraCtl_Focus.prMax,
                          FCaptureManager.CameraProperties.CameraCtl_Focus.prMin,
                          FCaptureManager.CameraProperties.CameraCtl_Focus.prDelta,
                          FCaptureManager.CameraProperties.CameraCtl_Focus.prDefault,
                          FCaptureManager.CameraProperties.CameraCtl_Focus.prflags,
                          LONG(CameraControl_Focus));
           end;
       end;
  end;
end;


function TMainWindow.SetVideoProperties(pIndex: Integer): HRESULT;
begin

  Result := S_OK;

  if not Assigned(FCaptureManager) then
    Exit(E_POINTER);

  cbxVideoValues.Clear;
  cbxVideoValues.Items.Append('X');  // Not supported.
  cbxVideoValues.ItemIndex := 0;
  cbxVideoValues.Enabled := False;
  {
   In this order:
   Brightness = 0
   Contrast   = 1
   Hue        = 2
   Saturation = 3
   Sharpness  = 4
   Gamma      = 5
   ColorEnable = 6
   WhiteBalance = 7
   BacklightCompensation = 8
   Gain = 9
  }

  case pIndex of
    0: begin
         if FCaptureManager.VideoProperties.VideoCtl_Brightness.IsSupported then
           begin

             SetCbxValues(cbxVideoValues,
                          pIndex,
                          FCaptureManager.VideoProperties.VideoCtl_Brightness.prMax,
                          FCaptureManager.VideoProperties.VideoCtl_Brightness.prMin,
                          FCaptureManager.VideoProperties.VideoCtl_Brightness.prDelta,
                          FCaptureManager.VideoProperties.VideoCtl_Brightness.prDefault,
                          FCaptureManager.VideoProperties.VideoCtl_Brightness.prflags,
                          LONG(VideoProcAmp_Contrast));
           end;
       end;
    1: begin
         if FCaptureManager.VideoProperties.VideoCtl_Contrast.IsSupported then
           begin

             SetCbxValues(cbxVideoValues,
                          pIndex,
                          FCaptureManager.VideoProperties.VideoCtl_Contrast.prMax,
                          FCaptureManager.VideoProperties.VideoCtl_Contrast.prMin,
                          FCaptureManager.VideoProperties.VideoCtl_Contrast.prDelta,
                          FCaptureManager.VideoProperties.VideoCtl_Contrast.prDefault,
                          FCaptureManager.VideoProperties.VideoCtl_Brightness.prflags,
                          LONG(VideoProcAmp_Contrast));
           end;
       end;
    2: begin
         if FCaptureManager.VideoProperties.VideoCtl_Hue.IsSupported then
           begin

             SetCbxValues(cbxVideoValues,
                          pIndex,
                          FCaptureManager.VideoProperties.VideoCtl_Hue.prMax,
                          FCaptureManager.VideoProperties.VideoCtl_Hue.prMin,
                          FCaptureManager.VideoProperties.VideoCtl_Hue.prDelta,
                          FCaptureManager.VideoProperties.VideoCtl_Hue.prDefault,
                          FCaptureManager.VideoProperties.VideoCtl_Brightness.prflags,
                          LONG(VideoProcAmp_Contrast));
           end;
       end;
    3: begin
         if FCaptureManager.VideoProperties.VideoCtl_Saturation.IsSupported then
           begin

             SetCbxValues(cbxVideoValues,
                          pIndex,
                          FCaptureManager.VideoProperties.VideoCtl_Saturation.prMax,
                          FCaptureManager.VideoProperties.VideoCtl_Saturation.prMin,
                          FCaptureManager.VideoProperties.VideoCtl_Saturation.prDelta,
                          FCaptureManager.VideoProperties.VideoCtl_Saturation.prDefault,
                          FCaptureManager.VideoProperties.VideoCtl_Saturation.prflags,
                          LONG(VideoProcAmp_Saturation));
           end;
       end;
    4: begin
         if FCaptureManager.VideoProperties.VideoCtl_Sharpness.IsSupported then
           begin

             SetCbxValues(cbxVideoValues,
                          pIndex,
                          FCaptureManager.VideoProperties.VideoCtl_Sharpness.prMax,
                          FCaptureManager.VideoProperties.VideoCtl_Sharpness.prMin,
                          FCaptureManager.VideoProperties.VideoCtl_Sharpness.prDelta,
                          FCaptureManager.VideoProperties.VideoCtl_Sharpness.prDefault,
                          FCaptureManager.VideoProperties.VideoCtl_Sharpness.prflags,
                          LONG(VideoProcAmp_Sharpness));
           end;
       end;
    5: begin
         if FCaptureManager.VideoProperties.VideoCtl_Gamma.IsSupported then
           begin

             SetCbxValues(cbxVideoValues,
                          pIndex,
                          FCaptureManager.VideoProperties.VideoCtl_Gamma.prMax,
                          FCaptureManager.VideoProperties.VideoCtl_Gamma.prMin,
                          FCaptureManager.VideoProperties.VideoCtl_Gamma.prDelta,
                          FCaptureManager.VideoProperties.VideoCtl_Gamma.prDefault,
                          FCaptureManager.VideoProperties.VideoCtl_Gamma.prflags,
                          LONG(VideoProcAmp_Gamma));
           end;
       end;
    6: begin
         if FCaptureManager.VideoProperties.VideoCtl_ColorEnable.IsSupported then
           begin

             SetCbxValues(cbxVideoValues,
                          pIndex,
                          FCaptureManager.VideoProperties.VideoCtl_ColorEnable.prMax,
                          FCaptureManager.VideoProperties.VideoCtl_ColorEnable.prMin,
                          FCaptureManager.VideoProperties.VideoCtl_ColorEnable.prDelta,
                          FCaptureManager.VideoProperties.VideoCtl_ColorEnable.prDefault,
                          FCaptureManager.VideoProperties.VideoCtl_ColorEnable.prflags,
                          LONG(VideoProcAmp_ColorEnable));
           end;
       end;
    7: begin
         if FCaptureManager.VideoProperties.VideoCtl_WhiteBalance.IsSupported then
           begin

             SetCbxValues(cbxVideoValues,
                          pIndex,
                          FCaptureManager.VideoProperties.VideoCtl_WhiteBalance.prMax,
                          FCaptureManager.VideoProperties.VideoCtl_WhiteBalance.prMin,
                          FCaptureManager.VideoProperties.VideoCtl_WhiteBalance.prDelta,
                          FCaptureManager.VideoProperties.VideoCtl_WhiteBalance.prDefault,
                          FCaptureManager.VideoProperties.VideoCtl_WhiteBalance.prflags,
                          LONG(VideoProcAmp_WhiteBalance));
           end;
       end;
    8: begin
         if FCaptureManager.VideoProperties.VideoCtl_BacklightCompensation.IsSupported then
           begin

             SetCbxValues(cbxVideoValues,
                          pIndex,
                          FCaptureManager.VideoProperties.VideoCtl_BacklightCompensation.prMax,
                          FCaptureManager.VideoProperties.VideoCtl_BacklightCompensation.prMin,
                          FCaptureManager.VideoProperties.VideoCtl_BacklightCompensation.prDelta,
                          FCaptureManager.VideoProperties.VideoCtl_BacklightCompensation.prDefault,
                          FCaptureManager.VideoProperties.VideoCtl_BacklightCompensation.prflags,
                          LONG(VideoProcAmp_BacklightCompensation));
           end;
       end;
    9: begin
         if FCaptureManager.VideoProperties.VideoCtl_Gain.IsSupported then
           begin

             SetCbxValues(cbxVideoValues,
                          pIndex,
                          FCaptureManager.VideoProperties.VideoCtl_Gain.prMax,
                          FCaptureManager.VideoProperties.VideoCtl_Gain.prMin,
                          FCaptureManager.VideoProperties.VideoCtl_Gain.prDelta,
                          FCaptureManager.VideoProperties.VideoCtl_Gain.prDefault,
                          FCaptureManager.VideoProperties.VideoCtl_Gain.prflags,
                          LONG(VideoProcAmp_Gain));
           end;
       end;
  end;
end;


procedure TMainWindow.cbxCameraControlFlagsSelect(Sender: TObject);
begin

  SetCameraPropFlags();
  // Set changes.
  //
end;


procedure TMainWindow.cbxCameraControlPropertySelect(Sender: TObject);
begin
  if FAILED(SetCameraProperties(cbxCameraControlProperty.ItemIndex)) then
    MessageBox(0,
               lpcwstr('Could not get camera property, because the engine in not initialized.'),
               lpcwstr('Failure'),
               MB_ICONSTOP);
end;


procedure TMainWindow.cbxCameraValuesSelect(Sender: TObject);
var
  hr: HRESULT;

begin

  fSelectedCamaraControl.cpValue := StrToInt(cbxCameraValues.Text);
  SetCameraPropFlags();

  hr := FCaptureManager.SetCameraProps(fSelectedCamaraControl);

  if FAILED(hr) and (hr <> E_POINTER) then
    MessageBox(0,
               lpcwstr('Could not set camera property.'),
               lpcwstr('Failure'),
               MB_ICONSTOP);
end;


procedure TMainWindow.cbxVideoControlFlagsSelect(Sender: TObject);
begin

  if FAILED(SetVideoProperties(cbxVideoControlProperty.ItemIndex)) then
    MessageBox(0,
               lpcwstr('Could not get camera property, because the engine in not initialized.'),
               lpcwstr('Failure'),
               MB_ICONSTOP);
end;


procedure TMainWindow.cbxVideoValuesSelect(Sender: TObject);
var
  hr: HRESULT;

begin

  fSelectedVideoControl.cpValue := StrToInt(cbxVideoValues.Text);
  SetVideoPropFlags();

  hr := FCaptureManager.SetVideoProps(fSelectedVideoControl);

  if FAILED(hr) and (hr <> E_POINTER) then
    MessageBox(0,
               lpcwstr('Could not set video property.'),
               lpcwstr('Failure'),
                       MB_ICONSTOP);
end;

// Initialization and finalization =============================================

procedure TMainWindow.mnuCameraVideoSettingsClick(Sender: TObject);
begin

  if not pnlCameraAndVideoControl.Visible then
    pnlCameraAndVideoControl.Visible := True
  else
    pnlCameraAndVideoControl.Visible := False;
end;


initialization

  if FAILED(MFStartup(MF_VERSION,
                      MFSTARTUP_LITE)) then
      begin
        MessageBox(0,
                   lpcwstr('Your computer does not support this Media Foundation API version ' +
                           IntToStr(MF_VERSION) + '.'),
                   lpcwstr('MFStartup Failure!'),
                           MB_ICONSTOP);
      end;

finalization

  MFShutdown();

end.
