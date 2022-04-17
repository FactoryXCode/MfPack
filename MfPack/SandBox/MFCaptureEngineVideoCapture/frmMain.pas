unit frmMain;

interface

uses
  {WinApi}
  Winapi.Windows,
  Winapi.Messages,
  WinApi.ComBaseApi,
  Winapi.ShlObj,
  Winapi.KnownFolders,
  {System}
  System.SysUtils,
  System.Variants,
  System.Classes,
  {VCL}
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.Menus,
  Vcl.AppEvnts,
  Vcl.ExtCtrls,
  Vcl.ComCtrls,
  {ActiveX}
  WinApi.ActiveX.ObjBase,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfApi,
  WinApi.MediaFoundationApi.MfIdl,
  WinApi.MediaFoundationApi.MfObjects,
  WinApi.MediaFoundationApi.MfUtils,
  {Application}
  CaptureEngine,
  dlgChooseDevice;

const
  IDTIMEOUT      = 'Unable to set the capture device.';
  ERR_INITIALIZE = 'Unable to initialize the capture engine.';
  ERR_PREVIEW    = 'An error occurred during preview.';
  ERR_RECORD     = 'An error occurred during recording.';
  ERR_CAPTURE    = 'An error occurred during capture.';
  ERR_PHOTO      = 'Unable to capture still photo.';



//function ChooseDeviceDlgProc(msg: TMessage): Integer;


type
  TMainWindow = class(TForm)
    MainMenu: TMainMenu;
    Capture1: TMenuItem;
    mnuStartPreview: TMenuItem;
    mnuChooseDevice: TMenuItem;
    mnuStartRecording: TMenuItem;
    mnuTakePhoto: TMenuItem;
    SaveFileDialog: TSaveDialog;
    Bevel1: TBevel;
    N1: TMenuItem;
    Exit1: TMenuItem;
    StatusBar: TStatusBar;
    pnlPreview: TPanel;
    pnlSnapShot: TPanel;
    pbCapture: TPaintBox;
    ApplicationEvents: TApplicationEvents;
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ApplicationEventsMessage(var Msg: tagMSG; var Handled: Boolean);
    procedure mnuChooseDeviceClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure mnuStartPreviewClick(Sender: TObject);
    procedure mnuTakePhotoClick(Sender: TObject);
  private
    { Private declarations }
    hPreview: HWND;
    hStatus: HWND;
    bRecording: Boolean;
    bPreviewing: Boolean;
    pSelectedDevice: IMFActivate;

    g_fSleepState: Boolean;

    // Messages
    procedure OnSize(var message: TWMSize); message WM_SIZE;

    // Update menuitems and status
    procedure UpdateUI();

  public
    { Public declarations }

  end;

var
  MainWindow: TMainWindow;

implementation

{$R *.dfm}


// MEssages

// OnResize
procedure TMainWindow.OnSize(var message: TWMSize);
begin
  inherited;
  //
end;



procedure TMainWindow.ApplicationEventsMessage(var Msg: tagMSG;
                                               var Handled: Boolean);
begin

  //if msg.hwnd = MainWindow.Handle then
  //  begin
      if msg.message = WM_CHOOSE_DEVICEDLG_ITEMINDEX then
        begin
           DeviceParam.selection := msg.lParam;
           DeviceParam.SelectedDeviceName := LPWSTR(msg.WParam);
           Handled := True;
           Exit;
        end;
   // end;

  if msg.hwnd = pnlPreview.Handle then
    begin
      Handled := True;
      Exit;
    end;

  Handled := False;

end;


procedure TMainWindow.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  FreeAndNil(DeviceParam);
end;

procedure TMainWindow.FormCreate(Sender: TObject);
var
  hr: HResult;

label
  Done;

begin
  // Initialize COM
  CoInitializeEx(Nil,
                 COINIT_APARTMENTTHREADED);

  // Startup Media Foundation
  if FAILED(MFStartup(MF_VERSION, 0)) then
    begin
      MessageBox(0,
                 lpcwstr('Your computer does not support this Media Foundation API version' + IntToStr(MF_VERSION) + '.'),
                 lpcwstr('MFStartup Failure!'),
                 MB_ICONSTOP);
      Application.Terminate;
   end;

   hPreview := pnlPreview.Handle;

   // Create the captureengine

   hr := TCaptureManager.CreateInstance(Self.Handle,
                                        g_pEngine);
   if FAILED(hr) then
     goto Done;

   hr := g_pEngine.InitializeCaptureManager(hPreview,
                                            pSelectedDevice);
   if FAILED(hr) then
     begin

{$IF DEBUG}
       OutputDebugString(format('Error: %s (hr = %d)', [IDS_ERR_SET_DEVICE, hr]));
{$ENDIF}

       goto Done;
     end;

  UpdateUI();

Done:

  if FAILED(hr) then
    Application.Terminate;

end;


procedure TMainWindow.FormDestroy(Sender: TObject);
begin
  SafeDelete(g_pEngine);
  PostQuitMessage(0);
  MFShutdown();
  CoUnInitialize();
end;


procedure TMainWindow.mnuChooseDeviceClick(Sender: TObject);
var
  pAttributes: IMFAttributes;
  iDevice: UINT32;
  i: Integer;
  uiNameLen: UINT32;
  szName: LPWSTR;
  hr: HResult;

label
  Done;

begin


  hr := MFCreateAttributes(pAttributes,
                           1);
  if FAILED(hr) then
    goto Done;

  // create ChooseDeviceParam class
  DeviceParam := TChooseDeviceParam.Create();

  // Ask for source type = video capture devices
  hr := pAttributes.SetGUID(MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE,
                            MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE_VIDCAP_GUID);
  if FAILED(hr) then
    goto Done;

  // Enumerate devices.
  hr := MFEnumDeviceSources(pAttributes,
                            DeviceParam.ppDevices,
                            DeviceParam.count);
  if FAILED(hr) then
    goto Done;

 // Populate the listbox with camera's found
  if Assigned(ChooseDeviceDlg) then
    begin
      // Clear the combobox
      ChooseDeviceDlg.lbxDeviceList.Clear;
      // Fill the combobox with found capture devices
      for i := 0 to DeviceParam.count - 1 do
        begin
          // Try to get the display-friendly-name.
{$POINTERMATH ON}
          hr := DeviceParam.ppDevices[i].GetAllocatedString(MF_DEVSOURCE_ATTRIBUTE_FRIENDLY_NAME,
                                                            szName,
                                                            uiNameLen);
{$POINTERMATH OFF}
          // Append the friendly name to the combobox.
          ChooseDeviceDlg.lbxDeviceList.Items.Append(szName);
          // Show the first in the list
          ChooseDeviceDlg.lbxDeviceList.ItemIndex:= 0;
        end;
    end;

  // Ask the user to select one.
  if ChooseDeviceDlg.ShowModal = 1212 then
    begin
      //OutputDebugString(format('Error: %s (hr = %d)', ['', hr]));
      Application.ProcessMessages;
      iDevice := DeviceParam.selection;
    end;


        {INT_PTR result = DialogBoxParam(GetModuleHandle(NULL),
            MAKEINTRESOURCE(IDD_CHOOSE_DEVICE), hwnd,
            ChooseDeviceDlgProc, (LPARAM)&param);

        if ((result == IDOK) && (param.selection != (UINT32)-1))
        {
            UINT iDevice = param.selection;

            if (iDevice >= param.count)
            {
                hr = E_UNEXPECTED;
                goto done;
            }
  {$POINTERMATH ON}
  hr := g_pEngine.InitializeCaptureManager(hPreview,
                                           DeviceParam.ppDevices[iDevice]);
  if FAILED(hr) then
    goto Done;

  SafeRelease(pSelectedDevice);

  pSelectedDevice := DeviceParam.ppDevices[iDevice];

  {$POINTERMATH OFF}

done:
  SafeRelease(&pAttributes);
{$IF DEBUG}
  if FAILED(hr) then
    OutputDebugString(format('Error: %s (hr = %d)', [IDS_ERR_SET_DEVICE, hr]));

{$ENDIF}

  UpdateUI();

end;


procedure TMainWindow.UpdateUI();
var
  bEnableRecording: BOOL;
  bEnablePhoto: BOOL;

begin
  if not g_pEngine.IsRecording then
    begin
      bRecording := g_pEngine.IsRecording;

      if bRecording then
        mnuStartRecording.Caption := 'Stop Recording'
      else
        mnuStartRecording.Caption := 'Start Recording';
    end;

  if g_pEngine.IsPreviewing then
    begin
      bPreviewing := g_pEngine.IsPreviewing;
      if bPreviewing then
        begin
          mnuStartPreview.Caption := 'Stop Preview';
          bEnableRecording := True;
          bEnablePhoto := True;
        end
      else
        mnuStartPreview.Caption := 'Start Preview';
    end;

  if bRecording then
    StatusBar.SimpleText := 'Recording'
  else if g_pEngine.IsPreviewing then
    StatusBar.SimpleText := 'Previewing'
  else
    begin
      StatusBar.SimpleText := 'Please select a device or start preview (using the default device).';
      bEnableRecording := False;
    end;

  if (g_pEngine.IsPreviewing or g_pEngine.IsPhotoPending) then
    bEnablePhoto := False;

  mnuStartRecording.Enabled := bEnableRecording;
  mnuTakePhoto.Enabled := bEnablePhoto;

end;

procedure TMainWindow.mnuStartPreviewClick(Sender: TObject);
var
  hr: HResult;
begin
  hr := g_pEngine.StartPreview();

  if FAILED(hr) then
    OutputDebugString(PWIDECHAR(format('Error: %s (hr = %d)', [ERR_RECORD, hr])));

  UpdateUI();
end;

procedure TMainWindow.mnuTakePhotoClick(Sender: TObject);
var
  wsfilename: WideChar;
  psi: IShellItem;
  pszFolderPath: PWideChar;
  ftime: SYSTEMTIME;
  cpath: PChar;
  hr: HResult;

label
  Done;

begin

  hr := g_pEngine.TakePhoto(snsOptions: TSnapShotOptions);
  if FAILED(hr) then
    goto Done;


Done:

  CoTaskMemFree(pszFolderPath);
                                              `
       // if (FAILED(hr))
        {
            ShowError(hwnd, IDS_ERR_PHOTO, hr);
        }
  UpdateUI();
end;

end;

end.
