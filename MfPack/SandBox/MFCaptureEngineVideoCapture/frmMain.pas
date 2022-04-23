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
  Vcl.StdCtrls,
  {ActiveX}
  WinApi.ActiveX.ObjBase,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfApi,
  WinApi.MediaFoundationApi.MfIdl,
  WinApi.MediaFoundationApi.MfObjects,
  WinApi.MediaFoundationApi.MfUtils,
  {Application}
  CaptureEngine,
  dlgChooseDevice,
  Utils;


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
    ApplicationEvents: TApplicationEvents;
    butSaveToFile: TButton;
    Bevel2: TBevel;
    Button1: TButton;
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ApplicationEventsMessage(var Msg: tagMSG; var Handled: Boolean);
    procedure mnuChooseDeviceClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure mnuStartPreviewClick(Sender: TObject);
    procedure mnuTakePhotoClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure mnuStartRecordingClick(Sender: TObject);

  private
    { Private declarations }
    hPreview: HWND;
    bRecording: Boolean;
    bPreviewing: Boolean;
    //g_fSleepState: Boolean;


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
var
  psample: IMFSample;

begin

  if msg.hwnd = pnlPreview.Handle then
    begin
      Handled := True;
      Exit;
    end;

  // We did recieve a sample from TCaptureManager.TCaptureEngineOnSampleCallback.OnSample
  if msg.message = WM_RECIEVED_SAMPLE_FROM_CALLBACK then
    begin
      psample := IMFSample(@msg.lParam);

      // process the sample
      {ProcessSample(pSample); } {TODO Needs implementation -cGeneral : ActionItem}


      Handled := True;
      Exit;
    end;

  Handled := False;

end;


procedure TMainWindow.Button1Click(Sender: TObject);
begin
  mnuTakePhotoClick(Self);
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
  CoInitializeEx(nil,
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

   hr := TCaptureManager.CreateCaptureEngine(Handle,
                                             g_pEngine);
   if FAILED(hr) then
     goto Done;

   // Create DeviceParam class that holds the Activate pointers of the selected device.
   DeviceParam := TChooseDeviceParam.Create();

   hr := g_pEngine.InitializeCaptureManager(hPreview,
                                            DeviceParam.Device);
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
  i: Integer;
  hr: HResult;

label
  Done;

begin

  // To be sure the dialog is created
  if not Assigned(ChooseDeviceDlg) then
    begin
      Application.CreateForm(TChooseDeviceDlg, ChooseDeviceDlg);
      ChooseDeviceDlg.Visible := False;
    end;

  // Populate the listbox with camera's found
  // ========================================

  // Clear the combobox
  ChooseDeviceDlg.lbxDeviceList.Clear;
  // Fill the combobox with found capture devices
  for i := 0 to DeviceParam.Count - 1 do
    begin

      // Choose device index and set params
      DeviceParam.DeviceIndex := i;

      // Append the friendly name to the combobox.
      ChooseDeviceDlg.lbxDeviceList.Items.Append(DeviceParam.DeviceName);
      // Show the first in the list
      ChooseDeviceDlg.lbxDeviceList.ItemIndex := 0;
    end;

  // Ask the user to select one.
  if ChooseDeviceDlg.ShowModal = 1212 then
    begin
      hr := g_pEngine.InitializeCaptureManager(hPreview,
                                               DeviceParam.Device);
      if FAILED(hr) then
        goto Done;
    end;

Done:

{$IF DEBUG}
  if FAILED(hr) then
    OutputDebugString(PWideChar(format('Error: %s (hr = %d)', [IDTIMEOUT, hr])));
{$ENDIF}

  UpdateUI();

end;



procedure TMainWindow.UpdateUI();
var
  bEnableRecording: BOOL;
  bEnablePhoto: BOOL;

begin
  bEnablePhoto := False;
  bEnableRecording := False;

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
{$IF DEBUG}
  if FAILED(hr) then
    OutputDebugString(PWideChar(format('Error: %s (hr = %d)', [ERR_RECORD, hr])));
{$ENDIF}
  UpdateUI();
end;


procedure TMainWindow.mnuStartRecordingClick(Sender: TObject);
begin


  g_pEngine.StartRecord('');
end;


procedure TMainWindow.mnuTakePhotoClick(Sender: TObject);
var
  hr: HResult;

label
  Done;

begin



  hr := g_pEngine.TakePhoto(ssoCallBack);
  if FAILED(hr) then
    goto Done;


Done:

{$IF DEBUG}
  if FAILED(hr) then
     OutputDebugString(PWideChar(format('Error: %s (hr = %d)', [ERR_PHOTO, hr])));
{$ENDIF}
  UpdateUI();
end;


end.
