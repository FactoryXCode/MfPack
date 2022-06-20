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
    butSaveToFile: TButton;
    Bevel2: TBevel;
    Button1: TButton;
    ApplicationEvents: TApplicationEvents;
    procedure FormCreate(Sender: TObject);
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

    // Messages
    procedure OnSize(var message: TWMSize); message WM_SIZE;
    // Update menuitems and status
    procedure UpdateUI();
    // Catches all messages to this object.
   // procedure HandleCaptureEngineMessages(var Msg: TMsg;
   //                                       var Handled: Boolean);

  public
    { Public declarations }
    procedure WndProc(var Msg: TMessage); override;
  end;

var
  MainWindow: TMainWindow;

implementation

{$R *.dfm}


// MEssages

// OnResize
procedure TMainWindow.OnSize(var message: TWMSize);
var
  crD: MFVideoNormalizedRect;
  pcrD: PMFVideoNormalizedRect;

begin
  // Set video size
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


{procedure TMainWindow.HandleCaptureEngineMessages(var Msg: TMsg;
                                                  var Handled: Boolean);
var
  psample: IMFSample;
  wp: Pointer;
  lp: Pointer;

begin

try

  case Msg.message of

    // We did recieve a sample from TCaptureManager.TCaptureEngineOnSampleCallback.OnSample
    WM_RECIEVED_SAMPLE_FROM_CALLBACK:
      begin
        psample := IMFSample(@Msg.WParam);

        // process the sample
        // ProcessSample(pSample); } {TODO Needs implementation -cGeneral : ActionItem
        Handled := True;
      end;

    WM_APP_CAPTURE_EVENT:
      begin
        wp := Pointer(Msg.WParam);
        lp := Pointer(Msg.LParam);
        Handled := True;
       // FCaptureManager.OnCaptureEvent(wp,
       //                                lp);

      end;
  end;


except
  on E : Exception do
   ShowMessage(format('%s error raised, with message %s',
                      [E.ClassName, E.Message]));
end;
end; }



procedure TMainWindow.WndProc(var Msg: TMessage);
var
  psample: IMFSample;

begin

try
try

  case Msg.Msg of

    // We did recieve a sample from TCaptureManager.TCaptureEngineOnSampleCallback.OnSample
    WM_RECIEVED_SAMPLE_FROM_CALLBACK:
      begin
        psample := IMFSample(Pointer(Msg.WParam));

        // process the sample
        // ProcessSample(pSample);  {TODO Needs implementation -cGeneral : ActionItem

      end;

    WM_APP_CAPTURE_EVENT:
      begin
        FCaptureManager.OnCaptureEvent(Msg.WParam,
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
except
  on E : Exception do
   ShowMessage(format('%s error raised, with message %s',
                      [E.ClassName, E.Message]));
end;
finally
  inherited WndProc(Msg);
end;
end;



procedure TMainWindow.Button1Click(Sender: TObject);
begin
  mnuTakePhotoClick(Self);
end;


procedure TMainWindow.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := False;
  FreeAndNil(FDeviceParam);
  SafeDelete(FCaptureManager);
  PostQuitMessage(0);
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
                                             FCaptureManager);
   if FAILED(hr) then
     goto Done;

   // Create DeviceParam class that holds the Activate pointers of the selected device.
   FDeviceParam := TChooseDeviceParam.Create();

   hr := FCaptureManager.InitializeCaptureManager(hPreview, Handle,
                                                  FDeviceParam.Device);
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


procedure TMainWindow.mnuChooseDeviceClick(Sender: TObject);
var
  i: Integer;
  hr: HResult;

label
  Done;

begin

  if FCaptureManager.IsPreviewing then
    FCaptureManager.StopPreview;
  if FCaptureManager.IsRecording then
    FCaptureManager.StopRecord;

  HandleThreadMessages(GetCurrentThread());
  Sleep(1000);
  //UpdateUI();

  // To be sure the dialog is created
  if not Assigned(ChooseDeviceDlg) then
    begin
      Application.CreateForm(TChooseDeviceDlg, ChooseDeviceDlg);
      ChooseDeviceDlg.Visible := False;
    end;

  // Destroy and Create DeviceParam class that holds the Activate pointers of the selected device.
  if Assigned(FDeviceParam) then
    SafeDelete(FDeviceParam);
  FDeviceParam := TChooseDeviceParam.Create();

  // Populate the listbox with camera's found
  // ========================================

  // Clear the combobox
  ChooseDeviceDlg.lbxDeviceList.Clear;
  // Fill the combobox with found capture devices
  for i := 0 to FDeviceParam.Count - 1 do
    begin

      // Choose device index and set params
      FDeviceParam.DeviceIndex := i;

      // Append the friendly name to the combobox.
      ChooseDeviceDlg.lbxDeviceList.Items.Append(FDeviceParam.DeviceName);
      // Show the first in the list
      ChooseDeviceDlg.lbxDeviceList.ItemIndex := 0;
    end;

  // Ask the user to select one.
  if ChooseDeviceDlg.ShowModal = 1212 then
    begin
      hPreview := pnlPreview.Handle;

      hr := FCaptureManager.InitializeCaptureManager(hPreview,
                                                     Handle,
                                                     FDeviceParam.Device);
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
        mnuStartPreview.Caption := 'Start Preview';
    end;

  if bRecording then
    StatusBar.SimpleText := 'Recording'
  else if FCaptureManager.IsPreviewing then
    StatusBar.SimpleText := 'Previewing'
  else
    begin
      StatusBar.SimpleText := 'Please select a device or start preview (using the default device).';
      bEnableRecording := False;
    end;

  if (FCaptureManager.IsPreviewing or FCaptureManager.IsPhotoPending) then
    bEnablePhoto := False;

  mnuStartRecording.Enabled := bEnableRecording;
  mnuTakePhoto.Enabled := bEnablePhoto;

end;


procedure TMainWindow.mnuStartPreviewClick(Sender: TObject);
var
  hr: HResult;

begin
  hr := FCaptureManager.StartPreview();
{$IF DEBUG}
  if FAILED(hr) then
    OutputDebugString(PWideChar(format('Error: %s (hr = %d)', [ERR_RECORD, hr])));
{$ENDIF}
  if FAILED(hr) then
    Exit;
  UpdateUI();
end;


procedure TMainWindow.mnuStartRecordingClick(Sender: TObject);
begin

  FCaptureManager.StartRecord('');

end;


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

{$IF DEBUG}
  if FAILED(hr) then
     OutputDebugString(PWideChar(format('Error: %s (hr = %d)', [ERR_PHOTO, hr])));
{$ENDIF}
  UpdateUI();
end;


end.
