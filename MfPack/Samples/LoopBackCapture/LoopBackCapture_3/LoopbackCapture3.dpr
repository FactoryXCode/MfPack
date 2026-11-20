program LoopbackCapture3;

uses
  WinApi.ComBaseApi,
  WinApi.ActiveX.ObjBase,
  System.Win.ComObj,
  System.SysUtils,
Vcl.Forms,
  Common in 'Common.pas',
  UniThreadTimer in 'UniThreadTimer.pas',
  frmMain in 'frmMain.pas' {MainForm},
  LoopbackCapture in 'LoopbackCapture.pas',
  dlgDevices in 'dlgDevices.pas' {DevicesDlg},
  MfAudioWriter in 'MfAudioWriter.pas';

{$R *.res}

begin
  OleCheck(CoInitializeEx(nil,
                          COINIT_APARTMENTTHREADED));
  try
    Application.Initialize;
    Application.MainFormOnTaskbar := True;
    Application.CreateForm(TMainForm, MainForm);
    Application.CreateForm(TDevicesDlg, DevicesDlg);
    if (ParamCount = 1) and (ParamStr(1) = '--smoke') then
      begin
        Application.ProcessMessages;
        MainForm.Close();
        Application.ProcessMessages;
        FreeAndNil(MainForm);
      end
    else
      Application.Run;
  finally
    CoUninitialize();
  end;
end.
