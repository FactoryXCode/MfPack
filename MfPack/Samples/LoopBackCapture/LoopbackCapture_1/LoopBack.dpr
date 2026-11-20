program LoopBack;

uses
  WinApi.ComBaseApi,
  WinApi.ActiveX.ObjBase,
  System.Win.ComObj,
  System.SysUtils,
{$IFDEF madExcept}
  madExcept,
  madLinkDisAsm,
  madListHardware,
  madListProcesses,
  madListModules,
  {$ENDIF}

  Vcl.Forms,
  frmWasapiLoopBack in 'frmWasapiLoopBack.pas' {frmLoopBackCapture},
  WasapiLoopback in 'WasapiLoopback.pas',
  Utils in 'Utils.pas',
  dlgDevices in 'dlgDevices.pas' {DevicesDlg},
  UniThreadTimer in 'UniThreadTimer.pas';

{$R *.res}

begin
  OleCheck(CoInitializeEx(nil,
                          COINIT_APARTMENTTHREADED));
  try
    Application.Initialize;
    Application.MainFormOnTaskbar := True;
    Application.CreateForm(TfrmLoopBackCapture, frmLoopBackCapture);
    Application.CreateForm(TDevicesDlg, DevicesDlg);
    if (ParamCount = 1) and (ParamStr(1) = '--smoke') then
      begin
        Application.ProcessMessages;
        frmLoopBackCapture.Close();
        Application.ProcessMessages;
        FreeAndNil(frmLoopBackCapture);
      end
    else
      Application.Run;
  finally
    CoUninitialize();
  end;
end.
