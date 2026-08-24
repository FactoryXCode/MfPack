program LoopBack;

uses

  madExcept,
  madLinkDisAsm,
  madListHardware,
  madListProcesses,
  madListModules,

  Vcl.Forms,
  frmWasapiLoopBack in 'frmWasapiLoopBack.pas' {frmLoopBackCapture},
  WasapiLoopback in 'WasapiLoopback.pas',
  Utils in 'Utils.pas',
  dlgDevices in 'dlgDevices.pas' {DevicesDlg},
  UniThreadTimer in 'UniThreadTimer.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmLoopBackCapture, frmLoopBackCapture);
  Application.CreateForm(TDevicesDlg, DevicesDlg);
  Application.Run;
end.
