program LoopBack;

uses
  {$IFDEF FASTMM}
  FastMM4,
  {$ENDIF }
  Vcl.Forms,
  frmWasapiLoopBack in 'frmWasapiLoopBack.pas' {frmLoopBackCapture},
  WasapiLoopback in 'WasapiLoopback.pas',
  Utils in 'Utils.pas',
  dlgDevices in 'dlgDevices.pas' {DevicesDlg};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmLoopBackCapture, frmLoopBackCapture);
  Application.CreateForm(TDevicesDlg, DevicesDlg);
  Application.Run;
end.
