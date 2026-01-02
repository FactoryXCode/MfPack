program LoopbackCapture3;

uses
  Vcl.Forms,
  Common in 'Common.pas',
  UniThreadTimer in 'UniThreadTimer.pas',
  frmMain in 'frmMain.pas' {MainForm},
  LoopbackCapture in 'LoopbackCapture.pas',
  dlgDevices in 'dlgDevices.pas' {DevicesDlg},
  MfAudioWriter in 'MfAudioWriter.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TDevicesDlg, DevicesDlg);
  Application.Run;
end.
