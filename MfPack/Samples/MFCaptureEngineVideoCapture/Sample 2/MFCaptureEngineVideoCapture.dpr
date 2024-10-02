program MFCaptureEngineVideoCapture;

uses

  Vcl.Forms,
  frmMain in 'frmMain.pas' {MainWindow},
  dlgChooseDevice in 'dlgChooseDevice.pas' {ChooseDeviceDlg},
  Utils in 'Utils.pas',
  DeviceExplorer in 'DeviceExplorer.pas',
  SampleConverter in 'SampleConverter.pas',
  CaptureEngine in 'CaptureEngine.pas';

{$R *.res}

begin

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainWindow, MainWindow);
  Application.CreateForm(TChooseDeviceDlg, ChooseDeviceDlg);
  Application.Run;

end.
