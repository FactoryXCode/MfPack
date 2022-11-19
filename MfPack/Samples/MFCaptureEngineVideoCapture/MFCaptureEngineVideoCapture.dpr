program MFCaptureEngineVideoCapture;

uses
  {$IFDEF madExcept}
  madExcept,
  madLinkDisAsm,
  madListHardware,
  madListProcesses,
  madListModules,
  {$ENDIF }


  Vcl.Forms,
  frmMain in 'frmMain.pas' {MainWindow},
  CaptureEngine in 'CaptureEngine.pas',
  dlgChooseDevice in 'dlgChooseDevice.pas' {ChooseDeviceDlg},
  Utils in 'Utils.pas',
  DeviceExplorer in 'DeviceExplorer.pas',
  SampleConverter in 'SampleConverter.pas';

{$R *.res}

begin

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainWindow, MainWindow);
  Application.Run;

end.
