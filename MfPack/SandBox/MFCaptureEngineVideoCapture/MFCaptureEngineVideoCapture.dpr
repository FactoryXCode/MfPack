program MFCaptureEngineVideoCapture;

uses
  {$IFDEF FASTMM}
  FastMM4,
  {$ELSE}
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
  SampleConverter_V1 in 'SampleConverter_V1.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainWindow, MainWindow);
  Application.CreateForm(TChooseDeviceDlg, ChooseDeviceDlg);
  Application.Run;
end.
