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
  Utils in 'Utils.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmLoopBackCapture, frmLoopBackCapture);
  Application.Run;
end.
