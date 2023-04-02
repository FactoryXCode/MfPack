program LoopBackCapture2;

uses
  madExcept,
  madLinkDisAsm,
  madListHardware,
  madListProcesses,
  madListModules,
  Vcl.Forms,
  frmLoopBackCapture in 'frmLoopBackCapture.pas' {frmMain},
  Common in 'Common.pas',
  LoopBackCapture in 'LoopBackCapture.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
