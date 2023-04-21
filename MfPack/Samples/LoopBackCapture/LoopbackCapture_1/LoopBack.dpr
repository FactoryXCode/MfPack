program LoopBack;

uses
  {$IFDEF FASTMM}
  FastMM4,
  {$ENDIF}
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
  Utils in 'Utils.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmLoopBackCapture, frmLoopBackCapture);
  Application.Run;
end.
