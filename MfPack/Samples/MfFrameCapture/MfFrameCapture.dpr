program MfFrameCapture;
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
  Form.Main in 'Form.Main.pas' {FrmMain},
  Support in 'Support.pas',
  SampleConverter in 'SampleConverter.pas',
  MessageHandler in 'MessageHandler.pas',
  FileCapture.Asynchronous in 'FileCapture.Asynchronous.pas',
  FileCapture.Synchronous in 'FileCapture.Synchronous.pas',
  FileCapture in 'FileCapture.pas';

{$R *.res}
begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFrmMain, FrmMain);
  Application.Run;
end.
