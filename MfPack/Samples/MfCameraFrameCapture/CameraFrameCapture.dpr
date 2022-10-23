program CameraFrameCapture;

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
  {$ENDIF }

  Vcl.Forms,
  Form.Main in 'Form.Main.pas' {FrmMain},
  Support in 'Support.pas',
  CameraCapture in 'CameraCapture.pas',
  MessageHandler in 'MessageHandler.pas',
  SampleConverter in 'SampleConverter.pas',
  CameraCapture.Asynchronous in 'CameraCapture.Asynchronous.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFrmMain, FrmMain);
  Application.Run;
end.
