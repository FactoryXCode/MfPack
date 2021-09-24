program ApplicationLoopBackAudio;

uses
  Vcl.Forms,
  frmMain in 'frmMain.pas' {MainForm},
  LoopbackCapture in 'LoopbackCapture.pas',
  Helpers in 'Helpers.pas',
  Common in 'Common.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
