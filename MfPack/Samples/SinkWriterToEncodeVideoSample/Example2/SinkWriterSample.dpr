program SinkWriterSample;

uses
  madExcept,
  madLinkDisAsm,
  madListHardware,
  madListProcesses,
  madListModules,
  Vcl.Forms,
  frmMain in 'frmMain.pas' {MainForm},
  SinkWriterClass in 'SinkWriterClass.pas',
  dlgVideoOutput in 'dlgVideoOutput.pas' {dlgVideoSetttings},
  Utils in 'Utils.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TdlgVideoSetttings, dlgVideoSetttings);
  Application.Run;
end.
