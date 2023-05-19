program SinkWriterSample;
uses
  {$IFDEF FASTMM}
  FastMM4,
  {$ENDIF }
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
