program MfFrameCapture;

uses
  {$IFDEF FASTMM}
  FastMM4,
  {$ENDIF }
  Vcl.Forms,
  Form.Main in 'Form.Main.pas' {FrmMain},
  Support in 'Support.pas',
  SampleConverter in 'SampleConverter.pas',
  SourceReaderCallback in 'SourceReaderCallback.pas',
  MessageHandler in 'MessageHandler.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFrmMain, FrmMain);
  Application.Run;
end.
