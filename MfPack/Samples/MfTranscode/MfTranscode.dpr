program MfTranscode;

uses
  Vcl.Forms,
  frmTranscode in 'frmTranscode.pas' {frmTranscoder},
  Transcoder in 'Transcoder.pas',
  Helpers in 'Helpers.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'MfTranscode version 2.6.4';
  Application.CreateForm(TfrmTranscoder, frmTranscoder);
  Application.Run;
end.
