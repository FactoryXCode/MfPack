program MfTranscode;

uses
  {$IFDEF FASTMM}
  FastMM4,
  {$ENDIF }
  {$IFDEF MadExcept}
  madExcept,
  madLinkDisAsm,
  madListHardware,
  madListProcesses,
  madListModules,
  {$ENDIF }

  Vcl.Forms,
  frmTranscode in 'frmTranscode.pas' {frmTranscoder},
  Transcoder in 'Transcoder.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'MfTranscode version 3.0.2';
  Application.CreateForm(TfrmTranscoder, frmTranscoder);
  Application.Run;
end.
