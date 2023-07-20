program MfTranscode;

uses
  {$IFDEF FASTMM}
  FastMM4,
  madExcept,
  madLinkDisAsm,
  madListHardware,
  madListProcesses,
  madListModules,
  {$ENDIF }
  Vcl.Forms,
  frmTranscode in 'frmTranscode.pas' {frmTranscoder},
  Transcoder in 'Transcoder.pas',
  Common in 'Common.pas',
  dlgAudioFormats in 'dlgAudioFormats.pas' {AudioFormatDlg};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'MfTranscode version 3.1.5';
  Application.CreateForm(TfrmTranscoder, frmTranscoder);
  Application.CreateForm(TAudioFormatDlg, AudioFormatDlg);
  Application.Run;
end.
