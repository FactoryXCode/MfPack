program MfTranscode;

uses

  {$IFDEF FASTMM}
  FastMM4,
  {$ENDIF}

  Vcl.Forms,
  frmTranscode in 'frmTranscode.pas' {frmTranscoder},
  Transcoder in 'Transcoder.pas',
  Common in 'Common.pas',
  dlgAudioFormats in 'dlgAudioFormats.pas' {AudioFormatDlg},
  dlgVideoFormats in 'dlgVideoFormats.pas' {VideoFormatDlg};

{$R *.res}

begin

  {$IFDEF DEBUG}
  System.ReportMemoryLeaksOnShutdown := True;
  {$ENDIF}

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'MfTranscode version 3.1.6';
  Application.CreateForm(TfrmTranscoder, frmTranscoder);
  Application.CreateForm(TAudioFormatDlg, AudioFormatDlg);
  Application.CreateForm(TVideoFormatDlg, VideoFormatDlg);
  Application.Run;
end.
