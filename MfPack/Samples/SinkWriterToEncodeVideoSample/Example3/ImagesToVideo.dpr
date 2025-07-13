program ImagesToVideo;

uses
  Vcl.Forms,
  ImageRenderer in 'ImageRenderer.pas',
  frmMain in 'frmMain.pas' {FfrmMain},
  Common in 'Common.pas',
  Scale in 'Scale.pas',
  Tools in 'Tools.pas',
  Transformer in 'Transformer.pas',
  dlgAudioFormats in 'dlgAudioFormats.pas' {AudioFormatDlg},
  frmAdvanced in 'frmAdvanced.pas' {FfrmAdvanced};

{$R *.res}

begin
  {$IFDEF DEBUG}
  {$WARN SYMBOL_PLATFORM OFF}
  ReportMemoryLeaksOnShutdown := True;
  {$ENDIF}
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, FfrmMain);
  Application.CreateForm(TAudioFormatDlg, AudioFormatDlg);
  Application.CreateForm(TFfrmAdvanced, FfrmAdvanced);
  Application.Run;
end.
