program ImagesToVideo;

uses
  Vcl.Forms,
  ImageRenderer in 'ImageRenderer.pas',
  frmMain in 'frmMain.pas' {FfrmMain},
  Common in 'Common.pas',
  Scale in 'Scale.pas',
  Tools in 'Tools.pas',
  Transformer in 'Transformer.pas',
  dlgAudioFormats in 'dlgAudioFormats.pas' {AudioFormatDlg};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, FfrmMain);
  Application.CreateForm(TAudioFormatDlg, AudioFormatDlg);
  Application.Run;
end.
