program MfCastPlayer;

uses
  Vcl.Forms,
  frmCastPlayer in 'frmCastPlayer.pas' {CastPlayerForm},
  dlgCaptureMonitor in 'dlgCaptureMonitor.pas' {CaptureMonitorDlg},
  LWFileBrowserExDlg in 'LWFileBrowserExDlg.pas' {DlgLWFileBrowserEx},
  MfYouTubeSourceResolver in 'MfYouTubeSourceResolver.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TCastPlayerForm, CastPlayerForm);
  Application.Run;
end.
