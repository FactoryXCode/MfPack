program MfCastPlayer;

uses
  Vcl.Forms,
  frmCastPlayer in 'frmCastPlayer.pas' {SimpleCastPlayerForm},
  MfYouTubeSourceResolver in 'MfYouTubeSourceResolver.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TCastPlayerForm, CastPlayerForm);
  Application.Run;
end.
