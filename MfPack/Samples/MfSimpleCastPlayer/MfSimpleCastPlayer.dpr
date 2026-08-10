program MfSimpleCastPlayer;

uses
  Vcl.Forms,
  frmSimpleCastPlayer in 'frmSimpleCastPlayer.pas' {SimpleCastPlayerForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TSimpleCastPlayerForm, SimpleCastPlayerForm);
  Application.Run;
end.
