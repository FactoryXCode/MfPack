program MfCastPlayer;

uses
  Vcl.Forms,
  frmCastPlayer in 'frmCastPlayer.pas' {CastPlayerForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TCastPlayerForm, CastPlayerForm);
  Application.Run;
end.
