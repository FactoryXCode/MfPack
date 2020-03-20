program TMFPlayer;

uses
  Vcl.Forms,
  fMFPlayer in 'fMFPlayer.pas' {F_MFPlayer},
  fMFPlayerCore in 'fMFPlayerCore.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TF_MFPlayer, F_MFPlayer);
  Application.Run;
end.
