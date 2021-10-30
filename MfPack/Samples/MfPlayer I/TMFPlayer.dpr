program TMFPlayer;

uses
  Vcl.Forms,
  frmMfPlayer in 'frmMfPlayer.pas' {frm_MfPlayer},
  MfPlayerClass in 'MfPlayerClass.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar:= True;
  Application.Title:= 'MFPlayer I';
  Application.CreateForm(Tfrm_MfPlayer, frm_MfPlayer);
  Application.Run;
end.
