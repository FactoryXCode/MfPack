program FxServeAdmin;

uses
  Vcl.Forms,
  frmFxServeAdmin in 'frmFxServeAdmin.pas' {FxServeAdminForm},
  dlgFxServePassword in 'dlgFxServePassword.pas' {FxServePasswordDlg};

{$R FxServeAdmin.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'FxServe Admin';
  Application.CreateForm(TfrmFxServeAdmin, FxServeAdminForm);
  Application.Run;
end.
