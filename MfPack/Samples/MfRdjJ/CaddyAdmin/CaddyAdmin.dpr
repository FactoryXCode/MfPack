program CaddyAdmin;

uses
  Vcl.Forms,
  frmCaddyAdmin in 'frmCaddyAdmin.pas',
  dlgPassWord in 'dlgPassWord.pas' {PasswordDlg},
  frmCaddyConfigEditor in 'frmCaddyConfigEditor.pas' {frmCaddyConfigEditor};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'FactoryX Caddy Admin';
  Application.CreateForm(TfrmCaddyAdmin, CaddyAdminFrm);
  Application.Run;
end.

