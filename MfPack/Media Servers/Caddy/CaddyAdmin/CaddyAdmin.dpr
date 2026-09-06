program CaddyAdmin;

uses
  Vcl.Forms,
  frmCaddyAdmin in 'frmCaddyAdmin.pas' {CaddyAdminFrm},
  dlgPassWord in 'dlgPassWord.pas' {PasswordDlg},
  frmCaddyConfigEditor in 'frmCaddyConfigEditor.pas' {frmCaddyConfigEditor},
  dlgLanDiscovery in 'dlgLanDiscovery.pas' {LanDiscoveryDialog},
  MacVendorDb in 'MacVendorDb.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'Caddy Admin';
  Application.CreateForm(TfrmCaddyAdmin, CaddyAdminFrm);
  Application.Run;
end.

