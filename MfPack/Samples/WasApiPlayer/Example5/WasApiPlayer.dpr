program WasApiPlayer;

uses
  Vcl.Forms,
  MainFrm in 'MainFrm.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
