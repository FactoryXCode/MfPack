program SimplePlayer;

uses
  Vcl.Forms,
  frmSimplePlayer in 'frmSimplePlayer.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
