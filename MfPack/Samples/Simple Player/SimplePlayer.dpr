program SimplePlayer;

uses

  {$IFDEF FASTMM}
  FastMM4,
  {$ENDIF }
  {$IFDEF MAD}
  madExcept,
  madLinkDisAsm,
  madListHardware,
  madListProcesses,
  madListModules,
  {$ENDIF }

  Vcl.Forms,
  frmSimplePlayer in 'frmSimplePlayer.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
