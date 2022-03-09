program FMPlayer;

uses

  {$IFDEF FASTMM}
  FastMM4,
  {$ELSE IFDEF madExcept}
  madExcept,
  madLinkDisAsm,
  madListHardware,
  madListProcesses,
  madListModules,
  {$ENDIF}

  System.StartUpCopy,
  FMX.Forms,
  FMX.MediaFoundation in 'FMX.MediaFoundation.pas',
  UMain in 'UMain.pas' {frmMain};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
