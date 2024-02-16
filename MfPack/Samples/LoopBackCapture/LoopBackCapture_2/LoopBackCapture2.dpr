program LoopBackCapture2;

uses
  {$IFDEF FASTMM}
  FastMM4,
  {$ENDIF}

  Vcl.Forms,
  frmLoopBackCapture in 'frmLoopBackCapture.pas' {frmMain},
  Common in 'Common.pas',
  LoopBackCapture in 'LoopBackCapture.pas',
  ProcessInfoDlg in 'ProcessInfoDlg.pas' {dlgProcessInfo};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TdlgProcessInfo, dlgProcessInfo);
  Application.Run;
end.
