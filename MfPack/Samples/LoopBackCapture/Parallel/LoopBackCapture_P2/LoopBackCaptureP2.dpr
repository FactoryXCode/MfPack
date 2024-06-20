program LoopBackCaptureP2;

uses
  Vcl.Forms,
  frmLoopBackCapture in 'frmLoopBackCapture.pas' {frmMain},
  Common in 'Common.pas',
  ProcessInfoDlg in 'ProcessInfoDlg.pas' {dlgProcessInfo},
  LoopBackCapture in 'LoopBackCapture.pas',
  UniThreadTimer in 'UniThreadTimer.pas',
  Writer in 'Writer.pas';

{$R *.res}

begin

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TdlgProcessInfo, dlgProcessInfo);
  Application.Run;
end.
