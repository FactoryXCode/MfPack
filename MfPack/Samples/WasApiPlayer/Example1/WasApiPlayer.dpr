program WasApiPlayer;

uses
  Vcl.Forms,
  MainFrm in 'MainFrm.pas' {MainForm},
  Tools in 'Tools.pas',
  Vcl.Themes,
  Vcl.Styles,
  WASAPIEngine in 'WASAPIEngine.pas',
  UniThreadTimer in 'UniThreadTimer.pas';

{$R *.res}

begin

  {$IFDEF DEBUG}
  {$WARN SYMBOL_PLATFORM OFF}
  ReportMemoryLeaksOnShutdown := True;
  {$ENDIF}

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
