program TMFPlayer;

uses
  Vcl.Forms,
  frmMfPlayer in 'frmMfPlayer.pas' {frm_MfPlayer},
  MfPlayerClass in 'MfPlayerClass.pas',
  frmFullScreenLayer in 'frmFullScreenLayer.pas' {FullScreenLayer},
  UniThreadTimer in 'UniThreadTimer.pas';

{$R *.res}

begin

  // Check for memoryleaks (debug mode (F9) only!)
  {$IFDEF DEBUG}
    {$WARN SYMBOL_PLATFORM OFF}
    ReportMemoryLeaksOnShutdown := (DebugHook <> 0);
    {$WARN SYMBOL_PLATFORM ON}
  {$ENDIF}

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title:= 'MFPlayer II';
  Application.CreateForm(Tfrm_MfPlayer, frm_MfPlayer);
  Application.Run;
end.
