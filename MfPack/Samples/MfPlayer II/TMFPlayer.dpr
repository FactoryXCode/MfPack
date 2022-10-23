program TMFPlayer;

uses

  {$IFDEF FASTMM}
  FastMM4,
  {$ENDIF}
  {$IFDEF madExcept}
  madExcept,
  madLinkDisAsm,
  madListHardware,
  madListProcesses,
  madListModules,
  {$ENDIF}

  Vcl.Forms,
  frmMfPlayer in 'frmMfPlayer.pas' {frm_MfPlayer},
  MfPlayerClass in 'MfPlayerClass.pas',
  frmFullScreenLayer in 'frmFullScreenLayer.pas' {FullScreenLayer},
  UniThreadTimer in 'UniThreadTimer.pas';

{$R *.res}

begin

  // Check for memoryleaks (debug mode (F9) only!)
{$IFNDEF madExcept}
  {$IFDEF DEBUG}
    {$WARN SYMBOL_PLATFORM OFF}
    ReportMemoryLeaksOnShutdown := (DebugHook <> 0);
    {$WARN SYMBOL_PLATFORM ON}
  {$ENDIF}
{$ENDIF}
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title:= 'MFPlayer II';
  Application.CreateForm(Tfrm_MfPlayer, frm_MfPlayer);
  Application.Run;
end.
