program TMFPlayer;

uses
  {$IFDEF FASTMM}
  FastMM4,
  {$ENDIF}

  Vcl.Forms,
  frmMfPlayer in 'frmMfPlayer.pas' {frm_MfPlayer},
  dlgStreamSelect in 'dlgStreamSelect.pas' {dlgSelectStreams},
  FloatingFrm in 'FloatingFrm.pas' {FloatingForm},
  TimedTextClass in 'TimedTextClass.pas',
  LangTags in 'LangTags.pas',
  MfPlayerClassX in 'MfPlayerClassX.pas',
  MFTimerCallBackClass in 'MFTimerCallBackClass.pas',
  MfPCXConstants in 'MfPCXConstants.pas',
  dlgTimedTextLanguages in 'dlgTimedTextLanguages.pas' {DlgTimedTextLanguages},
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
  Application.MainFormOnTaskbar:= True;
  Application.Title := 'MFPlayer X';
  Application.CreateForm(Tfrm_MfPlayer, frm_MfPlayer);
  Application.CreateForm(TdlgSelectStreams, dlgSelectStreams);
  Application.CreateForm(TDlgTimedTextLanguages, dlgTimedTextLang);
  Application.Run;
end.
