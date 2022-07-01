program MfAudioClipEx;

uses
  {$IFDEF FASTMM}
  FastMM4,
  {$ENDIF}
  {$IFDEF MAD}
  madExcept,
  madLinkDisAsm,
  madListHardware,
  madListProcesses,
  madListModules,
  {$ENDIF}
  Vcl.Forms,
  AudioClipEngine in 'AudioClipEngine.pas',
  frmAudioClipEx in 'frmAudioClipEx.pas' {AudioClipExFrm},
  Helpers in 'Helpers.pas';

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
  Application.Title := 'MfAudioClipEx version 3.0.0';
  Application.CreateForm(TAudioClipExFrm, AudioClipExFrm);
  Application.Run;
end.
