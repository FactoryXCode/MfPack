program MfAudioClipEx;

uses
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
  Application.Title := 'MfAudioClipEx version 2.6.4';
  Application.CreateForm(TAudioClipExFrm, AudioClipExFrm);
  Application.Run;
end.
