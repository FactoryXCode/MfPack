program MfSimpleCaptureToFile;

uses
  Vcl.Forms,
  frmSimpleCaptureToFile in 'frmSimpleCaptureToFile.pas' {Frm_SimpleCapture},
  MfCaptureToFileClass in 'MfCaptureToFileClass.pas';

{$R *.res}

begin

  // Check for memoryleaks (debug mode (F9) only!)
  {$WARN SYMBOL_PLATFORM OFF}
  ReportMemoryLeaksOnShutdown := DebugHook <> 0;
  {$WARN SYMBOL_PLATFORM ON}

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'Simple Caption To File example, version 2.6.4';
  Application.CreateForm(TFrm_SimpleCapture, Frm_SimpleCapture);
  Application.Run;
end.
