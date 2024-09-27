program MfSimpleCapture;

uses

  Vcl.Forms,
  MfDeviceCaptureClass in 'MfDeviceCaptureClass.pas',
  frmSimpleCapture in 'frmSimpleCapture.pas' {Frm_SimpleCapture},
  frmdlgChooseDevice in 'frmdlgChooseDevice.pas' {dlgChooseDevice};

{$R *.res}

begin

  {$IFDEF DEBUG}
  // Check for memoryleaks (debug mode (F9) only!)
  {$WARN  SYMBOL_PLATFORM OFF}
  ReportMemoryLeaksOnShutdown := (DebugHook <> 0);
  {$WARN SYMBOL_PLATFORM ON}
  {$ENDIF}
  Application.Initialize;
  Application.MainFormOnTaskbar:= True;
  Application.CreateForm(TFrm_SimpleCapture, Frm_SimpleCapture);
  Application.CreateForm(TdlgChooseDevice, dlgChooseDevice);
  Application.Run;
end.
