program MfSimpleCapture;

uses
  madExcept,
  madLinkDisAsm,
  madListHardware,
  madListProcesses,
  madListModules,
  Vcl.Forms,
  MfDeviceCaptureClass in 'MfDeviceCaptureClass.pas',
  frmSimpleCapture in 'frmSimpleCapture.pas' {Frm_SimpleCapture},
  frmdlgChooseDevice in 'frmdlgChooseDevice.pas' {dlgChooseDevice};

{$R *.res}

begin

  // Check for memoryleaks (debug mode (F9) only!)
  {$WARN SYMBOL_PLATFORM OFF}
  ReportMemoryLeaksOnShutdown := (DebugHook <> 0);
  {$WARN SYMBOL_PLATFORM ON}

  Application.Initialize;
  Application.MainFormOnTaskbar:= True;
  Application.CreateForm(TFrm_SimpleCapture, Frm_SimpleCapture);
  Application.Run;
end.
