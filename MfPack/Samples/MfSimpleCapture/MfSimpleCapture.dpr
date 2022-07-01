program MfSimpleCapture;

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
  MfDeviceCaptureClass in 'MfDeviceCaptureClass.pas',
  frmSimpleCapture in 'frmSimpleCapture.pas' {Frm_SimpleCapture},
  frmdlgChooseDevice in 'frmdlgChooseDevice.pas' {dlgChooseDevice};

{$R *.res}

begin

  // Check for memoryleaks (debug mode (F9) only!)
{$IFNDEF MAD}
  {$WARN SYMBOL_PLATFORM OFF}
  ReportMemoryLeaksOnShutdown := (DebugHook <> 0);
  {$WARN SYMBOL_PLATFORM ON}
{$ENDIF}
  Application.Initialize;
  Application.MainFormOnTaskbar:= True;
  Application.CreateForm(TFrm_SimpleCapture, Frm_SimpleCapture);
  Application.Run;
end.
