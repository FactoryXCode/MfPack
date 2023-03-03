program MfSimpleCapture;

uses
  {$IFDEF FASTMM}
  FastMM4,
  {$ENDIF }
  {$IFDEF madExcept}
  madExcept,
  madLinkDisAsm,
  madListHardware,
  madListProcesses,
  madListModules,
  {$ENDIF }
  Vcl.Forms,
  MfDeviceCaptureClass in 'MfDeviceCaptureClass.pas',
  frmSimpleCapture in 'frmSimpleCapture.pas' {Frm_SimpleCapture},
  frmdlgChooseDevice in 'frmdlgChooseDevice.pas' {dlgChooseDevice};

{$R *.res}

begin

  // Check for memoryleaks (debug mode (F9) only!)
{$IFNDEF madExcept}
  {$WARN SYMBOL_PLATFORM OFF}
  ReportMemoryLeaksOnShutdown := (DebugHook <> 0);
  {$WARN SYMBOL_PLATFORM ON}
{$ENDIF}
  Application.Initialize;
  Application.MainFormOnTaskbar:= True;
  Application.CreateForm(TFrm_SimpleCapture, Frm_SimpleCapture);
  Application.CreateForm(TdlgChooseDevice, dlgChooseDevice);
  Application.Run;
end.
