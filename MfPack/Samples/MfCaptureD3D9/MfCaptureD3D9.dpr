program MfCaptureD3D9;

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
  frmMfCaptureD3D9 in 'frmMfCaptureD3D9.pas' {frmMain},
  dlgSelDevice in 'dlgSelDevice.pas' {dlgSelectDevice},
  Preview in 'Preview.pas',
  Device in 'Device.pas',
  VideoBufferLock in 'VideoBufferLock.pas';

{$R *.res}

begin

   // Check for memoryleaks (debug mode (F9) only!)
  {$IFDEF DEBUG}
    {$WARN SYMBOL_PLATFORM OFF}
    ReportMemoryLeaksOnShutdown := (DebugHook <> 0);
    {$WARN SYMBOL_PLATFORM ON}
  {$ENDIF}

  Application.Initialize;
  Application.MainFormOnTaskbar:= True;
  Application.Title := 'MFCaptureD3D9';
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;

end.
