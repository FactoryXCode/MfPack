program MfCaptureToFile;

uses
  {$IFDEF FASTMM}
  FastMM4,
  {$ENDIF}
  Vcl.Forms,
  frmMain in 'frmMain.pas' {Form1},
  Capture in 'Capture.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TdlgMfCaptureToFile, dlgMfCaptureToFile);
  Application.Run;
end.
