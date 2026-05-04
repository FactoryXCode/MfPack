program CaptureVideoFromGPU;

uses
  madExcept,
  madLinkDisAsm,
  madListHardware,
  madListProcesses,
  madListModules,
  Vcl.Forms,
  frmCaptureVideoFromGpu in 'frmCaptureVideoFromGpu.pas' {FrmCapture},
  CaptureStreamEngine in 'CaptureStreamEngine.pas',
  Helpers in 'Helpers.pas',
  PreviewRenderer in 'PreviewRenderer.pas',
  GpuNV12Converter in 'GpuNV12Converter.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFrmCapture, FrmCapture);
  Application.Run;
end.
