program CaptureVideoFromGPU;

uses
  Vcl.Forms,
  frmCaptureVideoFromGpu in 'frmCaptureVideoFromGpu.pas' {FrmCapture},
  CaptureStreamEngine in 'CaptureStreamEngine.pas',
  Helpers in 'Helpers.pas',
  PreviewRenderer in 'PreviewRenderer.pas',
  GpuNV12Converter in 'GpuNV12Converter.pas',
  WasapiLoopbackCapture in 'WasapiLoopbackCapture.pas',
  ScreenActivityPinger in 'ScreenActivityPinger.pas',
  LoopbackAudioEngine in 'LoopbackAudioEngine.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFrmCapture, FrmCapture);
  Application.Run;
end.
