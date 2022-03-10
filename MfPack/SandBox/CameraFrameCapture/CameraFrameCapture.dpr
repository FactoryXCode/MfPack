program CameraFrameCapture;

uses
  {$IFDEF FASTMM}
  FastMM4,
  {$ENDIF }
  Vcl.Forms,
  Form.Main in 'Form.Main.pas' {FrmMain},
  MfDeviceCaptureClass in 'MfDeviceCaptureClass.pas',
  Support in 'Support.pas',
  FileCapture.Asynchronous in 'FileCapture.Asynchronous.pas',
  FileCapture in 'FileCapture.pas',
  MessageHandler in 'MessageHandler.pas',
  SampleConverter in 'SampleConverter.pas',
  CameraCapture.Asynchronous in 'CameraCapture.Asynchronous.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFrmMain, FrmMain);
  Application.Run;
end.
