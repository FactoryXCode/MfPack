program MFDeviceLoss;

uses
  Vcl.Forms,
  DeviceLoss in 'DeviceLoss.pas',
  frmdlgChooseDevice in 'frmdlgChooseDevice.pas' {dlgChooseDevice},
  frmSimpleCapture in 'frmSimpleCapture.pas' {Frm_SimpleCapture},
  MfDeviceCaptureClass in 'MfDeviceCaptureClass.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFrm_SimpleCapture, Frm_SimpleCapture);
  Application.CreateForm(TdlgChooseDevice, dlgChooseDevice);
  Application.Run;
end.
