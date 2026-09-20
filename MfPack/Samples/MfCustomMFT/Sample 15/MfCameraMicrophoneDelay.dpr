program MfCameraMicrophoneDelay;

uses
  Vcl.Forms,
  WinApi.Windows,
  WinApi.ActiveX,
  WinApi.MediaFoundationApi.MfApi,
  Form.Capture in 'Form.Capture.pas' {frmCapture},
  CaptureDevices in 'CaptureDevices.pas',
  CaptureSession in 'CaptureSession.pas',
  MfAudioDelayMFT in 'MfAudioDelayMFT.pas';

var
  Hr: HResult;
begin
  Hr := CoInitializeEx(nil, COINIT_APARTMENTTHREADED);
  if FAILED(Hr) then Halt(1);
  try
    Hr := MFStartup(MF_VERSION, MFSTARTUP_FULL);
    if FAILED(Hr) then Halt(2);
    try
      Application.Initialize();
      Application.MainFormOnTaskbar := True;
      Application.CreateForm(TfrmCapture, frmCapture);
      Application.Run();
    finally
      MFShutdown();
    end;
  finally
    CoUninitialize();
  end;
end.
