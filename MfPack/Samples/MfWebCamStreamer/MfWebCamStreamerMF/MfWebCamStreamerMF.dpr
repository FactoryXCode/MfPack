program MfWebCamStreamerMF;

uses
  WinApi.ComBaseApi,
  WinApi.ActiveX.ObjBase,
  System.Win.ComObj,
  System.SysUtils,
  WinApi.MediaFoundationApi.MfApi,
  Vcl.Forms,
  Form.Main in 'Form.Main.pas' {frmMain},
  CamControl in 'CamControl.pas',
  dlgMfCameraControls in 'dlgMfCameraControls.pas' {MfCameraControlsDialog},
  FxServePublisher in 'FxServePublisher.pas',
  SimpleAvCapture in 'SimpleAvCapture.pas',
  SimpleFmp4ByteStream in 'SimpleFmp4ByteStream.pas',
  MfCameraSettings in 'MfCameraSettings.pas';

{$R *.res}

begin

  OleCheck(CoInitializeEx(nil,
                          COINIT_APARTMENTTHREADED));
  try
    OleCheck(MFStartup(MF_VERSION,
                       MFSTARTUP_FULL));
    try
      Application.Initialize;
      Application.MainFormOnTaskbar := True;
      Application.Title := 'MfWebCamStreamer MF';
      Application.CreateForm(TfrmMain, frmMain);
  try
        if (ParamCount = 1) and (ParamStr(1) = '--smoke') then
          begin
            Application.ProcessMessages();
            frmMain.Close();
            Application.ProcessMessages();
          end
        else
          Application.Run();
      finally
        FreeAndNil(frmMain);
      end;
    finally
      MFShutdown();
    end;
  finally
    CoUninitialize();
  end;
end.

