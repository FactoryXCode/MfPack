program MfSimpleWebCamStreamer;

uses

  {WinApi}
  WinApi.ComBaseApi,
  {Vcl}
  Vcl.Forms,
  {ActiveX}
  WinApi.ActiveX.ObjBase,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfApi,
  {Application}
  Form.Main in 'Form.Main.pas' {frmMain},
  SimpleAvCapture in 'SimpleAvCapture.pas',
  SimpleFmp4ByteStream in 'SimpleFmp4ByteStream.pas',
  SimpleHttpServer in 'SimpleHttpServer.pas';

{$R *.res}

begin

  CoInitializeEx(nil,
                 COINIT_APARTMENTTHREADED);

  MFStartup(MF_VERSION,
            MFSTARTUP_FULL);
  try
    Application.Initialize;
    Application.MainFormOnTaskbar := True;
    Application.CreateForm(TfrmMain, frmMain);
    Application.Run;
  finally
    MFShutdown();
    CoUninitialize();
  end;
end.
