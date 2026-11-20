program MfSimpleWebCamStreamer;

uses
  WinApi.ComBaseApi,
  WinApi.ActiveX.ObjBase,
  System.Win.ComObj,
  System.SysUtils,
{WinApi}

  {Vcl}
  Vcl.Forms,
  {ActiveX}

  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfApi,
  {Application}
  Form.Main in 'Form.Main.pas' {frmMain},
  SimpleAvCapture in 'SimpleAvCapture.pas',
  SimpleFmp4ByteStream in 'SimpleFmp4ByteStream.pas',
  SimpleHttpServer in 'SimpleHttpServer.pas';

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
      Application.CreateForm(TfrmMain, frmMain);
      if (ParamCount = 1) and (ParamStr(1) = '--smoke') then
        begin
          Application.ProcessMessages;
          frmMain.Close();
          Application.ProcessMessages;
          FreeAndNil(frmMain);
        end
      else
        Application.Run;
    finally
      MFShutdown();
    end;
  finally
    CoUninitialize();
  end;
end.
