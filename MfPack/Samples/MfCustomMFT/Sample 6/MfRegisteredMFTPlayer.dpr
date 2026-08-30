program MfRegisteredMFTPlayer;

uses
  {WinApi}
  WinApi.Windows,
  WinApi.ComBaseApi,
  {ActiveX}
  WinApi.ActiveX.ObjBase,
  {System}
  System.SysUtils,
  {Vcl}
  Vcl.Forms,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfApi,
  {Application}
  Form.RegisteredMain in 'Form.RegisteredMain.pas' {frmMain},
  RegisteredTopologyPlayer in 'RegisteredTopologyPlayer.pas';

var
  Hr: HRESULT;

begin

  Hr := CoInitializeEx(nil,
                       COINIT_APARTMENTTHREADED);
  if Failed(Hr) then
    Halt(1);
  try
    Hr := MFStartup(MF_VERSION,
                    MFSTARTUP_FULL);
    if Failed(Hr) then
      Halt(1);

    try
      Application.Initialize;
      Application.MainFormOnTaskbar := True;
      Application.CreateForm(TfrmMain, frmMain);
      Application.Run;

    finally
      MFShutdown();
    end;

  finally
    CoUninitialize();
  end;
end.
