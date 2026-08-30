program MfCustomMFT;

uses

  System.SysUtils,
  WinApi.Windows,
  WinApi.ComBaseApi,
  WinApi.ActiveX.ObjBase,
  Vcl.Forms,
  WinApi.MediaFoundationApi.MfApi,
  Form.Main in 'Form.Main.pas' {frmMain},
  MfGrayscaleMFT in 'MfGrayscaleMFT.pas';

var
  Hr: HResult;

begin
  Hr := CoInitializeEx(nil, COINIT_APARTMENTTHREADED);
  if FAILED(Hr) then
    raise Exception.CreateFmt('CoInitializeEx failed (HRESULT 0x%.8x).',
                              [Cardinal(Hr)]);
  try
    Hr := MFStartup(MF_VERSION, MFSTARTUP_FULL);
    if FAILED(Hr) then
      raise Exception.CreateFmt('MFStartup failed (HRESULT 0x%.8x).',
                                [Cardinal(Hr)]);
    try
      Application.Initialize;
      Application.MainFormOnTaskbar := True;
      Application.CreateForm(TfrmMain, frmMain);
      Application.Run;
    finally
      MFShutdown;
    end;
  finally
    CoUninitialize;
  end;
end.
