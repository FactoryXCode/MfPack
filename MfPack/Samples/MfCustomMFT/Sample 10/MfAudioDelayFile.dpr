program MfAudioDelayFile;

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
  Form.AudioDelay in 'Form.AudioDelay.pas' {frmAudioDelay},
  AudioDelayFileEngine in 'AudioDelayFileEngine.pas',
  MfAudioDelayMFT in '..\Sample 9\MfAudioDelayMFT.pas';

var
  Hr: HResult;

begin
  Hr := CoInitializeEx(nil, COINIT_APARTMENTTHREADED);
  if FAILED(Hr) then
    raise Exception.CreateFmt('CoInitializeEx failed (HRESULT %.8x).',
                              [Cardinal(Hr)]);
  try
    Hr := MFStartup(MF_VERSION, MFSTARTUP_FULL);
    if FAILED(Hr) then
      raise Exception.CreateFmt('MFStartup failed (HRESULT %.8x).',
                                [Cardinal(Hr)]);
    try
      Application.Initialize;
      Application.MainFormOnTaskbar := True;
      Application.CreateForm(TfrmAudioDelay, frmAudioDelay);
      Application.Run;
    finally
      MFShutdown;
    end;
  finally
    CoUninitialize;
  end;
end.
