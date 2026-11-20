program MfAudioClip;

uses

  {$IFDEF FASTMM}
  FastMM4,
  {$ENDIF}

  System.SysUtils,
  WinApi.ComBaseApi,
  WinApi.ActiveX.ObjBase,
  Vcl.Forms,
  AudioClipCore in 'AudioClipCore.pas',
  frmAudioClip in 'frmAudioClip.pas' {frmAudioClip};

{$R *.res}

begin
  if CoInitializeEx(nil,COINIT_APARTMENTTHREADED) < 0 then
    raise Exception.Create('COM initialization failed.');
  try
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'MfAudioClip';
  Application.CreateForm(TfrmAudioClip, AudioClipFrm);
  if (ParamCount = 1) and (ParamStr(1) = '--smoke') then
    FreeAndNil(AudioClipFrm)
  else
    Application.Run;
  finally CoUninitialize;end;
end.
