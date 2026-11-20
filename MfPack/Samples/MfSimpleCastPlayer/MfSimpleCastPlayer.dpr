program MfSimpleCastPlayer;

uses
  System.SysUtils,
  WinApi.ComBaseApi,
  WinApi.ActiveX.ObjBase,
  Vcl.Forms,
  frmSimpleCastPlayer in 'frmSimpleCastPlayer.pas' {SimpleCastPlayerForm};

{$R *.res}

begin
  if CoInitializeEx(nil,COINIT_APARTMENTTHREADED) < 0 then
    raise Exception.Create('COM initialization failed.');
  try
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TSimpleCastPlayerForm, SimpleCastPlayerForm);
  if (ParamCount = 1) and (ParamStr(1) = '--smoke') then
    FreeAndNil(SimpleCastPlayerForm)
  else
    Application.Run;
  finally CoUninitialize;end;
end.
