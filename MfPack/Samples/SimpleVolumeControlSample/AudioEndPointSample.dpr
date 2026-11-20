program AudioEndPointSample;

uses
  System.SysUtils,
  WinApi.ComBaseApi,
  WinApi.ActiveX.ObjBase,
  Vcl.Forms,
  frmVolumeControl in 'frmVolumeControl.pas' {VolumeControl};

{$R *.res}

begin
  if CoInitializeEx(nil,COINIT_APARTMENTTHREADED) < 0 then
    raise Exception.Create('COM initialization failed.');
  try
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TVolumeControl, VolumeControl);
  try
    if not ((ParamCount = 1) and (ParamStr(1) = '--smoke')) then
      Application.Run;
  finally
    // Release native endpoint interfaces while COM is still initialized.
    FreeAndNil(VolumeControl);
  end;
  finally CoUninitialize;end;
end.
