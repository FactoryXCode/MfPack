program AudioEndPointSample;

uses
  Vcl.Forms,
  frmVolumeControl in 'frmVolumeControl.pas' {VolumeControl};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TVolumeControl, VolumeControl);
  Application.Run;
end.
