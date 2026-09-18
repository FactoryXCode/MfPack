program MfWebCamStreamer;

uses
  Vcl.Forms,
  Form.Main in 'Form.Main.pas' {frmMain},
  FxServePublisher in 'FxServePublisher.pas',
  SimpleAvCapture in 'SimpleAvCapture.pas',
  SimpleFmp4ByteStream in 'SimpleFmp4ByteStream.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'MfWebCamStreamer';
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
