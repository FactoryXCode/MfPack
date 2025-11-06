program MfAudioClipEx;

uses
  Vcl.Forms,
  frmAudioClipEx in 'frmAudioClipEx.pas' {frmAudioClipEx},
  AudioClipEngine in 'AudioClipEngine.pas',
  Helpers in 'Helpers.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TAudioClipExFrm, AudioClipExFrm);
  Application.Run;
end.
