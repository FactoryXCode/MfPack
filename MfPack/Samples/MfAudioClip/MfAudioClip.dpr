program MfAudioClip;

uses
  Vcl.Forms,
  AudioClipCore in 'AudioClipCore.pas',
  frmAudioClip in 'frmAudioClip.pas' {frmAudioClip};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'MfAudioClip';
  Application.CreateForm(TfrmAudioClip, AudioClipFrm);
  Application.Run;
end.
