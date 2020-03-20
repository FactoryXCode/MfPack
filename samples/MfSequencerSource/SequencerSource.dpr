program SequencerSource;

uses
  madExcept,
  madLinkDisAsm,
  madListHardware,
  madListProcesses,
  madListModules,
  Vcl.Forms,
  frmSequencer in 'frmSequencer.pas' {frm_PlayListPlayer},
  MfSequencerSource in 'MfSequencerSource.pas',
  CorePlayerEngine in 'CorePlayerEngine.pas',
  Helpers in 'Helpers.pas',
  PlayerSeeking in 'PlayerSeeking.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'MfSequencerSource';
  Application.CreateForm(Tfrm_PlayListPlayer, frm_PlayListPlayer);
  Application.Run;
end.
