program TMfMediaEnginePlayer2;

uses
  Vcl.Forms,
  Vcl.Themes,
  Vcl.Styles,
  MediaEngineClass in 'MediaEngineClass.pas',
  frmMfMediaEnginePlayer in 'frmMfMediaEnginePlayer.pas' {FeMediaEnginePlayer},
  TimedTextNotifyClass in 'TimedTextNotifyClass.pas',
  FloatingFrm in 'FloatingFrm.pas' {FloatingForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar:= True;
  Application.Title:= 'Mf MePlayer II';
  Application.CreateForm(TFeMediaEnginePlayer, FeMediaEnginePlayer);
  Application.Run;
end.
