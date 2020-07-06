program TMfMediaEnginePlayer;

uses
  Vcl.Forms,
  Vcl.Themes,
  Vcl.Styles,
  MediaEngineClass in 'MediaEngineClass.pas',
  frmMfMediaEnginePlayer in 'frmMfMediaEnginePlayer.pas' {FeMediaEnginePlayer};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar:= True;
  Application.Title:= 'Mf MePlayer I';
  Application.CreateForm(TFeMediaEnginePlayer, FeMediaEnginePlayer);
  Application.Run;
end.
