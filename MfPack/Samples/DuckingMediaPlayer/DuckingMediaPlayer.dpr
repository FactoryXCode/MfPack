program DuckingMediaPlayer;

uses
  {$IFDEF FASTMM}
  FastMM4,
  {$ENDIF}
  Vcl.Forms,
  frmDuckingMediaPlayer in 'frmDuckingMediaPlayer.pas' {Form1},
  MediaPlayer in 'MediaPlayer.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
