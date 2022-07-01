program MfVideoThumbnail;

uses

  {$IFDEF FASTMM}
  FastMM4,
  {$ENDIF }
  {$IFDEF MAD}
  madExcept,
  madLinkDisAsm,
  madListHardware,
  madListProcesses,
  madListModules,
  {$ENDIF }

  Vcl.Forms,
  frmVideoThumbnail in 'frmVideoThumbnail.pas' {Form1},
  Thumbnail in 'Thumbnail.pas',
  Sprite in 'Sprite.pas',
  SimpleTimer in 'SimpleTimer.pas',
  VideoTumbNailHelpers in 'VideoTumbNailHelpers.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
