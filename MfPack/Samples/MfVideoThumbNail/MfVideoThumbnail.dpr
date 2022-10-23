program MfVideoThumbnail;

uses

  {$IFDEF FASTMM}
  FastMM4,
  {$ENDIF }

  Vcl.Forms,
  frmVideoThumbnail in 'frmVideoThumbnail.pas' {Form1},
  Thumbnail in 'Thumbnail.pas',
  Sprite in 'Sprite.pas',
  SimpleTimer in 'SimpleTimer.pas',
  VideoTumbNailHelpers in 'VideoTumbNailHelpers.pas';

{$R *.res}

begin

  // Check for memoryleaks (debug mode (F9) only!)
  {$IFDEF DEBUG}
    {$WARN SYMBOL_PLATFORM OFF}
    ReportMemoryLeaksOnShutdown := (DebugHook <> 0);
    {$WARN SYMBOL_PLATFORM ON}
  {$ENDIF}

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
