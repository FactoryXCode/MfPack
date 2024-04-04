program XAudio2RenderingSample;

uses
  Vcl.Forms,
  MainFrm in 'MainFrm.pas' {frmMain},
  XAudio2Engine in 'XAudio2Engine.pas',
  Tools in 'Tools.pas';

{$R *.res}

begin

  {$IFDEF DEBUG}
  {$WARN SYMBOL_PLATFORM OFF}
  ReportMemoryLeaksOnShutdown := True;
  {$ENDIF}

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
