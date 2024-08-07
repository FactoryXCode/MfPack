program XAudio2RenderingSample;

uses
  Vcl.Forms,
  MainFrm in 'MainFrm.pas' {frmMain},
  Tools in 'Tools.pas',
  XAudio2_FXReverb in 'XAudio2_FXReverb.pas',
  XAudio2_FXMasterLimiter in 'XAudio2_FXMasterLimiter.pas',
  XAudio2Engine in 'XAudio2Engine.pas';

{$R *.res}

begin

  {$IFDEF DEBUG}
  {$WARN SYMBOL_PLATFORM OFF}
  //ReportMemoryLeaksOnShutdown := True;
  {$ENDIF}

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
