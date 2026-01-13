program WasApiPlayer;

uses
  Vcl.Forms,
  MainFrm in 'MainFrm.pas' {MainForm},
  Tools in 'Tools.pas',
  WASAPIEngine in 'WASAPIEngine.pas',
  MfAudioBassTrebleMFT in 'MfAudioBassTrebleMFT.pas',
  MfAudioBassTrebleTypes in 'MfAudioBassTrebleTypes.pas';

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
