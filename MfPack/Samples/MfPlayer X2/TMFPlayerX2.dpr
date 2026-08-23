program TMFPlayerX2;

uses
  Vcl.Forms,
  frmMfPlayer in 'frmMfPlayer.pas' {frm_MfPlayer},
  dlgStreamSelect in 'dlgStreamSelect.pas' {dlgSelectStreams},
  TimedTextClass in 'D:\PROJECTS\MfPack\Cast\Media\TimedTextClass.pas',
  MfMatroskaSubtitleReader in 'D:\PROJECTS\MfPack\Cast\Media\MfMatroskaSubtitleReader.pas',
  MfEmbeddedSubtitleReader in 'D:\PROJECTS\MfPack\Cast\Media\MfEmbeddedSubtitleReader.pas',
  LangTags in 'D:\PROJECTS\MfPack\Cast\Media\LangTags.pas',
  MfPlayerClassX in 'MfPlayerClassX.pas',
  MfMediaTimeline in 'MfMediaTimeline.pas',
  MfSubtitleCompositor in 'D:\PROJECTS\MfPack\Cast\Media\MfSubtitleCompositor.pas',
  MfSubtitleFramePump in 'D:\PROJECTS\MfPack\Cast\Media\MfSubtitleFramePump.pas',
  MfSubtitleTransform in 'MfSubtitleTransform.pas',
  MFTimerCallBackClass in 'MFTimerCallBackClass.pas',
  MfPCXConstants in 'D:\PROJECTS\MfPack\Cast\Media\MfPCXConstants.pas',
  dlgSelectTimedTextLanguages in 'dlgSelectTimedTextLanguages.pas' {DlgTimedTextLanguages},
  dlgMfCastDevices in 'dlgMfCastDevices.pas' {CastDevicesDlg};

{$R *.res}

begin

   // Check for memoryleaks (debug mode (F9) only!)
{$IFNDEF madExcept}
  {$IFDEF DEBUG}
    {$WARN SYMBOL_PLATFORM OFF}
    ReportMemoryLeaksOnShutdown := (DebugHook <> 0);
    {$WARN SYMBOL_PLATFORM ON}
  {$ENDIF}
{$ENDIF}

  Application.Initialize;
  Application.MainFormOnTaskbar:= True;
  Application.Title := 'MFPlayer X2';
  Application.CreateForm(Tfrm_MfPlayer, frm_MfPlayer);
  Application.CreateForm(TdlgSelectStreams, dlgSelectStreams);
  Application.CreateForm(TdlgTimedTextLanguages, dlgTimedTextLanguages);
  Application.CreateForm(TCastDevicesDlg, CastDevicesDlg);
  Application.Run;
end.

