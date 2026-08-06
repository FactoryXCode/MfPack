program TMFPlayerX2;

uses
  madExcept,
  madLinkDisAsm,
  madListHardware,
  madListProcesses,
  madListModules,
  Vcl.Forms,
  frmMfPlayer in 'frmMfPlayer.pas' {frm_MfPlayer},
  dlgStreamSelect in 'dlgStreamSelect.pas' {dlgSelectStreams},
  TimedTextClass in 'TimedTextClass.pas',
  MfMatroskaSubtitleReader in 'MfMatroskaSubtitleReader.pas',
  MfEmbeddedSubtitleReader in 'MfEmbeddedSubtitleReader.pas',
  LangTags in 'LangTags.pas',
  MfPlayerClassX in 'MfPlayerClassX.pas',
  MfMediaTimeline in 'MfMediaTimeline.pas',
  MfSubtitleCompositor in 'MfSubtitleCompositor.pas',
  MfSubtitleFramePump in 'MfSubtitleFramePump.pas',
  MfSubtitleTransform in 'MfSubtitleTransform.pas',
  MFTimerCallBackClass in 'MFTimerCallBackClass.pas',
  MfPCXConstants in 'MfPCXConstants.pas',
  dlgSelectTimedTextLanguages in 'dlgSelectTimedTextLanguages.pas' {DlgTimedTextLanguages},
  MfCastChannel in 'ChromeCast\MfCastChannel.pas',
  MfCastController in 'ChromeCast\MfCastController.pas',
  MfCastDiscovery in 'ChromeCast\MfCastDiscovery.pas',
  MfCastHttpServer in 'ChromeCast\MfCastHttpServer.pas',
  MfCastInterfaces in 'ChromeCast\MfCastInterfaces.pas',
  MfCastMedia in 'ChromeCast\MfCastMedia.pas',
  MfCastTranscode in 'ChromeCast\MfCastTranscode.pas',
  MfCastTypes in 'ChromeCast\MfCastTypes.pas',
  dlgMfCastDevices in 'ChromeCast\dlgMfCastDevices.pas' {CastDevicesDlg};

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

