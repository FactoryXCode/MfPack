program TMFPlayerX2;

uses
  WinApi.ComBaseApi,
  WinApi.ActiveX.ObjBase,
  System.SysUtils,
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

var
  hrCom: LongInt;

begin
  hrCom := CoInitializeEx(nil,COINIT_APARTMENTTHREADED);
  if hrCom < 0 then
    raise Exception.CreateFmt('COM initialization failed: %.8x',[Cardinal(hrCom)]);
  try

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
  if (ParamCount = 1) and (ParamStr(1) = '--smoke') then
    begin
      Application.ProcessMessages;
      frm_MfPlayer.Close();
      Application.ProcessMessages;
      FreeAndNil(frm_MfPlayer);
    end
  else
    Application.Run;
  finally
    CoUninitialize();
  end;
end.

