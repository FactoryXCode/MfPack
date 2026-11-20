program TMFPlayer;

uses
  WinApi.ComBaseApi,
  WinApi.ActiveX.ObjBase,
  System.SysUtils,
  {$IFDEF FASTMM}
  FastMM4,
  {$ENDIF}

  Vcl.Forms,
  frmMfPlayer in 'frmMfPlayer.pas' {frm_MfPlayer},
  MfPlayerClass in 'MfPlayerClass.pas',
  frmFullScreenLayer in 'frmFullScreenLayer.pas' {FullScreenLayer},
  UniThreadTimer in 'UniThreadTimer.pas';

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
  Application.MainFormOnTaskbar := True;
  Application.Title:= 'MFPlayer II';
  Application.CreateForm(Tfrm_MfPlayer, frm_MfPlayer);
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
