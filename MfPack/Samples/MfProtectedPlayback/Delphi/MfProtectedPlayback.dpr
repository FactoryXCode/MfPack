program MfProtectedPlayback;

{$APPTYPE GUI}

uses
  System.SysUtils,
  System.Math,
  System.Win.ComObj,
  WinApi.ComBaseApi,
  WinApi.ActiveX.ObjBase,
{WinApi}
  WinApi.Windows,

  {Vcl}
  Vcl.Forms,

  MainForm in 'MainForm.pas',
  Player in 'Player.pas',
  ContentEnabler in 'ContentEnabler.pas',
  WebHelper in 'WebHelper.pas',
  SampleLog in 'SampleLog.pas';

  

var
  SavedMask: TFPUExceptionMask;

begin
  try
    SavedMask := GetExceptionMask();
    SetExceptionMask([exInvalidOp,exDenormalized,exZeroDivide,exOverflow,exUnderflow,exPrecision]);
    OleCheck(CoInitializeEx(nil,COINIT_APARTMENTTHREADED));
    try
      Application.Initialize;
      Application.MainFormOnTaskbar := True;
      Application.Title := 'MF Protected Playback';
      Application.CreateForm(TProtectedPlaybackForm, ProtectedPlaybackForm);
      if ParamStr(1) = '--smoke' then Application.ProcessMessages
      else Application.Run;
    finally
      FreeAndNil(ProtectedPlaybackForm);
      CoUninitialize();
      ClearExceptions(False);
      SetExceptionMask(SavedMask);
    end;
  except
    on E: Exception do
      begin
        if ParamStr(1) = '--smoke' then begin Writeln('FAIL: '+E.ClassName+': '+E.Message);Halt(1);end;
        raise;
      end;
  end;
end.
