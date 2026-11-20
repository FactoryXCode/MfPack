program TMfMediaEnginePlayer;

uses
  System.SysUtils,
  System.Math,
  System.Win.ComObj,
  WinApi.ComBaseApi,
  WinApi.ActiveX.ObjBase,
{$IFDEF FASTMM}
  FastMM4,
  {$ENDIF }

  Vcl.Forms,
  Vcl.Themes,
  Vcl.Styles,
  MediaEngineClass in 'MediaEngineClass.pas',
  frmMfMediaEnginePlayer in 'frmMfMediaEnginePlayer.pas' {FeMediaEnginePlayer};



var
  SavedMask: TFPUExceptionMask;

begin
  try
    SavedMask := GetExceptionMask();
    SetExceptionMask([exInvalidOp,exDenormalized,exZeroDivide,exOverflow,exUnderflow,exPrecision]);
    OleCheck(CoInitializeEx(nil,COINIT_APARTMENTTHREADED));
    try
      Application.Initialize;
      Application.MainFormOnTaskbar:= True;
      Application.Title:= 'Mf MePlayer I';
      Application.CreateForm(TFeMediaEnginePlayer, FeMediaEnginePlayer);
      if ParamStr(1) = '--smoke' then Application.ProcessMessages
      else Application.Run;
    finally
      FreeAndNil(FeMediaEnginePlayer);
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
