program MfSimpleCaptureToFile;

uses
  System.SysUtils,
  System.Math,
  System.Win.ComObj,
  WinApi.ComBaseApi,
  WinApi.ActiveX.ObjBase,
{$IFDEF FASTMM}
  FastMM4,
  {$ENDIF}

  Vcl.Forms,
  frmSimpleCaptureToFile in 'frmSimpleCaptureToFile.pas' {Frm_SimpleCapture},
  MfCaptureToFileClass in 'MfCaptureToFileClass.pas';



var
  SavedMask: TFPUExceptionMask;

begin
  try
    SavedMask := GetExceptionMask();
    SetExceptionMask([exInvalidOp,exDenormalized,exZeroDivide,exOverflow,exUnderflow,exPrecision]);
    OleCheck(CoInitializeEx(nil,COINIT_APARTMENTTHREADED));
    try


  // Check for memoryleaks (debug mode (F9) only!)
  {$IFNDEF madExcept}
  {$WARN SYMBOL_PLATFORM OFF}
  ReportMemoryLeaksOnShutdown := (DebugHook <> 0);
  {$WARN SYMBOL_PLATFORM ON}
  {$ENDIF}
      Application.Initialize;
      Application.MainFormOnTaskbar := True;
      Application.Title := 'Simple Caption To File example, version 2.6.4';
      Application.CreateForm(TFrm_SimpleCapture, Frm_SimpleCapture);
      if ParamStr(1) = '--smoke' then Application.ProcessMessages
      else Application.Run;
    finally
      FreeAndNil(Frm_SimpleCapture);
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
