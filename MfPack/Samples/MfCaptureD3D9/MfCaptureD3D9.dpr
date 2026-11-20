program MfCaptureD3D9;

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
  frmMfCaptureD3D9 in 'frmMfCaptureD3D9.pas' {frmMain},
  dlgSelDevice in 'dlgSelDevice.pas' {dlgSelectDevice},
  Preview in 'Preview.pas',
  Device in 'Device.pas',
  VideoBufferLock in 'VideoBufferLock.pas';



var
  SavedMask: TFPUExceptionMask;

begin
  try
    SavedMask := GetExceptionMask();
    SetExceptionMask([exInvalidOp,exDenormalized,exZeroDivide,exOverflow,exUnderflow,exPrecision]);
    OleCheck(CoInitializeEx(nil,COINIT_APARTMENTTHREADED));
    try

   // Check for memoryleaks (debug mode (F9) only!)
  {$IFDEF DEBUG}
    {$WARN SYMBOL_PLATFORM OFF}
    ReportMemoryLeaksOnShutdown := (DebugHook <> 0);
    {$WARN SYMBOL_PLATFORM ON}
  {$ENDIF}

      Application.Initialize;
      Application.MainFormOnTaskbar:= True;
      Application.Title := 'MFCaptureD3D9';
      Application.CreateForm(TfrmMain, frmMain);
      if ParamStr(1) = '--smoke' then Application.ProcessMessages
      else Application.Run;
    
    finally
      FreeAndNil(frmMain);
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
