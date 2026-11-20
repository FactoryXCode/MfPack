program MfFrameCapture;
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
  Form.Main in 'Form.Main.pas' {FrmMain},
  Support in 'Support.pas',
  SampleConverter in 'SampleConverter.pas',
  MessageHandler in 'MessageHandler.pas',
  FileCapture.Asynchronous in 'FileCapture.Asynchronous.pas',
  FileCapture.Synchronous in 'FileCapture.Synchronous.pas',
  FileCapture in 'FileCapture.pas';


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
      Application.CreateForm(TFrmMain, FrmMain);
      if ParamStr(1) = '--smoke' then Application.ProcessMessages
      else Application.Run;
    finally
      FreeAndNil(FrmMain);
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
