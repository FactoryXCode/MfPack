program SinkWriterSample;

uses
  WinApi.ComBaseApi,
  WinApi.ActiveX.ObjBase,
  System.Win.ComObj,
  System.SysUtils,
  System.Math,
{$IFDEF madExcept}
  madExcept,
  madLinkDisAsm,
  madListHardware,
  madListProcesses,
  madListModules,
  {$ENDIF}

  Vcl.Forms,
  frmMain in 'frmMain.pas' {MainForm},
  SinkWriterClass in 'SinkWriterClass.pas';

{$R *.res}

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
      Application.CreateForm(TMainForm, MainForm);
      if (ParamCount = 1) and (ParamStr(1) = '--smoke') then
        Application.ProcessMessages
      else
        Application.Run;
    finally
      FreeAndNil(MainForm);
      CoUninitialize();
      ClearExceptions(False);
      SetExceptionMask(SavedMask);
    end;
  except
    on E: Exception do
      begin
        if (ParamCount = 1) and (ParamStr(1) = '--smoke') then
          begin
            Writeln('FAIL: '+E.ClassName+': '+E.Message);
            Halt(1);
          end;
        raise;
      end;
  end;
end.
