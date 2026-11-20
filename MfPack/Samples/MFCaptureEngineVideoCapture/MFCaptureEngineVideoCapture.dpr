program MFCaptureEngineVideoCapture;

uses
  System.SysUtils,
  System.Math,
  System.Win.ComObj,
  WinApi.ComBaseApi,
  WinApi.ActiveX.ObjBase,
Vcl.Forms,
  frmMain in 'frmMain.pas' {MainWindow},
  dlgChooseDevice in 'dlgChooseDevice.pas' {ChooseDeviceDlg},
  Utils in 'Utils.pas',
  DeviceExplorer in 'DeviceExplorer.pas',
  SampleConverter in 'SampleConverter.pas',
  CaptureEngine in 'CaptureEngine.pas';

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
      Application.CreateForm(TMainWindow, MainWindow);
      Application.CreateForm(TChooseDeviceDlg, ChooseDeviceDlg);
      if ParamStr(1) = '--smoke' then Application.ProcessMessages
      else Application.Run;
    
    finally
      FreeAndNil(ChooseDeviceDlg);
      FreeAndNil(MainWindow);
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
