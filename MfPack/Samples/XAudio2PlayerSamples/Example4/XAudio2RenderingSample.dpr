program XAudio2RenderingSample;

uses
  WinApi.ComBaseApi,
  WinApi.ActiveX.ObjBase,
  System.Win.ComObj,
  System.SysUtils,
  System.Math,
Vcl.Forms,
  MainFrm in 'MainFrm.pas' {frmMain},
  XAudio2Engine in 'XAudio2Engine.pas',
  Tools in 'Tools.pas',
  XAudio2_FXReverb in 'XAudio2_FXReverb.pas',
  XAudio2_FXMasterLimiter in 'XAudio2_FXMasterLimiter.pas';

{$R *.res}

var
  SavedMask: TFPUExceptionMask;

begin
  try
    SavedMask := GetExceptionMask();
    SetExceptionMask([exInvalidOp,exDenormalized,exZeroDivide,exOverflow,exUnderflow,exPrecision]);
    OleCheck(CoInitializeEx(nil,COINIT_APARTMENTTHREADED));
    try
  
    {$IFDEF DEBUG}
    {$WARN SYMBOL_PLATFORM OFF}
    {$IFNDEF FPC}ReportMemoryLeaksOnShutdown := True;{$ENDIF}
    {$ENDIF}
  
      Application.Initialize;
      Application.MainFormOnTaskbar := True;
      Application.CreateForm(TfrmMain, frmMain);
      if (ParamCount = 1) and (ParamStr(1) = '--smoke') then
        Application.ProcessMessages
      else
        Application.Run;
    finally
      FreeAndNil(frmMain);
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
