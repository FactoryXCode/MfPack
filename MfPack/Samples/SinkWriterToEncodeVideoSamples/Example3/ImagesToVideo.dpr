program ImagesToVideo;

uses
  WinApi.ComBaseApi,
  WinApi.ActiveX.ObjBase,
  System.Win.ComObj,
  System.SysUtils,
  System.Math,
Vcl.Forms,
  ImageRenderer in 'ImageRenderer.pas',
  frmMain in 'frmMain.pas' {FfrmMain},
  Common in 'Common.pas',
  Scale in 'Scale.pas',
  Tools in 'Tools.pas',
  Transformer in 'Transformer.pas',
  dlgAudioFormats in 'dlgAudioFormats.pas' {AudioFormatDlg},
  frmAdvanced in 'frmAdvanced.pas' {FfrmAdvanced};

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
      Application.CreateForm(TfrmMain, FfrmMain);
      Application.CreateForm(TAudioFormatDlg, AudioFormatDlg);
      Application.CreateForm(TFfrmAdvanced, FfrmAdvanced);
      if (ParamCount = 1) and (ParamStr(1) = '--smoke') then
        Application.ProcessMessages
      else
        Application.Run;
    finally
      FreeAndNil(FfrmAdvanced);
      FreeAndNil(AudioFormatDlg);
      FreeAndNil(FfrmMain);
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
