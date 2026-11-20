program MfSimpleCapture;

uses
  WinApi.ComBaseApi,
  WinApi.ActiveX.ObjBase,
  System.Win.ComObj,
  System.SysUtils,
{$IFDEF FASTMM}
  FastMM4,
  {$ENDIF }

  Vcl.Forms,
  MfDeviceCaptureClass in 'MfDeviceCaptureClass.pas',
  frmSimpleCapture in 'frmSimpleCapture.pas' {Frm_SimpleCapture},
  frmdlgChooseDevice in 'frmdlgChooseDevice.pas' {dlgChooseDevice};

{$R *.res}

begin
  {$IFNDEF madExcept}
  {$WARN SYMBOL_PLATFORM OFF}
  ReportMemoryLeaksOnShutdown := (DebugHook <> 0);
  {$WARN SYMBOL_PLATFORM ON}
  {$ENDIF}
  OleCheck(CoInitializeEx(nil,
                          COINIT_APARTMENTTHREADED));
  try
    Application.Initialize;
    Application.MainFormOnTaskbar:= True;
    Application.CreateForm(TFrm_SimpleCapture, Frm_SimpleCapture);
    Application.CreateForm(TdlgChooseDevice, dlgChooseDevice);
    if (ParamCount = 1) and (ParamStr(1) = '--smoke') then
      begin
        Application.ProcessMessages;
        Frm_SimpleCapture.Close();
        Application.ProcessMessages;
        FreeAndNil(Frm_SimpleCapture);
      end
    else
      Application.Run;
  finally
    CoUninitialize();
  end;
end.
