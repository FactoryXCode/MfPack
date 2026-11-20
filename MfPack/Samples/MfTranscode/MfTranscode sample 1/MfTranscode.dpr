program MfTranscode;

uses
  WinApi.ComBaseApi,
  WinApi.ActiveX.ObjBase,
  System.Win.ComObj,
  System.SysUtils,
{$IFDEF FASTMM}
  FastMM4,
  madExcept,
  madLinkDisAsm,
  madListHardware,
  madListProcesses,
  madListModules,
  {$ENDIF }

  Vcl.Forms,
  frmTranscode in 'frmTranscode.pas' {frmTranscoder},
  Transcoder in 'Transcoder.pas';

{$R *.res}

begin
  OleCheck(CoInitializeEx(nil,
                          COINIT_APARTMENTTHREADED));
  try
    Application.Initialize;
    Application.MainFormOnTaskbar := True;
    Application.Title := 'MfTranscode version 3.0.2';
    Application.CreateForm(TfrmTranscoder, frmTranscoder);
    if (ParamCount = 1) and (ParamStr(1) = '--smoke') then
      begin
        Application.ProcessMessages;
        frmTranscoder.Close();
        Application.ProcessMessages;
        FreeAndNil(frmTranscoder);
      end
    else
      Application.Run;
  finally
    CoUninitialize();
  end;
end.
