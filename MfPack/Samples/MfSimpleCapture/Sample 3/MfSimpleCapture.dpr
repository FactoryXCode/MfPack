program MfSimpleCapture;

{$APPTYPE GUI}

uses
  System.SysUtils,
  System.Math,
  Vcl.Forms,
  WinApi.ComBaseApi,
  WinApi.ActiveX.ObjBase,
  WinApi.MediaFoundationApi.MfApi,
  MfDeviceCaptureClass in 'MfDeviceCaptureClass.pas',
  MfCameraControls in 'MfCameraControls.pas',
  frmSimpleCapture in 'frmSimpleCapture.pas' {Frm_SimpleCapture},
  dlgMfCameraControls in 'dlgMfCameraControls.pas',
  frmdlgChooseDevice in 'frmdlgChooseDevice.pas';

{$R *.res}

var
  Status: HRESULT;
  SavedMask: TFPUExceptionMask;
  Smoke: Boolean;

begin

  Smoke := SameText(ParamStr(1),
                    '--smoke');
  SavedMask := GetExceptionMask();
  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);
  try
    try
      Status := CoInitializeEx(nil,
                               COINIT_APARTMENTTHREADED);
      if (Status < 0) then
        raise Exception.CreateFmt('COM initialization failed ($%.8x).',
                                  [Cardinal(Status)]);
      try
        Status := MFStartup(MF_VERSION,
                            MFSTARTUP_FULL);
        if (Status < 0) then
          raise Exception.CreateFmt('Media Foundation initialization failed ($%.8x).',
                                    [Cardinal(Status)]);
        try
          Application.Initialize();
          Application.MainFormOnTaskbar := True;
          Application.Title := 'MfSimpleCapture Sample 3';
          Application.ShowMainForm := not Smoke;
          Application.CreateForm(TFrm_SimpleCapture,
                                 Frm_SimpleCapture);
          try
            if Smoke then
              Application.ProcessMessages()
            else
              Application.Run();
          finally
            FreeAndNil(Frm_SimpleCapture);
          end;
        finally
          MFShutdown();
        end;
      finally
        CoUninitialize();
      end;
    except
      on E: Exception do
        begin
          if Smoke then
            Writeln('FAIL: ',
                    E.ClassName,
                    ': ',
                    E.Message)
          else
            Application.ShowException(E);
          ExitCode := 1;
        end;
    end;
  finally
    ClearExceptions(False);
    SetExceptionMask(SavedMask);
  end;
end.
