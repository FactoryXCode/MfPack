program MfPlaylist;

{$APPTYPE GUI}

uses
  System.SysUtils,
  System.Math,
  Vcl.Forms,
  WinApi.ComBaseApi,
  WinApi.ActiveX.ObjBase,
  frmMfPlaylist in 'frmMfPlaylist.pas' {MfPlaylistForm},
  MfPlaylistPlayer in 'MfPlaylistPlayer.pas';

{$R *.res}

var
  ComStatus: HRESULT;
  SavedMask: TFPUExceptionMask;
  Smoke: Boolean;

begin

  Smoke := (ParamCount = 1) and SameText(ParamStr(1),
                                         '--smoke');
  SavedMask := GetExceptionMask();
  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);
  try
    try
      ComStatus := CoInitializeEx(nil,
                                  COINIT_APARTMENTTHREADED);
      if (ComStatus < 0) then
        raise Exception.CreateFmt('COM initialization failed (HRESULT $%.8x).',
                                  [Cardinal(ComStatus)]);
      try
        Application.Initialize();
        Application.Title := 'MfPack Sequencer Source Playlist';
        Application.MainFormOnTaskbar := True;
        Application.ShowMainForm := not Smoke;
        Application.CreateForm(TMfPlaylistForm,
                               MfPlaylistForm);
        try
          if Smoke then
            Application.ProcessMessages()
          else
            Application.Run();
        finally
          FreeAndNil(MfPlaylistForm);
        end;
      finally
        CoUninitialize();
      end;
    except
      on E: Exception do
        begin
          if not Smoke then
            Application.ShowException(E);
          ExitCode := 1;
        end;
    end;
  finally
    ClearExceptions(False);
    SetExceptionMask(SavedMask);
  end;
end.
