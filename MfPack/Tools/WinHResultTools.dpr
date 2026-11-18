program WinHResultTools;

uses
  Vcl.Forms,
  System.SysUtils,
  WinApi.ComBaseApi,
  WinApi.ActiveX.ObjBase,
  WinApi.MediaFoundationApi.MfApi,
  HrDlg in 'HrDlg.pas' {dlgHrTools},
  Tools in 'Tools.pas',
  WinApi.Dbg.D3DError in '..\src\debug\WinApi.Dbg.D3DError.pas',
  WinApi.Dbg.WinError32 in '..\src\debug\WinApi.Dbg.WinError32.pas',
  WinApi.Dbg.WinFacility in '..\src\debug\WinApi.Dbg.WinFacility.pas',
  WinApi.Dbg.WinHResult in '..\src\debug\WinApi.Dbg.WinHResult.pas',
  WinApi.Dbg.WinHResultTools in '..\src\debug\WinApi.Dbg.WinHResultTools.pas',
  WinApi.Dbg.WinMfError in '..\src\debug\WinApi.Dbg.WinMfError.pas',
  WinApi.Dbg.MFTUtils in '..\src\debug\WinApi.Dbg.MFTUtils.pas',
  MediaTransformToolDlg in 'MediaTransformToolDlg.pas' {dlgMediaTransformTool},
  WinApi.dbg.StiErr in '..\src\debug\WinApi.Dbg.StiErr.pas',
  WinApi.Dbg.XAudio2Err in '..\src\debug\WinApi.Dbg.XAudio2Err.pas';

{$R *.res}

begin
  if CoInitializeEx(nil,COINIT_APARTMENTTHREADED) < 0 then raise Exception.Create('COM initialization failed.');
  try
    if MFStartup(MF_VERSION,MFSTARTUP_FULL) < 0 then raise Exception.Create('Media Foundation initialization failed.');
    try
      Application.Initialize;
      Application.MainFormOnTaskbar := True;
      Application.CreateForm(TdlgHrTools,dlgHrTools);
      if (ParamCount = 1) and (ParamStr(1) = '--smoke') then
        begin
          dlgHrTools.cbxAutoPaste.Checked := False;
          dlgHrTools.rbHLD.Checked := True;
          dlgHrTools.edHrHex.Text := '$80004005';
          dlgHrTools.butSearchClick(nil);
          if Pos('E_FAIL',UnicodeString(dlgHrTools.mmoDescription.Text)) = 0 then
            raise Exception.Create('Lookup initialization check failed.');
        end
      else
        Application.Run;
    finally
      FreeAndNil(dlgMediaTransformTool);
      FreeAndNil(dlgHrTools);
      MFShutdown;
    end;
  finally CoUninitialize;end;
end.
