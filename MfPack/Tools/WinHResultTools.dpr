program WinHResultTools;

uses
  Vcl.Forms,
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
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TdlgHrTools, dlgHrTools);
  Application.CreateForm(TdlgMediaTransformTool, dlgMediaTransformTool);
  Application.Run;
end.
