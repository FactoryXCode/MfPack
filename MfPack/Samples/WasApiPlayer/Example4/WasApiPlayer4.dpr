program WasApiPlayer4;

uses
  madExcept,
  madLinkDisAsm,
  madListHardware,
  madListProcesses,
  madListModules,
  Vcl.Forms,
  MainFrm in 'MainFrm.pas' {frmMain},
  EqSettingsFrm in 'EqSettingsFrm.pas' {frmEqSettings},
  MfAudioEqBaseMFT in 'MfAudioEqBaseMFT.pas',
  MfAudioHighMidLowMFT in 'MfAudioHighMidLowMFT.pas',
  MfAudioHighMidLowTypes in 'MfAudioHighMidLowTypes.pas',
  WASAPIEngine in 'WASAPIEngine.pas',
  EqPlotUtils in 'EqPlotUtils.pas',
  AudioDynamicsDSP in 'AudioDynamicsDSP.pas',
  PcmLib in 'PcmLib.pas',
  WASAPINotifications in 'WASAPINotifications.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmEqSettings, frmEqSettings);
  Application.Run;
end.