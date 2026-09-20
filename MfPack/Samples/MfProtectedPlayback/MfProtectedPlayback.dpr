program MfProtectedPlayback;

{$APPTYPE GUI}

uses
  {WinApi}
  WinApi.Windows,
  WinApi.ActiveX,
  {Vcl}
  Vcl.Forms,

  MainForm in 'MainForm.pas',
  Player in 'Player.pas',
  ContentEnabler in 'ContentEnabler.pas',
  WebHelper in 'WebHelper.pas',
  SampleLog in 'SampleLog.pas';

  {$R *.res}

begin

  if Failed(CoInitialize(nil)) then
    Halt(1);

  try
    Application.Initialize;
    Application.MainFormOnTaskbar := True;
    Application.Title := 'MF Protected Playback';
    Application.CreateForm(TProtectedPlaybackForm, ProtectedPlaybackForm);
    Application.Run;
  finally
    CoUninitialize;
  end;
end.
