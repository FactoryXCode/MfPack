program MfRdjJ;

uses
  Vcl.Forms,
  frmMasterDeck in 'frmMasterDeck.pas' {MasterDeckFrm},
  frmPlaylistEditor in 'frmPlaylistEditor.pas' {frmPlaylistEditor},
  frmSetup in 'frmSetup.pas' {frmSetup},
  RDJ.LibraryScanner in 'RDJ.LibraryScanner.pas',
  RDJ.PlaylistDb in 'RDJ.PlaylistDb.pas',
  RDJ.PlaylistManager in 'RDJ.PlaylistManager.pas',
  RDJ.PlaylistTypes in 'RDJ.PlaylistTypes.pas',
  RDJ.Setup in 'RDJ.Setup.pas',
  RDJ.TagReader in 'RDJ.TagReader.pas',
  RDJ.TrackLibrary in 'RDJ.TrackLibrary.pas',
  RDJ.LibraryScanThread in 'RDJ.LibraryScanThread.pas',
  RDJ.InternalMixer in 'RDJ.InternalMixer.pas',
  MfWasApiRenderOutputEngine in 'MfWasApiRenderOutputEngine.pas',
  MfChannelDeckEngine in 'MfChannelDeckEngine.pas',
  frmMasterFxRack in 'frmMasterFxRack.pas' {frmMasterFxRack},
  frmTagEditor in 'frmTagEditor.pas' {frmTagEditor},
  RDJ.TagWriter in 'RDJ.TagWriter.pas',
  MfLoopbackDeckEngine in 'MfLoopbackDeckEngine.pas',
  frmLoopbackDeck in 'frmLoopbackDeck.pas' {LoopbackDeckfrm},
  LoopBackCapture in 'LoopBackCapture.pas',
  frmChannelDeck in 'frmChannelDeck.pas' {ChannelDeckFrm},
  frmProcessPicker in 'frmProcessPicker.pas' {dlgProcessPicker},
  ProcessAudioPickerUtils in 'ProcessAudioPickerUtils.pas',
  MfAudioRecorder in 'MfAudioRecorder.pas',
  MfAudioFileWriter in 'MfAudioFileWriter.pas',
  RDJ_Common in 'RDJ_Common.pas',
  Vcl.Themes,
  Vcl.Styles,
  frmMainMDI in 'frmMainMDI.pas' {MainMDIFrm},
  MfIcecastBroadcastEngine in 'MfIcecastBroadcastEngine.pas',
  MfMicrophoneFx in 'MfMicrophoneFx.pas',
  MfMicrophoneDeckEngine in 'MfMicrophoneDeckEngine.pas',
  MicrophoneDeckFrm in 'MicrophoneDeckFrm.pas' {MicrophoneDeckFrm},
  MfIcecastServerManager in 'MfIcecastServerManager.pas',
  MfBroadcastEncoderAac in 'MfBroadcastEncoderAac.pas',
  RDJ.JSon in 'RDJ.JSon.pas',
  RDJ.FilenameParser in 'RDJ.FilenameParser.pas',
  dlgMediaServer in 'dlgMediaServer.pas' {frmMediaServer},
  dlgAudioDevices in 'dlgAudioDevices.pas' {AudioDevicesDlg},
  LWFileBrowserExDlg in 'LWFileBrowserExDlg.pas' {LWFileBrowserExDlg},
  RDJ_NetWorkStationsScanner in 'RDJ_NetWorkStationsScanner.pas',
  RDJ.CarmenPro in 'RDJ.CarmenPro.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'RDJ Charlie';
  Application.CreateForm(TMainMDIFrm, MainMDIFrm);
  // Autocreate these forms for less UI load during rendering in loopback.
  Application.CreateForm(TLWFileBrowserExDlg, DlgLWFileBrowserEx);
  Application.CreateForm(TfrmMediaServer, fMediaServer);
  Application.Run;
end.
