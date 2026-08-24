// FactoryX
//
// Copyright: FactoryX. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: frmMasterDeck.pas
// Kind: Pascal Unit
// Release date: 24-01-2026
// Language: ENU
//
// Revision Version: 4.0.0
// Description: Master PA and PFL deck MDI child form.
//              Includes recorder and IceCast UI.
//
// Company: FactoryX
// Intiator(s): Tony (maXcomX), Carmen (carmenh).
// Contributor(s): Tony Kalf (maXcomX), Carmen (carmenh).
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 24/08/2026 All                 Moby release  SDK 10.0.28000.2705  (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 or higher.
//          Please, read documentation carefully!
//
// Related objects: -
// Related projects: MfPackX400
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.28000.2705
//
// Todo: -
//
// =============================================================================
// Source: FactoryX.Code.
// =============================================================================
//
// LICENSE
//
// The contents of this file are subject to the Mozilla Public License
// Version 2.0 (the "License"); you may not use this file except in
// compliance with the License. You may obtain a copy of the License at
// https://www.mozilla.org/en-US/MPL/2.0/
//
// Software distributed under the License is distributed on an "AS IS"
// basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
// License for the specific language governing rights and limitations
// under the License.
//
// Non commercial users may distribute this sourcecode provided that this
// header is included in full at the top of the file.
// Commercial users are not allowed to distribute this sourcecode as part of
// their product.
//
//==============================================================================
unit frmMasterDeck;

interface

uses

  {WinApi}
  WinApi.Windows,
  WinApi.Messages,
  WinApi.WinApiTypes,
  WinApi.ShellAPI,
  Winapi.WinSock,
  WinApi.WinError,
  {System}
  System.SysUtils,
  System.IOUtils,
  System.UITypes,
  System.Variants,
  System.Classes,
  System.Diagnostics,
  System.TimeSpan,
  {Vcl}
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.ComCtrls,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfUtils,
  {CoreAudioApi}
  WinApi.CoreAudioApi.MMDeviceApi,
  {WinMM}
  WinApi.WinMM.MMeApi,
  {Application}
  RDJ.Setup,
  RDJ_Common,
  dlgAudioDevices,
  MfAudioRecorder,
  MfPeakMeterMmcs,
  MfAudioEndPoint,
  MfTrackBar,
  MPxpButton,
  MfAudioMixVisualizer,
  MfIcecastServerManager;

const
  MIN_VOLUME = 0.0;
  MAX_VOLUME = 100.0;
  RDJ_MASTER_INPUT_NAME = 'RDJ Master Input';

type

  TOutputFormat = (ofWav,
                   ofFlac);

  TMasterDeckFrm = class(TForm)
    pnlBottom: TPanel;
    pnlTop: TPanel;
    bvlPfl: TBevel;
    bvlMaster: TBevel;
    lblTitlePfl: TLabel;
    lblTitleMaster: TLabel;
    lblBalMaster: TLabel;
    lblMasterVolL: TLabel;
    lblMasterVolR: TLabel;
    lblPflVol: TLabel;
    pmMasterL: TMfPeakMeterMmcs;
    pmMasterR: TMfPeakMeterMmcs;
    pmPflR: TMfPeakMeterMmcs;
    pmPflL: TMfPeakMeterMmcs;
    tbBalance: TMfTrackBar;
    Label1: TLabel;
    Label2: TLabel;
    tbMasterVolL: TMfTrackBar;
    tbMasterVolR: TMfTrackBar;
    tbPflVol: TMfTrackBar;
    chkLockMasterFaders: TMPxpButton;
    btnPflMute: TMPxpButton;
    epPFL: TMfAudioEndPoint;
    epMaster: TMfAudioEndPoint;
    tmrTime: TTimer;
    pnlCaption: TPanel;
    lblCaption: TLabel;
    pnlRecIcecast: TPanel;
    pnlRecorder: TPanel;
    btnRecord: TMPxpButton;
    chkPostFx: TMPxpButton;
    chkPreFx: TMPxpButton;
    edFileName: TEdit;
    btnSelectAudiDevice: TMPxpButton;
    lblRecTime: TLabel;
    Label3: TLabel;
    lblRecordingDevice: TLabel;
    lblRecorderStatus: TLabel;
    lblAudioRecorder: TLabel;
    Bevel1: TBevel;
    lblFileExt: TLabel;
    OnRecordingCap: TShape;
    shpRecording: TShape;
    lblRecording: TLabel;
    pnlFXButtons: TPanel;
    avMixGraph: TMfAudioMixVisualizer;

    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);

    procedure tbMasterVolLChange(Sender: TObject);
    procedure tbMasterVolRChange(Sender: TObject);
    procedure tbPflVolChange(Sender: TObject);
    procedure btnPflMuteClick(Sender: TObject);
    procedure tbBalanceDblClick(Sender: TObject);
    procedure tbMasterVolLDblClick(Sender: TObject);
    procedure tbMasterVolRDblClick(Sender: TObject);
    procedure tbPflVolDblClick(Sender: TObject);
    procedure btnSelectAudiDeviceClick(Sender: TObject);
    procedure btnRecordClick(Sender: TObject);
    procedure tmrTimeTimer(Sender: TObject);
    procedure chkPreFxClick(Sender: TObject);
    procedure chkPostFxClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);

  private
    { Private declarations }

    // IDs come from MfAudioEndPoint (IMMDevice ID string)
    FMasterDeviceId: string;  // optional fallback (speakers/monitor)

    // Audio recorder
    prAudioRecorder: TAudioCapture;
    prEndPoint: IMMDevice;
    prDeviceName: LPWSTR;
    prFileName: TFileName;
    prOrgFileName: TFileName;
    prEdited: Boolean;
    prEnableStreamSwitch: Boolean;
    prDisableMmcss: Boolean;
    prDontOverWriteFiles: Boolean;
    prUseDefaultAudioPmcFmt: Boolean;
    prAudioOutputFormat: string;
    prBufferDuration: REFERENCE_TIME;
    prTargetLatency: REFERENCE_TIME;

    // We use timers here, to prevent distortions during capture.
    // The timer is set to 10 millisecond resolution.
    prStopWatch: TStopwatch;
    FTimerRunning: Boolean;
    FRecordingInternal: Boolean;

    prEndPointDataFlow: EDataFlow;
    prEndPointRole: ERole;

    procedure SetVolumeChannels();
    procedure UpdateRecordingUi();

    procedure SetParameters();
    function CreateEngine(): Boolean;
    procedure RemoveEngine();
    procedure UpdateTimeLabel();

    function IsInternalMixerSourceSelected(): Boolean;
    function BuildRecordFileName(out AFileName: TFileName): Boolean;
    procedure StartInternalRecorder();
    procedure StopInternalRecorder();
    procedure StartEndpointRecorder();
    procedure StopEndpointRecorder();

    // Recorder
    // Event handlers.
    procedure OnCapturingStartEvent(Sender: TObject);
    procedure OnCapturingStoppedEvent(Sender: TObject);

  public
    { Public declarations }

    procedure ApplyCurrentSetup();
    procedure SetMasterDeviceId(const ADeviceId: string); // optional
  end;


implementation

{$R *.dfm}

uses
  {System}
  System.Math,
  {Application}
  frmMainMDI;


procedure TMasterDeckFrm.ApplyCurrentSetup();
var
  setup: TRDJSetup;

begin

  if not Assigned(MainMDIFrm) then
    Exit;

  avMixGraph.Enabled := False;

  setup := MainMDIFrm.Setup;

  // Bind endpoint-volume controls to the same devices the engines will render to.
  if (setup.MasterDeviceId <> '') then
    begin

      epMaster.BindToDeviceId(setup.MasterDeviceId);
      epMaster.Mute := False;
      epMaster.ChannelVolume[0] := 0.0;
      epMaster.ChannelVolume[1] := 0.0;
      pmMasterL.EndpointDeviceID := setup.MasterDeviceId;
      pmMasterR.EndpointDeviceID := setup.MasterDeviceId;

      // Assign Graph endpoint.
      avMixGraph.DeviceId := setup.MasterDeviceId;
    end
  else
    begin

      epMaster.GetDefaultDevice();
      epMaster.Mute := False;
      epMaster.ChannelVolume[0] := 0.0;
      epMaster.ChannelVolume[1] := 0.0;
      pmMasterL.EndpointDeviceID := '';
      pmMasterR.EndpointDeviceID := '';
      avMixGraph.DeviceId := '';
    end;

  // PFL
  if setup.PFLEnabled and (setup.PFLDeviceId <> '') then
    begin

      epPFL.BindToDeviceId(setup.PFLDeviceId);
      epPFL.Mute := False;
      epPFL.ChannelVolume[0] := 0.0;
      epPFL.ChannelVolume[1] := 0.0;
      pmPflL.EndpointDeviceID := setup.PFLDeviceId;
      pmPflR.EndpointDeviceID := setup.PFLDeviceId;
    end
  else
    begin

      epPFL.Mute := False;
      epPFL.ChannelVolume[0] := 0.0;
      epPFL.ChannelVolume[1] := 0.0;
      epPFL.GetDefaultDevice();
      pmPflL.EndpointDeviceID := '';
      pmPflR.EndpointDeviceID := '';
    end;

  SetParameters();

  avMixGraph.Enabled := True;
  avMixGraph.Active := True;

  if Assigned(MainMDIFrm) then
    if (MainMDIFrm.Setup.AudioRecorderTapPoint = 0) then
      begin

        // 0
        chkPreFx.Checked := True;
        chkPostFx.Checked := False;
      end
    else
      begin

        // 1
        chkPreFx.Checked := False;
        chkPostFx.Checked := True;
      end;
end;


procedure TMasterDeckFrm.FormCreate(Sender: TObject);
begin

  // No form caption
  SetWindowLong(Handle,
                GWL_STYLE,
                GetWindowLong(Handle, GWL_STYLE) and not WS_CAPTION or WS_BORDER);
end;


procedure TMasterDeckFrm.FormDestroy(Sender: TObject);
begin

  if FRecordingInternal then
    StopInternalRecorder();

  RemoveEngine();
end;


procedure TMasterDeckFrm.FormShow(Sender: TObject);
begin

  //Height := 1538;
  //Width := 507;

  // Apply once on first activation.
  ApplyCurrentSetup();
  UpdateRecordingUi();
end;


procedure TMasterDeckFrm.btnPflMuteClick(Sender: TObject);
begin

  epPFL.Mute := btnPflMute.Checked;
end;


procedure TMasterDeckFrm.SetMasterDeviceId(const ADeviceId: string);
begin

  FMasterDeviceId := ADeviceId;
end;


procedure TMasterDeckFrm.SetVolumeChannels();
var
  baseL,
  baseR: Single;
  bal: Integer;
  gainL,
  gainR: Single;

begin

  baseL := Abs(tbMasterVolL.Position) * 0.01;
  baseR := Abs(tbMasterVolR.Position) * 0.01;

  bal := tbBalance.Position;

  gainL := 1.0;
  gainR := 1.0;

  if (bal < 0) then
    gainR := 1.0 - (Abs(bal) * 0.01)
  else
    if (bal > 0) then
      gainL := 1.0 - (bal * 0.01);

  epMaster.SetChannelScalarVolume(0,
                                  baseL * gainL);
  epMaster.SetChannelScalarVolume(1,
                                  baseR * gainR);
end;


procedure TMasterDeckFrm.tbBalanceDblClick(Sender: TObject);
begin

  tbBalance.AnimateTrackBarToPosition(0,
                                      1);
end;


procedure TMasterDeckFrm.tbMasterVolLChange(Sender: TObject);
var
  vol: Integer;

begin

  if (chkLockMasterFaders.Checked = True) then
    tbMasterVolR.Position := tbMasterVolL.Position;

  SetVolumeChannels();

  vol := tbMasterVolL.Position;
  lblMasterVolL.Caption := Format('%d%%',
                                  [Trunc(vol)]);
end;


procedure TMasterDeckFrm.tbMasterVolRChange(Sender: TObject);
var
  vol: Integer;

begin

  if (chkLockMasterFaders.Checked = True) then
    tbMasterVolL.Position := tbMasterVolR.Position;

  SetVolumeChannels();
  vol := tbMasterVolR.Position;

  lblMasterVolR.Caption := Format('%d%%',
                                  [Trunc(vol)]);
end;


procedure TMasterDeckFrm.tbMasterVolLDblClick(Sender: TObject);
var
  TargetPos: Integer;

begin

  if (tbMasterVolL.Position <> 0) then
    TargetPos := 0
  else
    TargetPos := tbMasterVolL.Maximum;

  tbMasterVolL.AnimateTrackBarToPosition(TargetPos,
                                         2);
end;


procedure TMasterDeckFrm.tbMasterVolRDblClick(Sender: TObject);
var
  TargetPos: Integer;

begin

  if (tbMasterVolR.Position <> 0) then
    TargetPos := 0
  else
    TargetPos := tbMasterVolR.Maximum;

  tbMasterVolR.AnimateTrackBarToPosition(TargetPos,
                                         2);
end;


procedure TMasterDeckFrm.tbPflVolChange(Sender: TObject);
var
  volPosFixed: Integer;
  volScalar: Single;

begin

  volPosFixed := tbPflVol.Position;
  volScalar := tbPflVol.Position / 100;

  if (volScalar > 1.0) then
    volScalar := 1.0
  else
    if (volScalar < 0.0) then
      volScalar := 0.0;

  epPFL.MasterScalarVolume := volScalar;

  lblPflVol.Caption := IntToStr(volPosFixed) + '%';
end;


procedure TMasterDeckFrm.tbPflVolDblClick(Sender: TObject);
var
  TargetPos: Integer;

begin

  if (tbPflVol.Position <> 0) then
    TargetPos := 0
  else
    TargetPos := tbPflVol.Maximum;

  tbPflVol.AnimateTrackBarToPosition(TargetPos,
                                     1);
end;


procedure TMasterDeckFrm.UpdateRecordingUi();
begin

  if FRecordingInternal then
    begin

      btnRecord.Caption := 'Stop';
      btnRecord.Enabled := True;
      btnSelectAudiDevice.Enabled := False;
      edFileName.Enabled := False;
      lblRecorderStatus.Caption := 'Recording internal mixer audio.';
    end
  else
    if Assigned(prAudioRecorder) and (prAudioRecorder.DeviceState = Capturing) then
      begin

        btnRecord.Caption := 'Stop';
        btnRecord.Enabled := True;
        btnSelectAudiDevice.Enabled := False;
        edFileName.Enabled := False;

      end
    else
      begin

        btnRecord.Caption := 'Start';
        btnSelectAudiDevice.Enabled := True;
        edFileName.Enabled := True;
      end;

  chkPreFx.Enabled := IsInternalMixerSourceSelected() and (Tag = 0);
  chkPostFx.Enabled := IsInternalMixerSourceSelected() and (Tag = 0);
end;


function TMasterDeckFrm.IsInternalMixerSourceSelected(): Boolean;
begin

  Result := (prEndPoint = nil) and
            SameText(Trim(WideCharToString(prDeviceName)),
                     RDJ_MASTER_INPUT_NAME);
end;


function TMasterDeckFrm.BuildRecordFileName(out AFileName: TFileName): Boolean;
var
  bFileExists: Boolean;
  i: Integer;
  baseName: string;
  setup: TRDJSetup;

begin

  Result := False;
  AFileName := '';

  baseName := Trim(edFileName.Text);
  if (baseName = '') then
    Exit;

  prFileName := baseName;
  setup := MainMDIFrm.Setup;

  if (prOrgFileName = '') or prEdited then
    begin

      prOrgFileName := prFileName;
      prEdited := False;
    end;

  // Check dir
  if not DirectoryExists(setup.AudioRecordingsPath) or (setup.AudioRecordingsPath = '')  then
    begin

      if not CreateDir(setup.AudioRecordingsPath) then
        prFileName := baseName;
    end;

  if setup.AudioRecorderDontOverWriteAudioFiles then
    begin

      bFileExists := True;
      i := 0;

      while (bFileExists = True) do
        begin

          if FileExists(setup.AudioRecordingsPath + prFileName + prAudioOutputFormat) then
            begin

              if (prOrgFileName = prFileName) then
                prFileName := Format('%s(%d)',
                                     [edFileName.Text,
                                      i])
              else
                begin

                  prFileName := Format('%s(%d)',
                                       [prOrgFileName,
                                        i]);

                  edFileName.Text := prFileName;
                end;
              Inc(i);
            end
          else
            bFileExists := False;
        end;
    end;

  edFileName.Text := prFileName;

  AFileName := setup.AudioRecordingsPath + prFileName + prAudioOutputFormat;
  Result := True;
end;


procedure TMasterDeckFrm.chkPostFxClick(Sender: TObject);
begin

  if Assigned(MainMDIFrm) then
    MainMDIFrm.SetAudioRecorderRecordPostFx(chkPostFx.Checked);
end;


procedure TMasterDeckFrm.chkPreFxClick(Sender: TObject);
begin

  if Assigned(MainMDIFrm) then
    MainMDIFrm.SetAudioRecorderRecordPreFx(chkPreFx.Checked);
end;


// Audio recorder --------------------------------------------------------------

procedure TMasterDeckFrm.OnCapturingStartEvent(Sender: TObject);
begin

  tmrTime.Enabled := True;
  prStopWatch.Start;
  prStopWatch.StartNew;
  btnRecord.Caption := 'Stop Recording';
  lblRecorderStatus.Caption := 'Recording audio.';
  btnRecord.Enabled := True;
end;


procedure TMasterDeckFrm.OnCapturingStoppedEvent(Sender: TObject);
var
  Status: TDeviceState;

begin

  if not Assigned(prAudioRecorder) then
    Exit;

  // Stop the timer and stopwatch.
  prStopwatch.Stop;
  FTimerRunning := False;
  tmrTime.Enabled := False;
  UpdateTimeLabel();
  prStopWatch.Reset;

  Status := prAudioRecorder.DeviceState;

  if (Status = Stopped) or (Status = Error) then
    begin
      lblRecorderStatus.Caption := Format('Capturing stopped. Captured %f Mb.',
                                          [prAudioRecorder.BytesCaptured / (1000 * 1000)]);
    end
  else
    if (Status = Error) then
      begin
        lblRecorderStatus.Caption := Format('Capturing stopped because of an error (hr = %d).',
                                            [E_FAIL]);
      end;

  btnRecord.Caption := 'Start';
  btnSelectAudiDevice.Enabled := True;
  edFileName.Enabled := True;
end;


function TMasterDeckFrm.CreateEngine(): Boolean;
begin

  // Destroy an existing capture engine.
  if Assigned(prAudioRecorder) then
    RemoveEngine();

  SetParameters();

  // Create the capture object.
  prAudioRecorder := TAudioCapture.Create(prEndPoint,
                                          prEnableStreamSwitch,
                                          prDisableMmcss,
                                          prEndPointRole);
  if not Assigned(prAudioRecorder) then
    begin

      InfoMsg(optIDE,
              'Unable create the WASCapture engine.',
              E_POINTER,
              Handle);
      Exit(False);
    end
  else
    begin

      // Set event handler.
      prAudioRecorder.OnStartCapturing := OnCapturingStartEvent;
      prAudioRecorder.OnStoppedCapturing := OnCapturingStoppedEvent;

      InfoMsg(optIDE,
              Format('The AudioRecorder "%s" is successfully initialized.',
                     [WideCharToString(prDeviceName)]),
              S_OK,
              Handle);

      prStopWatch := TStopwatch.Create();
      Result := True;
    end;
end;


procedure TMasterDeckFrm.RemoveEngine();
begin

  if Assigned(prAudioRecorder) then
    begin

      prAudioRecorder.Stop();
      Sleep(200);
      // Destroy the engine
      prAudioRecorder.Shutdown();
      prAudioRecorder.OnStartCapturing := nil;
      prAudioRecorder.OnStoppedCapturing := nil;
      FreeAndNil(prAudioRecorder);
    end;
end;


procedure TMasterDeckFrm.btnSelectAudiDeviceClick(Sender: TObject);
begin

  // Create the dialog if it's not allready done.
  if not Assigned(DevicesDlg) then
    begin
      Application.CreateForm(TAudioDevicesDlg,
                             DevicesDlg);
      DevicesDlg.Visible := False;
    end;

  // Which flow should be presented.
  DevicesDlg.DataFlow := eRender;

  // Ask the user to select one.
  if (DevicesDlg.ShowModal = mrOk) then
    begin
      prEndPointDataFlow := DevicesDlg.DataFlow;
      prEndPoint := DevicesDlg.EndPointDevice;
      prDeviceName := DevicesDlg.DeviceName;

      UpdateRecordingUi();
      lblRecordingDevice.Caption := Format('Recorder source: %s',
                                           [WideCharToString(prDeviceName)]);

      if IsInternalMixerSourceSelected() then
        lblRecorderStatus.Caption := 'Ready for recording from RDJ Master Input.'
      else
        lblRecorderStatus.Caption := 'Ready for recording.';

      btnRecord.Enabled := True;
    end
  else
    begin
      lblRecorderStatus.Caption := 'Select a device.';
    end;
end;


procedure TMasterDeckFrm.UpdateTimeLabel();
var
  TS: TTimeSpan;
  Hours: Integer;
  Minutes: Integer;
  Seconds: Integer;
  Hundredths: Integer;

begin

  TS := prStopwatch.Elapsed;

  Hours := Trunc(TS.TotalHours);
  Minutes := TS.Minutes;
  Seconds := TS.Seconds;
  Hundredths := TS.Milliseconds div 10;

  lblRecTime.Caption := Format('Recorded: %.2d:%.2d:%.2d.%.2d',
                               [Hours, Minutes, Seconds, Hundredths]);
end;


procedure TMasterDeckFrm.StartInternalRecorder();
var
  AFileName: TFileName;
  RecPre: Boolean;
  RecPost: Boolean;

begin

  if not Assigned(MainMDIFrm) then
    Exit;

  if not BuildRecordFileName(AFileName) then
    Exit;

  RecPre := chkPreFx.Checked;
  RecPost := chkPostFx.Checked;

  if (not RecPre) and (not RecPost) then
    begin

      lblRecorderStatus.Caption := 'Select Pre-FX and/or Post-FX.';
      Exit;
    end;

  if MainMDIFrm.StartInternalMixerRecording(AFileName,
                                            RecPre,
                                            RecPost) then
    begin

      FRecordingInternal := True;
      prStopwatch := TStopwatch.StartNew();
      FTimerRunning := True;
      tmrTime.Enabled := True;
      Tag := 1;

      btnRecord.Caption := 'Stop Recording';
      btnSelectAudiDevice.Enabled := False;
      edFileName.Enabled := False;
      chkPreFx.Enabled := False;
      chkPostFx.Enabled := False;

      if RecPre and RecPost then
        lblRecorderStatus.Caption := 'Recording internal mixer audio (Pre-FX + Post-FX).'
      else if RecPre then
        lblRecorderStatus.Caption := 'Recording internal mixer audio (Pre-FX).'
      else
        lblRecorderStatus.Caption := 'Recording internal mixer audio (Post-FX).';
    end
  else
    begin

      FRecordingInternal := False;
      Tag := 0;
      lblRecorderStatus.Caption := 'Failed recording internal mixer audio!';
    end;
end;


procedure TMasterDeckFrm.StopInternalRecorder();
begin

  if Assigned(MainMDIFrm) then
    MainMDIFrm.StopInternalMixerRecording();

  if FTimerRunning then
    begin

      prStopwatch.Stop;
      FTimerRunning := False;
      tmrTime.Enabled := False;
      UpdateTimeLabel();
    end;

  FRecordingInternal := False;
  Tag := 0;
  btnRecord.Caption := 'Start';
  btnSelectAudiDevice.Enabled := True;
  edFileName.Enabled := True;
  chkPreFx.Enabled := True;
  chkPostFx.Enabled := True;
  lblRecorderStatus.Caption := 'Internal mixer recording stopped.';
end;


procedure TMasterDeckFrm.StartEndpointRecorder();
var
  AFileName: TFileName;
  bSuccess: Boolean;

begin

  // Create the engine with setup-parameters
  if not CreateEngine() then
    Exit;

  if not BuildRecordFileName(AFileName) then
    Exit;

  // Initialize.
  bSuccess := prAudioRecorder.Initialize(prBufferDuration,
                                         prTargetLatency,
                                         prUseDefaultAudioPmcFmt);

  if bSuccess then
    begin

      prStopwatch := TStopwatch.StartNew();
      FTimerRunning := bSuccess;
      tmrTime.Enabled := bSuccess;
      Tag := 1;

      // Capture the audio stream from the chosen rendering device.
      bSuccess := prAudioRecorder.Start(AFileName);

      if bSuccess then
        begin

          btnRecord.Caption := 'Stop Recording';
          lblRecorderStatus.Caption := 'Recording audio';
          btnSelectAudiDevice.Enabled := False;
          edFileName.Enabled := False;
        end;
    end
  else
    begin
      InfoMsg(optIDE,
              'Unable to start capture.',
              E_FAIL,
              Handle);

      prStopwatch.Stop;
      FTimerRunning := bSuccess;
      tmrTime.Enabled := bSuccess;
      UpdateTimeLabel();
      btnRecord.Caption := 'Start';
      lblRecorderStatus.Caption := 'Failed recording audio!';
      Tag := 0;
      Exit;
    end;
end;


procedure TMasterDeckFrm.StopEndpointRecorder();
begin

  if Assigned(prAudioRecorder) then
    begin

      prAudioRecorder.Stop();
      RemoveEngine();
    end;

  if FTimerRunning then
    begin

      prStopwatch.Stop;
      FTimerRunning := False;
      tmrTime.Enabled := False;
      UpdateTimeLabel();
    end;

  btnRecord.Caption := 'Start';
  lblRecorderStatus.Caption := 'Capturing stopped.';
  btnSelectAudiDevice.Enabled := True;
  edFileName.Enabled := True;
  Tag := 0;
end;


procedure TMasterDeckFrm.btnRecordClick(Sender: TObject);
begin

  if (Tag = 1) then
    begin
      if FRecordingInternal then
        StopInternalRecorder()
      else
        StopEndpointRecorder();

      Sleep(10);
      UpdateRecordingUi();
      Exit;
    end;

  if IsInternalMixerSourceSelected() then
    StartInternalRecorder()
  else
    StartEndpointRecorder();

  Sleep(10);
  UpdateRecordingUi();
end;


procedure TMasterDeckFrm.tmrTimeTimer(Sender: TObject);
begin

  if FTimerRunning then
    UpdateTimeLabel();
end;


procedure TMasterDeckFrm.SetParameters();
var
  pSetupRec: TRDJSetup;

begin

  // We got those parameters from the endpoint select dialog:
  // prEndPointDataFlow
  // prEndPoint
  // prDeviceName

  // Fixed parameter.
  prEndPointRole := eMultimedia;

  // Get the rest of the parameters from setup.
  pSetupRec := MainMDIFrm.Setup;

  if pSetupRec.AudioRecorderAutoBufferSize then
    prBufferDuration := 0
  else
    prBufferDuration := (REFTIMES_PER_MILLISEC) * pSetupRec.AudioRecorderCaptureBufferSize;

  prTargetLatency := pSetupRec.AudioRecorderSystemLatency;

  prEnableStreamSwitch := pSetupRec.AudioRecorderEnableStreamSwitchDetection;
  prDisableMmcss := pSetupRec.AudioRecorderDisableMMCSS;
  prDontOverWriteFiles := pSetupRec.AudioRecorderDontOverWriteAudioFiles;
  prUseDefaultAudioPmcFmt := pSetupRec.AudioRecorderUsePCMFormat;

  case pSetupRec.AudioRecorderAudioFormat of
    0: prAudioOutputFormat := '.wav';
    1: prAudioOutputFormat := '.flac';
  end;

  lblFileExt.Caption := prAudioOutputFormat;
end;

end.
