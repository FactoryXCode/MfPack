// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfCaptureVideoFromGPU version 2
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module:  frmCaptureVideoFromGpu.pas
// Kind: Pascal / Delphi unit
// Release date: 13-08-2025
// Language: ENU
//
// Revision Version: 3.1.9
// Description: GUI for MfCaptureVideoFromGPU version 2
//
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Carmen (carmenh), Tony Kalf (maXcomX)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 05/05/2026 All                 Bauhaus release  SDK 10.0.26100.4654 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: -
//
// Related objects: -
// Related projects: MfPackX319/Samples/MfCaptureVideoFromGPU II
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.26100.4654
//
// Todo: -
//
//==============================================================================
// Source: -
//
// Copyright (c) FactoryX. All rights reserved.
//==============================================================================
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
unit frmCaptureVideoFromGpu;

interface

uses

  {WinApi}
  WinApi.Windows,
  WinApi.Messages,
  WinApi.ShellAPI,
  {System}
  System.SysUtils,
  System.Classes,
  System.StrUtils,
  {Vcl}
  Vcl.Controls,
  Vcl.Forms,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.Dialogs,
  Vcl.Graphics,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfUtils,
  {CoreAudioApi}
  WinApi.CoreAudioApi.MMDeviceApi,
  WinApi.CoreAudioApi.FunctionDiscoveryKeys_devpkey,
  {ActiveX}
  WinApi.ActiveX,
  WinApi.ActiveX.PropSys,
  WinApi.ActiveX.PropIdl,
  {Application}
  CaptureStreamEngine,
  LoopbackAudioEngine,
  ScreenActivityPinger;

const

  DEFAULT_OUTPUT_FILENAME: string = 'capture_output.mp4';

  HOTKEY_START = 1;
  HOTKEY_STOP = 2;
  HOTKEY_TOGGLE_UI = 3;

type

  TfrmCapture = class(TForm)
    pnlTop: TPanel;
    pnlPreview: TPanel;
    pnlBottom: TPanel;
    lblMonitor: TLabel;
    lblOutput: TLabel;
    lblFPS: TLabel;
    cbxMonitor: TComboBox;
    edtOutput: TEdit;
    btnBrowse: TButton;
    mmoLog: TMemo;
    lblAudio: TLabel;
    cbxAudioDevice: TComboBox;
    Bevel1: TBevel;
    cbxResolutions: TComboBox;
    cbxFrameRate: TComboBox;
    lblResolution: TLabel;
    lblFrameRate: TLabel;
    Bevel2: TBevel;
    rbRecVideoAndAudio: TRadioButton;
    rbRecVideo: TRadioButton;
    rbRecAudio: TRadioButton;
    butStart: TButton;
    butStop: TButton;
    lblStatus: TLabel;
    Bevel3: TBevel;
    lblAudioBitrate: TLabel;
    cbxAudioBitrate: TComboBox;
    lblAudioCodec: TLabel;
    cbxAudioCodec: TComboBox;
    cbxAudioFormat: TComboBox;
    Bevel4: TBevel;
    cbxKeepOnTop: TCheckBox;
    cbxHotKeys: TCheckBox;
    Label1: TLabel;
    lblModeCaption: TLabel;
    lblMode: TLabel;
    lblRecTimeCaption: TLabel;
    lblRecTime: TLabel;
    lblAudioStateCaption: TLabel;
    lblAudioState: TLabel;
    tmrGUI: TTimer;
    Bevel5: TBevel;
    Bevel6: TBevel;
    butPlayOutput: TButton;

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);

    procedure butStartClick(Sender: TObject);
    procedure butStopClick(Sender: TObject);
    procedure butPlayOutputClick(Sender: TObject);

    procedure btnBrowseClick(Sender: TObject);
    procedure rbRecVideoAndAudioClick(Sender: TObject);
    procedure rbRecVideoClick(Sender: TObject);
    procedure rbRecAudioClick(Sender: TObject);
    procedure cbxAudioCodecChange(Sender: TObject);
    procedure cbxHotKeysClick(Sender: TObject);
    procedure cbxKeepOnTopClick(Sender: TObject);

    procedure tmrGUITimer(Sender: TObject);
    procedure AnyUiChanged(Sender: TObject);
    procedure cbxAudioCodecCloseUp(Sender: TObject);

  private

    FEngine: TCaptureStreamEngine;
    FOutputs: TArray<TDXGIOutputInfo>;
    FLastFrameTime: Double;
    // FPS smoothing (UI only)
    FFpsAvg: Double;
    FFpsAvgCount: Integer;
    FAudioDeviceIds: TStringList;
    FCaptureMode: TCaptureMode;
    FAudioCodec: TAudioCodec;
    FAudioFileFormat: TAudioFileFormat;
    FIsRecording: Boolean;
    FRecordingStartTick: UInt64;
    FAudioOnly: TLoopbackAudioOnlyRecorder;
    FActivityPinger: TScreenActivityPinger;
    // Resolution and frame rate.
    FCaptureWidth,
    FCaptureHeigth: UINT;
    FFrameRate: UINT32;
    // Hotkey
    FHotkeysActive: Boolean;

    procedure CheckChecks();
    procedure InitMonitors();
    procedure InitAudioDevices();

    procedure ApplyUiGuardrails();

    procedure UpdateUiIndicators();
    function FormatElapsed(const ElapsedMs: UInt64): string;

    function SelectedAacAvgBytesPerSec: Cardinal;

    procedure CaptureProgress(Sender: TObject;
                              FrameIndex: Int64;
                              Msec: Double);

    procedure CaptureError(Sender: TObject;
                           const Msg: string);

    function GetSelectedAudioDeviceId(): string;

    procedure GetRenderSettings();

    // Hotkey use.
    procedure WMHotKey(var Msg: TMessage); message WM_HOTKEY;
    procedure EnableGlobalHotkeys();
    procedure DisableGlobalHotkeys();
    procedure ApplyHotkeyOption();

    procedure HideUI();
    procedure ShowUI();
    procedure ToggleUI();
  end;

var
  frmCapture: TfrmCapture;


implementation


{$R *.dfm}

procedure TfrmCapture.FormCreate(Sender: TObject);
begin

  mmoLog.Clear;
  lblStatus.Caption := 'Idle';
  lblStatus.Font.Color := clGray;

  FIsRecording := False;
  FRecordingStartTick := 0;
  FFpsAvg := 0.0;
  FFpsAvgCount := 0;

  if Assigned(tmrGUI) then
    tmrGUI.Enabled := False;

  // Initial Create engine with default values. 1080p+ or also named FHD+ (16:10)
  // NOTE: All other settings will be handled in btnStartClick.
  FEngine := TCaptureStreamEngine.Create(pnlPreview.Handle,
                                         1920,
                                         1200,
                                         30);

  FEngine.OnProgress := CaptureProgress;
  FEngine.OnError := CaptureError;

  FAudioDeviceIds := TStringList.Create;

  InitMonitors();
  InitAudioDevices();

  // Default recording modes
  rbRecVideoAndAudio.Checked := True;

  if Assigned(cbxHotkeys) then
    cbxHotkeys.Checked := True; // default.

  // Enable hints (tooltips) for UI guardrails
  Application.ShowHint := True;
  Self.ShowHint := True;

  // Wire generic UI-change handler (guardrails + indicators) without changing behavior
  if Assigned(cbxMonitor) then cbxMonitor.OnChange := AnyUiChanged;
  if Assigned(edtOutput) then edtOutput.OnChange := AnyUiChanged;
  if Assigned(cbxAudioDevice) then cbxAudioDevice.OnChange := AnyUiChanged;
  if Assigned(cbxAudioBitrate) then cbxAudioBitrate.OnChange := AnyUiChanged;
  if Assigned(cbxResolutions) then cbxResolutions.OnChange := AnyUiChanged;
  if Assigned(cbxFrameRate) then cbxFrameRate.OnChange := AnyUiChanged;

  ApplyUiGuardrails();
  UpdateUiIndicators();
end;


procedure TfrmCapture.FormDestroy(Sender: TObject);
begin

  DisableGlobalHotkeys();

  if Assigned(FActivityPinger) then
    begin
      FActivityPinger.Stop();
      FreeAndNil(FActivityPinger);
    end;

  FreeAndNil(FAudioDeviceIds);
  FreeAndNil(FEngine);
end;


procedure TfrmCapture.btnBrowseClick(Sender: TObject);
var
  dlg: TSaveDialog;

begin

  dlg := TSaveDialog.Create(Self);

  try

    if rbRecAudio.Checked then
      dlg.Filter := 'Audio Files|*.wav;*.flac|WAV Files|*.wav|FLAC Files|*.flac'
    else
      dlg.Filter := 'MP4 Files|*.mp4';
    if rbRecAudio.Checked then
      dlg.DefaultExt := LowerCase(cbxAudioFormat.Text)
    else
      dlg.DefaultExt := 'mp4';
    dlg.FileName := edtOutput.Text;

    if dlg.Execute then
      edtOutput.Text := dlg.FileName;
  finally
    dlg.Free;
  end;
end;


procedure TfrmCapture.butStartClick(Sender: TObject);
var
  sFileName: string;
  rOutputRect: TRect;

begin

  if (cbxMonitor.ItemIndex < 0) then
    begin
      ShowMessage('Please select a monitor.');
      Exit;
    end;

  if (edtOutput.Text = '') then
    begin
      ShowMessage('Please enter or select an output file.');
      Exit;
    end;

  CheckChecks();


  // Reset FPS smoothing (UI only)
  FFpsAvg := 0.0;
  FFpsAvgCount := 0;

  butStart.Enabled := False;
  butStop.Enabled := True;
  butPlayOutput.Enabled := False;

  // The file to write to
  sFileName := edtOutput.Text;

  // Get height, width and sample rate if changed inbetween.
  GetRenderSettings();

  FEngine.SetFrameWidth := FCaptureWidth;
  FEngine.SetFrameHeight := FCaptureHeigth;
  FEngine.SetFrameRate := FFrameRate;

  // Set the audio device ID.
  FEngine.AudioDeviceID := GetSelectedAudioDeviceId();

  // Recorder selection
  if rbRecVideo.Checked then
    FEngine.CaptureMode := cmVideoOnly
  else
    if rbRecAudio.Checked then
      FEngine.CaptureMode := cmAudioOnly
  else
    FEngine.CaptureMode := cmAudioVideo;


  // ---------------------------------------------------------------------------
  // AUDIO ONLY: use separate recorder unit, do NOT start FEngine
  // ---------------------------------------------------------------------------
  if (FEngine.CaptureMode = cmAudioOnly) then
    begin

      // Stop activity pinger (audio-only should not use it)
      FreeAndNil(FActivityPinger);

      // Create recorder if needed
      if not Assigned(FAudioOnly) then
        FAudioOnly := TLoopbackAudioOnlyRecorder.Create(Self);

      mmoLog.Lines.Add('Starting AUDIO-ONLY recording: ' + sFileName);

      // Start recorder ////////////////////////////////////////////////////////

      FAudioOnly.StartToFile(sFileName,
                             FAudioFileFormat,
                             0,
                             FEngine.AudioDeviceID);


      //////////////////////////////////////////////////////////////////////////
      lblStatus.Caption := 'Recording (audio only)...';
      lblStatus.Font.Color := clLime;

      FIsRecording := True;
      FRecordingStartTick := GetTickCount64;

      if Assigned(tmrGUI) then
        tmrGUI.Enabled := True;

      ApplyUiGuardrails();
      UpdateUiIndicators();

      mmoLog.Lines.Add('--- Audio-Only Capture Started ---');
      mmoLog.Lines.Add('Output file: ' + sFileName);

      // debug check full path.
      mmoLog.Lines.Add('Audio-only file (absolute): ' + ExpandFileName(FAudioOnly.OutputFileName));

      Exit; // IMPORTANT: Don't start video pipeline and ActivityPinger.
    end;


  // Audio&Video and Video only. ///////////////////////////////////////////////

  if (FAudioCodec = acAac) then
    FEngine.EnableLoopbackAudioAacMp4($3 {stereo mask},
                                      SelectedAacAvgBytesPerSec)
  else
    if FAudioCodec = acFlac then
      FEngine.EnableLoopbackAudioFlacMp4(nil,
                                         $3)
  else
    if (FAudioCodec = acNone) then
      FEngine.DisableAudio;

  mmoLog.Lines.Add('Starting capture: ' + sFileName);

  // Start recording ///////////////////////////////////////////////////////////

  FEngine.StartCapture(sFileName,
                       FOutputs[cbxMonitor.ItemIndex]);

  //////////////////////////////////////////////////////////////////////////////

  // Create and start the activity pinger on the SAME DXGI output.
  // The pinger is used to keep the capturing alive especially when
  // using 2 screens on separate video outputs.
  // The pinger unit is ScreenActivityPinger.pas
  rOutputRect := FEngine.GetSelectedOutputRect();

  if Assigned(FActivityPinger) then
    begin
      FActivityPinger.Stop();
      FreeAndNil(FActivityPinger);
    end;

  FActivityPinger := TScreenActivityPinger.Create(rOutputRect,
                                                  FFrameRate,
                                                  3, // pixels, width
                                                  1, // pixels, height
                                                  clLime); // pixel color

  FActivityPinger.Start();
  lblStatus.Caption := 'Recording...';
  lblStatus.Font.Color := clLime;

  FIsRecording := True;
  FRecordingStartTick := GetTickCount64;

  if Assigned(tmrGUI) then
    tmrGUI.Enabled := True;

  UpdateUiIndicators();

  mmoLog.Lines.Add('--- Capture Started ---');
  mmoLog.Lines.Add('Output file: ' + sFileName);
end;


procedure TfrmCapture.butStopClick(Sender: TObject);
begin

  if Assigned(FAudioOnly) then
    FAudioOnly.Stop;

  if Assigned(FEngine) then
    FEngine.StopCapture();

  if Assigned(FActivityPinger) then
    begin
      FActivityPinger.Stop();
      FreeAndNil(FActivityPinger);
    end;

  lblStatus.Caption := 'Idle';
  lblStatus.Font.Color := clGray;

  mmoLog.Lines.Add('--- Capture Stopped ---');

  FIsRecording := False;
  FFpsAvg := 0.0;
  FFpsAvgCount := 0;
  if Assigned(tmrGUI) then
    tmrGUI.Enabled := False;

  ApplyUiGuardrails();
  UpdateUiIndicators();

  butStart.Enabled := True;
  butStop.Enabled := False;
  butPlayOutput.Enabled := True;

  // UI only
  if Assigned(lblFPS) then
    lblFPS.Caption := 'FPS: 0.0';
end;


procedure TfrmCapture.butPlayOutputClick(Sender: TObject);
var
  path: string;

begin

  if (butStart.Enabled = True) and (butStop.Enabled = False) then
    begin

      path := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)));
      // Play file with the default player.
      ShellExecute(Handle,
                   'open',
                   StrToPWideChar(path + edtOutput.Text),
                   nil,
                   nil,
                   SW_SHOWNORMAL);
    end;
end;


procedure TfrmCapture.InitMonitors;
var
  i: Integer;
  Monitorinfo: TDXGIOutputInfo;

begin

  FOutputs := FEngine.EnumerateOutputs();

  cbxMonitor.Items.BeginUpdate;
    try
      cbxMonitor.Items.Clear;
      for i := 0 to High(FOutputs) do
        begin
          Monitorinfo := FOutputs[i];
          cbxMonitor.Items.Add(
          Format('Display %d - %s',
                 [Monitorinfo.OutputIndex,
                  Monitorinfo.DeviceName]));

        end;
    finally
      cbxMonitor.Items.EndUpdate;
    end;

  if (cbxMonitor.Items.Count > 0) then
    cbxMonitor.ItemIndex := 0;
end;


procedure TfrmCapture.rbRecAudioClick(Sender: TObject);
begin

  CheckChecks();
end;


procedure TfrmCapture.rbRecVideoAndAudioClick(Sender: TObject);
begin

  CheckChecks();
end;


procedure TfrmCapture.rbRecVideoClick(Sender: TObject);
begin

  CheckChecks();
end;


procedure TfrmCapture.InitAudioDevices();
var
  hr: HResult;
  Enum: IMMDeviceEnumerator;
  Col: IMMDeviceCollection;
  Dev: IMMDevice;
  Count: UINT;
  i: Integer;
  IdW: LPWSTR;
  Prop: IPropertyStore;
  PropVar: PROPVARIANT;
  sFriendlyName: string;

begin

  cbxAudioDevice.Items.BeginUpdate;

  try
    cbxAudioDevice.Items.Clear;
    FAudioDeviceIds.Clear;

    cbxAudioDevice.Items.Add('<Default output device>');

    hr := CoCreateInstance(CLSID_MMDeviceEnumerator,
                           nil,
                           CLSCTX_ALL,
                           IID_IMMDeviceEnumerator,
                           Enum);
    if FAILED(hr) then
      Exit;

    hr := Enum.EnumAudioEndpoints(eRender,
                                  DEVICE_STATE_ACTIVE,
                                  Col);
    if FAILED(hr) then
      Exit;

    hr := Col.GetCount(Count);
    if FAILED(hr) then
      Exit;

    for i := 0 to Integer(Count) - 1 do
      begin

        hr := (Col.Item(i,
                        Dev));
        if FAILED(hr) then
          Continue;

        hr := Dev.GetId(IdW);
        if FAILED(hr) then
          Continue;

        hr := Dev.OpenPropertyStore(STGM_READ,
                                    Prop);
        if FAILED(hr) then
          begin
            CoTaskMemFree(IdW);
            Continue;
          end;

      PropVariantInit(PropVar);

      hr := Prop.GetValue(PKEY_Device_FriendlyName,
                          PropVar);
      if FAILED(hr) then
        begin
          PropVariantClear(PropVar);
          CoTaskMemFree(IdW);
          Continue;
        end;

      sFriendlyName := PropVar.pwszVal;

      cbxAudioDevice.Items.Add(sFriendlyName);
      FAudioDeviceIds.Add(IdW);

      CoTaskMemFree(IdW);
      PropVariantClear(PropVar);
    end;

    cbxAudioDevice.ItemIndex := 0;
  finally

    cbxAudioDevice.Items.EndUpdate;
  end;
end;


function TfrmCapture.GetSelectedAudioDeviceId(): string;
var
  idx: Integer;

begin

  Result := '';
  idx := cbxAudioDevice.ItemIndex;

  if (idx <= 0) then
    begin

      Result := '';  // Default device, WASAPI will use the default device on your machine.
      Exit;
    end;

  if (idx - 1 < FAudioDeviceIds.Count) then
    Result := FAudioDeviceIds[idx - 1];
end;


procedure TfrmCapture.cbxKeepOnTopClick(Sender: TObject);
begin

  // Keep on top.
  if cbxKeepOnTop.Checked then

    SetWindowPos(Handle,
                 HWND_TOPMOST,
                 0,
                 0,
                 0,
                 0,
                 SWP_NoMove or SWP_NoSize)
  else
    SetWindowPos(Handle,
                 HWND_NOTOPMOST,
                 0,
                 0,
                 0,
                 0,
                 SWP_NoMove or SWP_NoSize);
end;


procedure TfrmCapture.CheckChecks();
var
  fn: string;

begin

  if (Length(edtOutput.Text) < 5) then
    begin

      // Set to default.
      edtOutput.Text := DEFAULT_OUTPUT_FILENAME;
      rbRecVideoAndAudio.Checked := True;
    end;

  fn := edtOutput.Text;

  if rbRecVideoAndAudio.Checked or rbRecVideo.Checked then
    edtOutput.Text := ChangeFileExt(fn,
                                    '.mp4')
  else
    if rbRecAudio.Checked and (cbxAudioFormat.ItemIndex = 0) then
      edtOutput.Text := ChangeFileExt(fn,
                                      '.wav')
  else
    if rbRecAudio.Checked and (cbxAudioFormat.ItemIndex = 1) then
      edtOutput.Text := ChangeFileExt(fn,
                                      '.flac');

  if rbRecVideoAndAudio.Checked then
    begin

      cbxAudioCodec.Enabled := True;
      cbxAudioDevice.Enabled := True;
      cbxAudioBitrate.Enabled := True;
      FCaptureMode := cmAudioVideo;
      cbxAudioCodec.Enabled := True;
      cbxFrameRate.Enabled := True;
      cbxResolutions.Enabled := True;
      cbxAudioFormat.Enabled := False;
      FCaptureMode := cmAudioVideo;

      case cbxAudioCodec.ItemIndex of
        0: FAudioCodec := acAac;
        1: FAudioCodec := acFlac;
      end;
    end;

  if rbRecVideo.Checked then
    begin

      cbxAudioDevice.Enabled := False;
      cbxAudioCodec.Enabled := False;
      cbxAudioBitrate.Enabled := False;
      cbxAudioCodec.Enabled := False;
      cbxResolutions.Enabled := False;
      cbxAudioCodec.Enabled := False;
      cbxMonitor.Enabled := True;
      cbxFrameRate.Enabled := True;
      cbxResolutions.Enabled := True;
      FCaptureMode := cmVideoOnly;
    end;

  if rbRecAudio.Checked then
    begin

      cbxResolutions.Enabled := False;
      cbxFrameRate.Enabled := False;
      cbxAudioCodec.Enabled := False;
      cbxMonitor.Enabled := False;
      cbxAudioBitrate.Enabled := False;
      cbxAudioFormat.Enabled := True;
      cbxAudioDevice.Enabled := True;
      FCaptureMode := cmAudioOnly;

      case cbxAudioFormat.ItemIndex of
        0: FAudioFileFormat := aoWavPcm16;
        1: FAudioFileFormat := aoFlac;
      end;
    end;

  ApplyUiGuardrails();
  UpdateUiIndicators();
end;


function TfrmCapture.FormatElapsed(const ElapsedMs: UInt64): string;
var
  TotalSec,
  H,
  M,
  S: UInt64;

begin

  TotalSec := ElapsedMs div 1000;

  H := TotalSec div 3600;
  M := (TotalSec div 60) mod 60;
  S := TotalSec mod 60;
  Result := Format('%.2d:%.2d:%.2d', [H, M, S]);
end;


procedure TfrmCapture.UpdateUiIndicators;
var
  ModeText: string;
  AudioText: string;
  Ext: string;

begin

  // Mode (UI only: reflects selected radio buttons)
  if rbRecAudio.Checked then
    ModeText := 'Audio only'
  else if rbRecVideo.Checked then
    ModeText := 'Video only'
  else
    ModeText := 'Video + Audio';

  // Add container hint from output filename when available
  Ext := LowerCase(ExtractFileExt(Trim(edtOutput.Text)));
  if (Ext <> '') then
    ModeText := ModeText + ' (' + Ext + ')';

  if Assigned(lblMode) then
    lblMode.Caption := ModeText;

  // Audio state (UI only)
  if rbRecVideo.Checked then
    AudioText := 'Disabled'
  else if rbRecAudio.Checked then
    begin
      // Audio-only: depends on selected format
      if Assigned(cbxAudioFormat) and (cbxAudioFormat.ItemIndex >= 0) then
        AudioText := cbxAudioFormat.Text
      else
        AudioText := 'Enabled';
    end
  else
    begin
      // Video + Audio
      if Assigned(cbxAudioCodec) and (cbxAudioCodec.ItemIndex >= 0) then
        AudioText := cbxAudioCodec.Text
      else
        AudioText := 'Enabled';
    end;

  if Assigned(cbxAudioDevice) and (not rbRecVideo.Checked) then
    begin
      if (cbxAudioDevice.ItemIndex < 0) then
        AudioText := AudioText + ' (no device)'
      else
        AudioText := AudioText + ' (' + cbxAudioDevice.Text + ')';
    end;

  if Assigned(lblAudioState) then
    lblAudioState.Caption := AudioText;

  // Recording time label (set here so changes to UI state reset it)
  if (not FIsRecording) and Assigned(lblRecTime) then
    lblRecTime.Caption := '00:00:00';

  // Make the live status color consistent
  if Assigned(lblStatus) then
    begin
      if FIsRecording then
        lblStatus.Font.Color := clLime
      else
        lblStatus.Font.Color := clGray;
    end;
end;


procedure TfrmCapture.tmrGUITimer(Sender: TObject);
var
  Elapsed: UInt64;
begin

  // Lightweight UI refresher. Does not affect capture behavior.
  if FIsRecording then
    begin
      Elapsed := GetTickCount64 - FRecordingStartTick;
      if Assigned(lblRecTime) then
        lblRecTime.Caption := FormatElapsed(Elapsed);
    end;

  UpdateUiIndicators();
end;


procedure TfrmCapture.AnyUiChanged(Sender: TObject);
begin
  // UI-only guardrails: do not change capture behavior, only enable/disable and show hints
  ApplyUiGuardrails();
  UpdateUiIndicators();
end;


procedure TfrmCapture.ApplyUiGuardrails();
var
  IsVideo: Boolean;
  IsAudio: Boolean;
  IsAudioOnly: Boolean;
  OutExt: string;
  NeedsMonitor: Boolean;
  CanStart: Boolean;
  IsAac: Boolean;
  IsFlac: Boolean;
  HasOutput: Boolean;

begin

  if not Assigned(butStart) or not Assigned(butStop) then
    Exit;

  IsAudioOnly := rbRecAudio.Checked;
  IsVideo := rbRecVideo.Checked or rbRecVideoAndAudio.Checked;
  IsAudio := rbRecAudio.Checked or rbRecVideoAndAudio.Checked;

  OutExt := LowerCase(ExtractFileExt(Trim(edtOutput.Text)));
  HasOutput := Trim(edtOutput.Text) <> '';

  // Determine selected codec (only meaningful in Video+Audio mode)
  IsAac := (Assigned(cbxAudioCodec) and (cbxAudioCodec.ItemIndex = 0));
  IsFlac := (Assigned(cbxAudioCodec) and (cbxAudioCodec.ItemIndex = 1));

  // ---------------------------------------------------------------------------
  // Tooltips / hints (show "why", not "what")
  // ---------------------------------------------------------------------------
  if Assigned(edtOutput) then
    begin

      edtOutput.ShowHint := True;
      if IsAudioOnly then
        edtOutput.Hint := 'Audio-only output. Recommended extensions: .wav or .flac'
      else
        edtOutput.Hint := 'Video output. Recommended extension: .mp4';
    end;

  if Assigned(cbxAudioCodec) then
    begin

      cbxAudioCodec.ShowHint := True;
      cbxAudioCodec.Hint := 'Audio codec used for MP4 when recording video+audio.';
    end;

  if Assigned(cbxAudioBitrate) then
    begin

      cbxAudioBitrate.ShowHint := True;
      if IsFlac then
        cbxAudioBitrate.Hint := 'Bitrate selection applies to AAC only. FLAC is lossless and ignores bitrate.'
      else
        cbxAudioBitrate.Hint := 'AAC target bitrate (approx). Higher values increase quality and file size.';
    end;

  if Assigned(cbxAudioFormat) then
    begin

      cbxAudioFormat.ShowHint := True;
      cbxAudioFormat.Hint := 'Audio-only file format.';
    end;

  if Assigned(cbxAudioDevice) then
    begin

    cbxAudioDevice.ShowHint := True;
    cbxAudioDevice.Hint := 'Select the Windows render endpoint used for loopback capture.';
    end;

  if Assigned(cbxMonitor) then
    begin

      cbxMonitor.ShowHint := True;
      cbxMonitor.Hint := 'Select the display to capture.';
    end;

  if Assigned(btnBrowse) then
    begin

      btnBrowse.ShowHint := True;
      btnBrowse.Hint := 'Choose output file name and location.';
    end;

  // ---------------------------------------------------------------------------
  // Enable/disable controls (UI guardrails only)
  // ---------------------------------------------------------------------------
  // Audio codec/bitrate are relevant only for Video+Audio.
  if Assigned(cbxAudioCodec) then
    cbxAudioCodec.Enabled := rbRecVideoAndAudio.Checked;

  if Assigned(cbxAudioBitrate) then
    cbxAudioBitrate.Enabled := rbRecVideoAndAudio.Checked and IsAac;

  if Assigned(lblAudioBitrate) then
    lblAudioBitrate.Enabled := Assigned(cbxAudioBitrate) and cbxAudioBitrate.Enabled;

  // Audio-only format selector is relevant only for Audio-only.
  if Assigned(cbxAudioFormat) then
    cbxAudioFormat.Enabled := IsAudioOnly;

  // Audio device selection for Audio-only and Video+Audio.
  if Assigned(cbxAudioDevice) then
    cbxAudioDevice.Enabled := IsAudio;

  if Assigned(lblAudio) then
    lblAudio.Enabled := Assigned(cbxAudioDevice) and cbxAudioDevice.Enabled;

  // Monitor selection for Video-only and Video+Audio.
  if Assigned(cbxMonitor) then
    cbxMonitor.Enabled := IsVideo;

  if Assigned(lblMonitor) then
    lblMonitor.Enabled := Assigned(cbxMonitor) and cbxMonitor.Enabled;

  // Resolution / FPS only for video modes.
  if Assigned(cbxResolutions) then
    cbxResolutions.Enabled := IsVideo;

  if Assigned(cbxFrameRate) then
    cbxFrameRate.Enabled := IsVideo;

  if Assigned(lblResolution) then
    lblResolution.Enabled := Assigned(cbxResolutions) and cbxResolutions.Enabled;

  if Assigned(lblFrameRate) then
    lblFrameRate.Enabled := Assigned(cbxFrameRate) and cbxFrameRate.Enabled;

  // ---------------------------------------------------------------------------
  // Start/Stop availability (reflect state; do not enforce beyond UI)
  // ---------------------------------------------------------------------------
  NeedsMonitor := not IsAudioOnly;
  CanStart := (not FIsRecording) and HasOutput;

  if NeedsMonitor then
    CanStart := CanStart and
                (Assigned(cbxMonitor) and
                (cbxMonitor.ItemIndex >= 0));

  // Soft warning via hint if extension does not match expected container
  if (not IsAudioOnly) and
     (OutExt <> '') and
     (OutExt <> '.mp4') then
    edtOutput.Hint := edtOutput.Hint + sLineBreak + 'Note: Video capture expects .mp4 output.'
  else
    if IsAudioOnly and
       (OutExt <> '') and
       (OutExt <> '.wav') and
       (OutExt <> '.flac') then
      edtOutput.Hint := edtOutput.Hint + sLineBreak + 'Note: Audio-only expects .wav or .flac output.';

  butStart.Enabled := CanStart;
  butStop.Enabled := FIsRecording;
  butPlayOutput.Enabled := not FIsRecording and not CanStart;
end;


procedure TfrmCapture.GetRenderSettings();
begin

  // Preview window size = 0
  // 720p (1280 x 720) = 1
  // Full HD (1920 x 1080) = 2
  // 1080p+ or WUXGA (Widescreen Ultra Extended Graphics Array) (1920 x 1200)  
  // 2K (2560 x 1440) = 3
  // 4K (3840 x 2160) = 4

  case cbxResolutions.ItemIndex of
    0:  begin
          FCaptureWidth := pnlPreview.Width;
          FCaptureHeigth := pnlPreview.Height;
        end;

    1:  begin
          FCaptureWidth := 1280;
          FCaptureHeigth := 720;
        end;

    2:  begin
          FCaptureWidth := 1920;
          FCaptureHeigth := 1080;
        end;

    3:  begin
          FCaptureWidth := 1920;
          FCaptureHeigth := 1200;
        end;

    4:  begin
          FCaptureWidth := 2560;
          FCaptureHeigth := 1440;
        end;

    5:  begin
          FCaptureWidth := 3840;
          FCaptureHeigth := 2160;
        end;
  end;

  case cbxFrameRate.ItemIndex of
    0:  FFrameRate := 30;

    1:  FFrameRate := 60;
  end;
end;


function TfrmCapture.SelectedAacAvgBytesPerSec: Cardinal;
begin
  // SinkWriter AAC target in BYTES/sec (approx). 20000 ~= 160 kbps.
  case cbxAudioBitrate.ItemIndex of
    0: Result := 12000; // 96 kbps
    1: Result := 16000; // 128 kbps
    2: Result := 20000; // 160 kbps
    3: Result := 24000; // 192 kbps
    4: Result := 32000; // 256 kbps
  else
    Result := 20000;
  end;
end;


procedure TfrmCapture.CaptureProgress(Sender: TObject;
                                      FrameIndex: Int64;
                                      Msec: Double);
const
  // Window size for FPS smoothing. UI-only; does not affect capture.
  FPS_AVG_WINDOW = 60;

var
  InstFps: Double;
  W: Double;

begin

  FLastFrameTime := Msec;

  // Compute instantaneous FPS. Note: Msec is in seconds (see log: Msec * 1000).
  if (Msec > 0) then
    InstFps := 1 / Msec
  else
    InstFps := 0.0;

  // Smooth it into an average (simple sliding/EMA hybrid).
  if FFpsAvgCount < FPS_AVG_WINDOW then
    Inc(FFpsAvgCount);

  if FFpsAvgCount > 0 then
    W := 1.0 / FFpsAvgCount
  else
    W := 1.0;

  // After the window is filled, keep a constant smoothing factor.
  if FFpsAvgCount >= FPS_AVG_WINDOW then
    W := 1.0 / FPS_AVG_WINDOW;

  FFpsAvg := FFpsAvg + (InstFps - FFpsAvg) * W;

  TThread.Queue(nil,
                procedure
                  begin
                    // Show averaged FPS to avoid jitter.
                    lblFPS.Caption := Format('FPS: %.1f', [FFpsAvg]);

                    // Keep the instantaneous value available as a tooltip.
                    lblFPS.ShowHint := True;
                    lblFPS.Hint := Format('Instant: %.1f FPS, Avg(%.0d): %.1f FPS',
                                          [InstFps, FPS_AVG_WINDOW, FFpsAvg]);

                    if ((FrameIndex mod 30) = 0) then
                      mmoLog.Lines.Add(Format('Frame %d Elapsed time: %.3f ms',
                                       [FrameIndex, Msec * 1000]));
                  end);
end;


procedure TfrmCapture.cbxAudioCodecChange(Sender: TObject);
begin

  case cbxAudioCodec.ItemIndex of
    0: FAudioCodec := acAac;
    1: FAudioCodec := acFlac;
    else
      FAudioCodec := acAac;
  end;

  ApplyUiGuardrails();
  UpdateUiIndicators();
end;


procedure TfrmCapture.cbxAudioCodecCloseUp(Sender: TObject);
begin

  CheckChecks();
end;


procedure TfrmCapture.cbxHotKeysClick(Sender: TObject);
begin

  ApplyHotkeyOption();
end;


procedure TfrmCapture.CaptureError(Sender: TObject;
                                   const Msg: string);
begin

  TThread.Queue(nil,
                procedure
                  begin
                    mmoLog.Lines.Add('ERROR: ' + Msg);
                    lblStatus.Caption := 'Error';
                    lblStatus.Font.Color := clRed;
                   end);
end;


// HOTKEYS ---------------------------------------------------------------------

procedure TfrmCapture.EnableGlobalHotkeys;
var
  ok1,
  ok2,
  ok3: BOOL;

begin

  if FHotkeysActive then
    Exit;

  ok1 := RegisterHotKey(Handle,
                        HOTKEY_START,
                        MOD_NOREPEAT,
                        VK_F9);

  ok2 := RegisterHotKey(Handle,
                        HOTKEY_STOP,
                        MOD_NOREPEAT,
                        VK_F10);

  // Optional: toggle UI hotkey (F10)
  ok3 := RegisterHotKey(Handle,
                        HOTKEY_TOGGLE_UI,
                         MOD_NOREPEAT,
                         VK_F11);

  FHotkeysActive := ok1 and ok2 and ok3;

  if not FHotkeysActive then
    begin

      UnregisterHotKey(Handle,
                       HOTKEY_START);

      UnregisterHotKey(Handle,
                       HOTKEY_STOP);

      UnregisterHotKey(Handle,
                       HOTKEY_TOGGLE_UI);

      ShowMessage('Hotkeys could not be registered (already in use?)');
  end;
end;


procedure TfrmCapture.DisableGlobalHotkeys();
begin

  if not FHotkeysActive then
    Exit;

  UnregisterHotKey(Handle,
                   HOTKEY_START);
  UnregisterHotKey(Handle,
                   HOTKEY_STOP);

  UnregisterHotKey(Handle,
                   HOTKEY_TOGGLE_UI);

  FHotkeysActive := False;
end;


// Call this whenever the checkbox changes and during FormCreate.
procedure TfrmCapture.ApplyHotkeyOption;
begin

  if Assigned(cbxHotKeys) and cbxHotKeys.Checked then
    EnableGlobalHotkeys()
  else
    DisableGlobalHotkeys();
end;


procedure TfrmCapture.HideUI();
begin

  // Keep capture running; only hide window
  Application.Minimize;
  Hide();
end;


procedure TfrmCapture.ShowUI();
begin

  Show();
  Application.Restore();
  frmCapture.WindowState := wsNormal;
  Application.BringToFront;
end;


procedure TfrmCapture.ToggleUI();
begin

  if Visible or (frmCapture.WindowState = wsNormal) then
    HideUI()
  else
    begin
      ShowUI();
    end;
end;


// Message handler.
procedure TfrmCapture.WMHotKey(var Msg: TMessage);
begin

  inherited;

  case Msg.WParam of
    HOTKEY_START: if butStart.Enabled then
                    begin
                      butStartClick(nil);
                      Beep;
                    end;

    HOTKEY_STOP: if butStop.Enabled then
                   begin
                     butStopClick(nil);
                     Beep;
                     Sleep(2000);
                     Beep;
                   end;

    HOTKEY_TOGGLE_UI: ToggleUI();
  end;
end;

end.
