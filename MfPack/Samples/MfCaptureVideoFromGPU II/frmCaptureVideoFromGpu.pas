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
// Revision Version: 3.1.8
// Description: GUI for MfCaptureVideoFromGPU version 2
//
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 24/11/2025 Tony                Ozzy Osbourne release  SDK 10.0.26100.4654 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: -
//
// Related objects: -
// Related projects: MfPackX318/Samples/MfCaptureVideoFromGPU II
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
  {Winapi}
  Winapi.Windows,
  Winapi.Messages,
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

  OUTPUT_FILENAME: string = 'capture_output.mp4';

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
    btnStart: TButton;
    btnStop: TButton;
    lblStatus: TLabel;
    Bevel3: TBevel;
    lblAudioBitrate: TLabel;
    cbxAudioBitrate: TComboBox;
    lblAudioCodec: TLabel;
    cbxAudioCodec: TComboBox;
    cbxAudioFormat: TComboBox;

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);

    procedure btnStartClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure btnBrowseClick(Sender: TObject);
    procedure rbRecVideoAndAudioClick(Sender: TObject);
    procedure rbRecVideoClick(Sender: TObject);
    procedure rbRecAudioClick(Sender: TObject);
    procedure cbxAudioCodecChange(Sender: TObject);
    procedure cbxAudioFormatChange(Sender: TObject);

  private

    FEngine: TCaptureStreamEngine;
    FOutputs: TArray<TDXGIOutputInfo>;
    FLastFrameTime: Double;
    FAudioDeviceIds: TStringList;
    FCaptureMode: TCaptureMode;
    FAudioCodec: TAudioCodec;
    FAudioFileFormat: TAudioFileFormat;
    FAudioOnly: TLoopbackAudioOnlyRecorder;
    FActivityPinger: TScreenActivityPinger;

    // Resolution and frame rate.
    FCaptureWidth,
    FCaptureHeigth: UINT;
    FFrameRate: UINT32;

    procedure CheckChecks();
    procedure InitMonitors();
    procedure InitAudioDevices();

    function SelectedAacAvgBytesPerSec: Cardinal;

    procedure CaptureProgress(Sender: TObject;
                              FrameIndex: Int64;
                              Msec: Double);

    procedure CaptureError(Sender: TObject;
                           const Msg: string);

    function GetSelectedAudioDeviceId(): string;

    procedure GetRenderSettings();
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
end;


procedure TfrmCapture.FormDestroy(Sender: TObject);
begin

  FreeAndNil(FAudioDeviceIds);
  FreeAndNil(FEngine);
end;


procedure TfrmCapture.btnBrowseClick(Sender: TObject);
var
  dlg: TSaveDialog;

begin

  dlg := TSaveDialog.Create(Self);

  try

    dlg.Filter := 'MP4 Files|*.mp4';
    dlg.DefaultExt := 'mp4';
    dlg.FileName := edtOutput.Text;

    if dlg.Execute then
      edtOutput.Text := dlg.FileName;
  finally
    dlg.Free;
  end;
end;


procedure TfrmCapture.btnStartClick(Sender: TObject);
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

  btnStart.Enabled := False;
  btnStop.Enabled := True;

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

    // Start recorder //////////////////////////////////////////////////////////

    FAudioOnly.StartToFile(sFileName,
                           FAudioFileFormat,
                           0,
                           FEngine.AudioDeviceID);


    ////////////////////////////////////////////////////////////////////////////

    lblStatus.Caption := 'Recording (audio only)...';
    lblStatus.Font.Color := clLime;

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
  rOutputRect := FEngine.GetSelectedOutputRect;

  FreeAndNil(FActivityPinger);
  FActivityPinger := TScreenActivityPinger.Create(rOutputRect,
                                                  FFrameRate,
                                                  4, // pixels, width
                                                  4, // pixels, height
                                                  clLime); // pixel color

  FActivityPinger.Start();
  lblStatus.Caption := 'Recording...';
  lblStatus.Font.Color := clLime;

  mmoLog.Lines.Add('--- Capture Started ---');
  mmoLog.Lines.Add('Output file: ' + sFileName);
end;


procedure TfrmCapture.btnStopClick(Sender: TObject);
begin

  if Assigned(FAudioOnly) then
    FAudioOnly.Stop;

  if Assigned(FEngine) then
    FEngine.StopCapture();

  lblStatus.Caption := 'Idle';
  lblStatus.Font.Color := clGray;

  mmoLog.Lines.Add('--- Capture Stopped ---');

  btnStart.Enabled := True;
  btnStop.Enabled := False;
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


procedure TfrmCapture.CheckChecks();
var
  fn: string;

begin

  if (Length(edtOutput.Text) < 5) then
    begin
      edtOutput.Text := OUTPUT_FILENAME;
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
begin

  FLastFrameTime := Msec;

  TThread.Queue(nil,
                procedure
                  begin
                    if (Msec > 0) then
                      lblFPS.Caption := Format('FPS: %.1f',
                                               [1 / Msec])
                    else
                      lblFPS.Caption := 'FPS: 0.0';

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
end;


procedure TfrmCapture.cbxAudioFormatChange(Sender: TObject);
begin
  case cbxAudioFormat.ItemIndex of
    0: edtOutput.Text := ChangeFileExt(OUTPUT_FILENAME,
                                       '.wav');
    1: edtOutput.Text := ChangeFileExt(OUTPUT_FILENAME,
                                       '.flac');
  end;
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

end.
