// FactoryX
//
// Copyright: FactoryX. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: frmSetup.pas
// Kind: Pascal Unit
// Release date: 24-01-2026
// Language: ENU
//
// Revision Version: 4.0.0
// Description: RDJ Setup form GUI, settings for audio, IceCast and Caddy.
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
unit frmSetup;

interface

uses

  {WinApi}
  Winapi.Windows,
  Winapi.WinApiTypes,
  Winapi.ActiveX,
  {System}
  System.SysUtils,
  System.Classes,
  System.Types,
  System.UITypes,
  System.IOUtils,
  {Vcl}
  Vcl.Forms,
  Vcl.Controls,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.ComCtrls,
  Vcl.Dialogs,
  Vcl.Graphics,
  {CoreAudioApi}
  WinApi.ActiveX.PropSys,
  WinApi.ActiveX.PropIdl,
  WinApi.CoreAudioApi.FunctionDiscoveryKeys_devpkey,
  WinApi.CoreAudioApi.MMDeviceApi,
  WinApi.CoreAudioApi.MMDevApiUtils,
  {Application}
  RDJ_Common,
  RDJ.Setup,
  MPxpButton,
  MfTrackBar;

type

  TEndpointItem = class
  public

    DeviceId: string; // IMMDevice.GetId() string
    State: DWORD;     // DEVICE_STATE_*
  end;

  TfrmSetup = class(TForm)
    btnOk: TMPxpButton;
    btnCancel: TMPxpButton;
    pnlAudioEndPoints: TPanel;
    lblAudioBufSize: TLabel;
    Label4: TLabel;
    lblChannels: TLabel;
    lblBuffSize: TLabel;
    cbChannels: TComboBox;
    tbAudioBufferDuration: TMfTrackBar;
    Bevel2: TBevel;
    Label1: TLabel;
    Bevel3: TBevel;
    Label2: TLabel;
    Label3: TLabel;
    tbRecCapBufferSize: TMfTrackBar;
    lblAudioRecBufSize: TLabel;
    Label6: TLabel;
    tbSysLatency: TMfTrackBar;
    lblSysLatency: TLabel;
    chkDontOverWrite: TMPxpButton;
    chkDisableMMCSS: TMPxpButton;
    chkUsePCMFormat: TMPxpButton;
    chkEnableStreamSwitchDetection: TMPxpButton;
    lblAudioFormat: TLabel;
    cbxOutputFormat: TComboBox;
    Label5: TLabel;
    Bevel5: TBevel;
    edRecordingsDirName: TEdit;
    Label7: TLabel;
    lblBufferDuration: TLabel;
    lblLoopbackDecks: TLabel;
    cbLoopbackDecks: TComboBox;
    Bevel6: TBevel;
    Bevel1: TBevel;
    Bevel4: TBevel;
    lblMainOut: TLabel;
    lblCueOut: TLabel;
    lblHint: TLabel;
    lblAudioRecorder: TLabel;
    cbMainOut: TComboBox;
    cbCueOut: TComboBox;
    chkPfl: TMPxpButton;
    chkGeneralSettings: TMPxpButton;
    chkBroadcastSettings: TMPxpButton;
    pnlBroadCastSettings: TPanel;
    Bevel11: TBevel;
    Label26: TLabel;
    edtBroadcastHost: TEdit;
    lblBroadcastHost: TLabel;
    lblBroadcastPort: TLabel;
    edtBroadcastPort: TEdit;
    lblBroadcastMount: TLabel;
    edtBroadcastMount: TEdit;
    lblBroadcastPassword: TLabel;
    edtBroadcastPassword: TEdit;
    Label12: TLabel;
    lblBroadcastName: TLabel;
    edtBroadcastName: TEdit;
    lblBroadcastDescription: TLabel;
    edtBroadcastDescription: TEdit;
    Label15: TLabel;
    lblBroadcastGenre: TLabel;
    edtBroadcastGenre: TEdit;
    lblBroadcastUrl: TLabel;
    edtBroadcastUrl: TEdit;
    chkBroadcastPublic: TMPxpButton;
    lblBroadCastUserName: TLabel;
    edtBroadcastUsername: TEdit;
    Bevel7: TBevel;
    lblMicIn: TLabel;
    cbMicIn: TComboBox;
    chkMicDeckEnabled: TMPxpButton;
    edtIcecastExePath: TEdit;
    Label8: TLabel;
    Label9: TLabel;
    edtIcecastConfigPath: TEdit;
    Label10: TLabel;
    Bevel8: TBevel;
    edtServerport: TEdit;
    Label11: TLabel;
    edtServerHost: TEdit;
    Label13: TLabel;
    Label14: TLabel;
    edtIcecastHttpPath: TEdit;
    Label16: TLabel;
    edtWorkingDir: TEdit;
    Label17: TLabel;
    edtIcecastRestartDelayMs: TEdit;
    chkIcecastAutoRestart: TMPxpButton;
    Bevel9: TBevel;
    Label18: TLabel;
    lblCaddyPath: TLabel;
    edtCaddyPath: TEdit;
    lblCaddyConfigPath: TLabel;
    edtCaddyConfigPath: TEdit;
    lblCaddyJsonNowPlayingPath: TLabel;
    edtCaddyJsonNowPlayingPath: TEdit;
    lblCaddyCmdLine: TLabel;
    edtCaddyCmdLine: TEdit;
    btnGetIceCastExePath: TMPxpButton;
    btnGetIceCastConfigPath: TMPxpButton;
    btnGetIceCastWorkingDir: TMPxpButton;
    btnGetCaddyPath: TMPxpButton;
    btnGetCaddyConfigPath: TMPxpButton;
    btnGetCaddyJsonPath: TMPxpButton;
    pnlGeneralSettings: TPanel;
    pnlButtons: TPanel;
    edDataBaseDirName: TEdit;
    Label19: TLabel;
    btnRecordingsDirName: TMPxpButton;
    btnDataBaseDirName: TMPxpButton;
    Label20: TLabel;
    edCoversDirName: TEdit;
    btnCoversDirName: TMPxpButton;
    Label21: TLabel;
    edtCaddyCoversPath: TEdit;
    btnCaddyCoversPath: TMPxpButton;

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure chkPflClick(Sender: TObject);
    procedure tbAudioBufferDurationChange(Sender: TObject);
    procedure tbRecCapBufferSizeChange(Sender: TObject);
    procedure tbSysLatencyChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure chkGeneralSettingsClick(Sender: TObject);
    procedure chkBroadcastSettingsClick(Sender: TObject);
    procedure btnGetIceCastExePathClick(Sender: TObject);
    procedure btnGetIceCastConfigPathClick(Sender: TObject);
    procedure btnGetIceCastWorkingDirClick(Sender: TObject);
    procedure btnGetCaddyPathClick(Sender: TObject);
    procedure btnGetCaddyConfigPathClick(Sender: TObject);
    procedure btnGetCaddyJsonPathClick(Sender: TObject);
    procedure btnRecordingsDirNameClick(Sender: TObject);
    procedure btnDataBaseDirNameClick(Sender: TObject);
    procedure btnCoversDirNameClick(Sender: TObject);
    procedure btnCaddyCoversPathClick(Sender: TObject);

  private

    // Called when destoying the form.
    procedure FreeEndpointComboItems(cb: TComboBox);

    procedure PopulateChannels(const Current: Integer);
    procedure PopulateLoopbackDecks(const Current: Integer);

    procedure PopulateEndpoints(cb: TComboBox;
                                const CurrentDeviceId: string;
                                const ADataFlow: EDataFlow);

    function GetSelectedDeviceId(cb: TComboBox): string;
    procedure UpdateCueEnableUi();
    function PickPath(ADirOnly: Boolean;
                      AFilter: PWideChar): string;

    function SetRecorderBufferDuration(ASetCaption: Boolean = False): Integer;

  public

    class function Execute(var ASetup: TRDJSetup): Boolean;

  published

    //pnlGeneralSettings: TPanel;
    //pnlButtons: TPanel;
  end;


implementation

{$R *.dfm}

uses
  System.Math;


function StateToTag(const S: DWORD): string;
begin

  case S of
    DEVICE_STATE_ACTIVE: Result := '[Active]';
    DEVICE_STATE_DISABLED: Result := '[Disabled]';
    DEVICE_STATE_NOTPRESENT: Result := '[Not present]';
    DEVICE_STATE_UNPLUGGED: Result := '[Unplugged]';
  else
    Result := Format('[State 0x%x]', [S]);
  end;
end;


function GetFriendlyName(const Dev: IMMDevice): string;
var
  Store: IPropertyStore;
  pv: PROPVARIANT;
  hr: HRESULT;

begin

  Result := '';
  Store := nil;
  FillChar(pv, SizeOf(pv), 0);

  hr := Dev.OpenPropertyStore(STGM_READ,
                              Store);
  if FAILED(hr) or (Store = nil) then
    Exit;

  PropVariantInit(pv);
  try

    hr := Store.GetValue(PKEY_Device_FriendlyName,
                         pv);
    if SUCCEEDED(hr) and (pv.vt = VT_LPWSTR) and (pv.pwszVal <> nil) then
      Result := pv.pwszVal;
  finally

    PropVariantClear(pv);
  end;
end;


function GetDeviceIdStr(const Dev: IMMDevice): string;
var
  pId: PWideChar;
  hr: HRESULT;

begin

  Result := '';
  pId := nil;
  hr := Dev.GetId(pId);

  if FAILED(hr) then
    Exit;

  try

    Result := pId;
  finally

    CoTaskMemFree(pId);
  end;
end;


function GetDeviceState(const Dev: IMMDevice): DWORD;
var
  st: DWORD;

begin

  st := 0;
  if Assigned(Dev) and Succeeded(Dev.GetState(st)) then
    Result := st
  else
    Result := 0;
end;

{ TfrmSetup }

class function TfrmSetup.Execute(var ASetup: TRDJSetup): Boolean;
var
  frm: TfrmSetup;

begin

  frm := TfrmSetup.Create(nil);

  try

    frm.tbAudioBufferDuration.Position := ASetup.AudioBufferMs;
    frm.PopulateChannels(ASetup.ChannelCount);
    frm.PopulateLoopbackDecks(ASetup.LoopbackDeckCount);

    frm.PopulateEndpoints(frm.cbMainOut,
                          ASetup.MasterDeviceId,
                          eRender);

    frm.chkPfl.Checked := ASetup.PFLEnabled;

    frm.PopulateEndpoints(frm.cbCueOut,
                          ASetup.PFLDeviceId,
                          eRender);

    frm.PopulateEndpoints(frm.cbMicIn,
                          ASetup.MicDeviceId,
                          eCapture);

    frm.UpdateCueEnableUi();

    // Microphone
    frm.cbMicIn.ItemIndex := ASetup.MicDeviceCbItemIndex;
    frm.chkMicDeckEnabled.Checked := ASetup.MicDeckEnabled;

    // Audio recorder
    frm.tbRecCapBufferSize.Position := ASetup.AudioRecorderCaptureBufferSize;
    frm.tbRecCapBufferSize.Tag := Ord(ASetup.AudioRecorderAutoBufferSize);
    frm.tbSysLatency.Position := ASetup.AudioRecorderSystemLatency;
    frm.chkDontOverWrite.Checked := ASetup.AudioRecorderDontOverWriteAudioFiles;
    frm.chkUsePCMFormat.Checked := ASetup.AudioRecorderUsePCMFormat;
    frm.chkDisableMMCSS.Checked := ASetup.AudioRecorderDisableMMCSS;
    frm.chkEnableStreamSwitchDetection.Checked := ASetup.AudioRecorderEnableStreamSwitchDetection;
    frm.cbxOutputFormat.ItemIndex := ASetup.AudioRecorderAudioFormat;

    // Icecast
    frm.edtBroadcastHost.Text := ASetup.Broadcast.Host;
    frm.edtBroadcastPort.Text := IntToStr(ASetup.Broadcast.Port);
    frm.edtBroadcastMount.Text := ASetup.Broadcast.Mount;
    frm.edtBroadcastUserName.Text := ASetup.Broadcast.Username;
    frm.edtBroadcastPassword.Text := ASetup.Broadcast.Password;
    frm.edtBroadcastName.Text := ASetup.Broadcast.StreamName;
    frm.edtBroadcastDescription.Text := ASetup.Broadcast.Description;
    frm.edtBroadcastGenre.Text := ASetup.Broadcast.Genre;
    frm.edtBroadcastUrl.Text := ASetup.Broadcast.Url;
    frm.chkBroadcastPublic.Checked := ASetup.Broadcast.PublicStream;

    // IceCast Server Settings
    frm.edtServerHost.Text := ASetup.IcecastHost;
    frm.edtServerport.Text := IntToStr(ASetup.IcecastPort);
    frm.edtIcecastExePath.Text := ASetup.IcecastExePath;
    frm.edtIcecastConfigPath.Text := ASetup.IcecastConfigPath;
    frm.edtIcecastHttpPath.Text := ASetup.IcecastHttpPath;
    frm.edtWorkingDir.Text := ASetup.IcecastWorkingDir;
    frm.edtIcecastRestartDelayMs.Text := IntToStr(ASetup.IcecastRestartDelayMs);
    frm.chkIcecastAutoRestart.Checked := ASetup.IcecastAutoRestart;

    // Caddy
    frm.edtCaddyPath.Text := ASetup.IcecastCaddyDir;
    frm.edtCaddyConfigPath.Text := ASetup.IcecastCaddyConfigFile;
    frm.edtCaddyJsonNowPlayingPath.Text := ASetup.IcecastNowPlayingJsonFile;
    frm.edtCaddyCoversPath.Text := ASetup.IcecastCaddyCoversPath;
    frm.edtCaddyCmdLine.Text := ASetup.IcecastCaddyCommand;

    // Save audiorecordings path
    frm.edRecordingsDirName.Text := ASetup.AudioRecordingsDir;
    frm.edRecordingsDirName.Hint := ASetup.AudioRecordingsPath;
    // Save Database path
    frm.edDataBaseDirName.Text := ASetup.DatabaseDir;
    frm.edDataBaseDirName.Hint := ASetup.DatabasePath;
    // Save Local covers path
    frm.edCoversDirName.Text := ASetup.LocalCoversDir;
    frm.edCoversDirName.Hint := ASetup.LocalCoversPath;

    Result := (frm.ShowModal = mrOk);

    if Result then
      begin

        ASetup.AudioBufferMs := frm.tbAudioBufferDuration.Position;
        ASetup.ChannelCount := frm.cbChannels.ItemIndex + 1;
        ASetup.LoopbackDeckCount := frm.cbLoopbackDecks.ItemIndex;
        ASetup.MasterDeviceId := frm.GetSelectedDeviceId(frm.cbMainOut);
        ASetup.MicDeviceId := frm.GetSelectedDeviceId(frm.cbMicIn);
        ASetup.MicDeviceCbItemIndex := frm.cbMicIn.ItemIndex;
        ASetup.MicDeckEnabled := frm.chkMicDeckEnabled.Checked;
        ASetup.PFLEnabled := frm.chkPfl.Checked;

        if ASetup.PFLEnabled then
          ASetup.PFLDeviceId := frm.GetSelectedDeviceId(frm.cbCueOut)
        else
          ASetup.PFLDeviceId := '';

        ASetup.AudioRecorderCaptureBufferSize := frm.tbRecCapBufferSize.Position;
        ASetup.AudioRecorderAutoBufferSize := LongBool(frm.tbRecCapBufferSize.Tag);
        ASetup.AudioRecorderSystemLatency := frm.tbSysLatency.Position;
        ASetup.AudioRecorderDontOverWriteAudioFiles := frm.chkDontOverWrite.Checked;
        ASetup.AudioRecorderUsePCMFormat := frm.chkUsePCMFormat.Checked;
        ASetup.AudioRecorderDisableMMCSS := frm.chkDisableMMCSS.Checked;
        ASetup.AudioRecorderEnableStreamSwitchDetection := frm.chkEnableStreamSwitchDetection.Checked;
        ASetup.AudioRecorderAudioFormat := frm.cbxOutputFormat.ItemIndex;

        // Icecast -------------------------------------------------------------
        ASetup.Broadcast.Host := Trim(frm.edtBroadcastHost.Text);
        ASetup.Broadcast.Port := StrToIntDef(Trim(frm.edtBroadcastPort.Text),
                                                  8000);
        ASetup.Broadcast.Mount := Trim(frm.edtBroadcastMount.Text);
        ASetup.Broadcast.Username := Trim(frm.edtBroadcastUserName.Text);
        ASetup.Broadcast.Password := frm.edtBroadcastPassword.Text;
        ASetup.Broadcast.StreamName := Trim(frm.edtBroadcastName.Text);
        ASetup.Broadcast.Description := Trim(frm.edtBroadcastDescription.Text);
        ASetup.Broadcast.Genre := Trim(frm.edtBroadcastGenre.Text);
        ASetup.Broadcast.Url := Trim(frm.edtBroadcastUrl.Text);
        ASetup.Broadcast.PublicStream := frm.chkBroadcastPublic.Checked;

        // IceCast server settings
        ASetup.IcecastHost := Trim(frm.edtServerHost.Text);
        ASetup.IcecastPort := WORD(StrToInt(frm.edtServerport.Text));
        ASetup.IcecastExePath := Trim(frm.edtIcecastExePath.Text);
        ASetup.IcecastConfigPath := Trim(frm.edtIcecastConfigPath.Text);
        ASetup.IcecastHttpPath := Trim(frm.edtIcecastHttpPath.Text);
        ASetup.IcecastWorkingDir := Trim(frm.edtWorkingDir.Text);
        ASetup.IcecastRestartDelayMs := Cardinal(StrToInt(frm.edtIcecastRestartDelayMs.Text));
        ASetup.IcecastAutoRestart := frm.chkIcecastAutoRestart.Checked;
        // IceCast/Caddy/json settings
        ASetup.IcecastCaddyDir := frm.edtCaddyPath.Text;
        ASetup.IcecastCaddyConfigFile := frm.edtCaddyConfigPath.Text;
        ASetup.IcecastNowPlayingJsonFile := frm.edtCaddyJsonNowPlayingPath.Text;
        ASetup.IcecastCaddyCoversPath := frm.edtCaddyCoversPath.Text;
        ASetup.IcecastCaddyCommand := frm.edtCaddyCmdLine.Text;

        // Keep current engine defaults for now.
        ASetup.Broadcast.Codec := bcAac;
        ASetup.Broadcast.BitrateKbps := 128;
        ASetup.Broadcast.SampleRate := 44100;
        ASetup.Broadcast.Channels := 2;
        ASetup.Broadcast.TapPoint := btpPreMasterFx; // or  btpPostMasterFx, what's not a very good idea, because post is ment for the PA speakers.
        ASetup.Broadcast.AutoReconnect := True;
        ASetup.Broadcast.BroadcastGainDb := 0.0;

        if (ASetup.Broadcast.Mount = '') then
          ASetup.Broadcast.Mount := '/live'
        else
          if (ASetup.Broadcast.Mount[1] <> '/') then
            ASetup.Broadcast.Mount := '/' + ASetup.Broadcast.Mount;
        // Icecast end ---------------------------------------------------------

        // Audio recording
        ASetup.AudioRecordingsDir := frm.edRecordingsDirName.Text;
        ASetup.AudioRecordingsPath := frm.edRecordingsDirName.Hint;
        // Database
        ASetup.DatabaseDir := frm.edDataBaseDirName.Text;
        ASetup.DatabasePath := frm.edDataBaseDirName.Hint;
        // Covers
        ASetup.LocalCoversDir := frm.edCoversDirName.Text;
        ASetup.LocalCoversPath := frm.edCoversDirName.Hint;
      end;
  finally

    FreeAndNil(frm);
  end;
end;


procedure TfrmSetup.PopulateChannels(const Current: Integer);
var
  i: Integer;
  cur: Integer;

begin

  cbChannels.Items.BeginUpdate;
  try

    cbChannels.Items.Clear;

    for i := 1 to MAX_CHANNELS do
      cbChannels.Items.Add(IntToStr(i));

    cur := EnsureRange(Current,
                       1,
                       MAX_CHANNELS);
      cbChannels.ItemIndex := cur - 1;
  finally

    cbChannels.Items.EndUpdate;
  end;
end;


procedure TfrmSetup.PopulateLoopbackDecks(const Current: Integer);
var
  i: Integer;
  cur: Integer;

begin

  cbLoopbackDecks.Items.BeginUpdate;

  try

    cbLoopbackDecks.Items.Clear;

    for i := 0 to MAX_LOOPBACK_DECKS do
      cbLoopbackDecks.Items.Add(IntToStr(i));

    cur := EnsureRange(Current,
                       0,
                       MAX_LOOPBACK_DECKS);

    cbLoopbackDecks.ItemIndex := cur;
  finally

    cbLoopbackDecks.Items.EndUpdate;
  end;
end;


procedure TfrmSetup.PopulateEndpoints(cb: TComboBox;
                                      const CurrentDeviceId: string;
                                      const ADataFlow: EDataFlow);
var
  Enum: IMMDeviceEnumerator;
  Coll: IMMDeviceCollection;
  Dev: IMMDevice;
  Count: UINT;
  i: UINT;
  hr: HRESULT;
  Item: TEndpointItem;
  Name,
  IdStr,
  Text: string;
  St: DWORD;
  selIdx: Integer;

begin

  cb.Items.BeginUpdate;

  try

    // Free old objects
    FreeEndpointComboItems(cb);

    hr := CoCreateInstance(CLSID_MMDeviceEnumerator,
                           nil,
                           CLSCTX_INPROC_SERVER,
                           IID_IMMDeviceEnumerator,
                           Enum);
    if FAILED(hr) then
      Exit;

    // Show all states (active/disabled/unplugged/notpresent)
    hr := Enum.EnumAudioEndpoints(ADataFlow,
                                  DEVICE_STATE_ACTIVE or
                                  DEVICE_STATE_DISABLED or
                                  DEVICE_STATE_UNPLUGGED,
                                  Coll);
    if FAILED(hr) then
      Exit;

    hr := Coll.GetCount(Count);
    if FAILED(hr) then
      Exit;

    selIdx := -1;

    for i := 0 to Count - 1 do
      begin

        Dev := nil;
        hr := Coll.Item(i,
                        Dev);
        if FAILED(hr) or (Dev = nil) then
          Continue;

        IdStr := GetDeviceIdStr(Dev);
        St := GetDeviceState(Dev);
        Name := GetFriendlyName(Dev);

        if (Name = '') then
          Name := '(Unknown device)';

        Text := Format('%s  %s',
                       [Name, StateToTag(St)]);

        Item := TEndpointItem.Create;
        Item.DeviceId := IdStr;
        Item.State := St;

        cb.Items.AddObject(Text, Item);

        if (CurrentDeviceId <> '') and SameText(CurrentDeviceId,
                                                IdStr) then
          selIdx := cb.Items.Count - 1;
      end;

    // Default selection: current match, else first active, else first item
    if (selIdx < 0) then
      begin

        for selIdx := 0 to cb.Items.Count - 1 do
          begin

            Item := TEndpointItem(cb.Items.Objects[selIdx]);
            if (Item <> nil) and (Item.State = DEVICE_STATE_ACTIVE) then
              Break;
          end;
        if (selIdx >= cb.Items.Count) then
          selIdx := 0;
      end;

    if (cb.Items.Count > 0) then
      cb.ItemIndex := selIdx;
  finally

    cb.Items.EndUpdate;
  end;
end;


procedure TfrmSetup.tbAudioBufferDurationChange(Sender: TObject);
begin

  lblBuffSize.Caption := Format('%d ms',
                                [tbAudioBufferDuration.Position]);
end;


procedure TfrmSetup.tbRecCapBufferSizeChange(Sender: TObject);
begin

  lblAudioRecBufSize.Caption := Format('%d ms',
                                       [tbRecCapBufferSize.Position]);
  SetRecorderBufferDuration(True);

  // Autosize buffer?
  tbRecCapBufferSize.Tag := Ord(tbRecCapBufferSize.Position <= tbRecCapBufferSize.Minimum);
end;


procedure TfrmSetup.tbSysLatencyChange(Sender: TObject);
begin

  lblSysLatency.Caption := Format('%d ms',
                                  [tbSysLatency.Position]);
end;


function TfrmSetup.GetSelectedDeviceId(cb: TComboBox): string;
var
  Item: TEndpointItem;

begin

  Result := '';
  if (cb.ItemIndex < 0) or (cb.ItemIndex >= cb.Items.Count) then
    Exit;
  Item := TEndpointItem(cb.Items.Objects[cb.ItemIndex]);

  if (Item <> nil) then
    Result := Item.DeviceId;
end;


procedure TfrmSetup.btnCaddyCoversPathClick(Sender: TObject);
begin

  edtCaddyCoversPath.Text := PickPath(True,
                                      'Covers Directory'#0'*.jpg'#0#0);
end;


procedure TfrmSetup.btnRecordingsDirNameClick(Sender: TObject);
begin

  edRecordingsDirName.Text :=  ExtractFileName(ExtractFileDir(PickPath(False,
                                                                       'Recordings Directory'#0'*.*'#0#0)));
end;


procedure TfrmSetup.btnCoversDirNameClick(Sender: TObject);
begin

  edCoversDirName.Text :=  ExtractFileName(ExtractFileDir(PickPath(False,
                                                                     'Covers Directory'#0'*.*'#0#0)));
end;


procedure TfrmSetup.btnDataBaseDirNameClick(Sender: TObject);
begin

  edDataBaseDirName.Text :=  ExtractFileName(ExtractFileDir(PickPath(False,
                                                                     'Database Directory'#0'RDJLibrary.db'#0#0)));
end;


procedure TfrmSetup.btnCancelClick(Sender: TObject);
begin

  ModalResult := mrCancel;
end;


procedure TfrmSetup.UpdateCueEnableUi();
begin

  cbCueOut.Enabled := chkPfl.Checked;
  lblCueOut.Enabled := chkPfl.Checked;
end;


function TfrmSetup.PickPath(ADirOnly: Boolean;
                            AFilter: PWideChar): string;
var
  filename: TFileName;

begin

  if BrowseFile(Handle,
                AFilter,
                ADirOnly,
                filename) then
    Result := filename
  else
    Result := '';
end;


procedure TfrmSetup.btnGetCaddyConfigPathClick(Sender: TObject);
begin

  edtCaddyConfigPath.Text := PickPath(False,
                                      'Caddy Config File'#0'caddy.cff'#0#0);
end;


procedure TfrmSetup.btnGetCaddyJsonPathClick(Sender: TObject);
begin

  edtCaddyJsonNowPlayingPath.Text := PickPath(False,
                                              'Caddy Json NowPlaying File'#0'nowplaying.json'#0#0);
end;


procedure TfrmSetup.btnGetCaddyPathClick(Sender: TObject);
begin

  edtCaddyPath.Text := PickPath(True,
                                'Caddy Root Directory'#0'Caddy.exe'#0#0);
end;


procedure TfrmSetup.btnGetIceCastConfigPathClick(Sender: TObject);
begin

  edtIcecastConfigPath.Text := PickPath(False,
                                        'IceCast Config File'#0'icecast.xml'#0#0);
end;

procedure TfrmSetup.btnGetIceCastExePathClick(Sender: TObject);
begin

  edtIcecastExePath.Text := PickPath(False,
                                     'IceCast Executable'#0'IceCast.exe'#0#0);
end;


procedure TfrmSetup.btnGetIceCastWorkingDirClick(Sender: TObject);
begin

  edtIcecastExePath.Text := PickPath(True,
                                     'IceCast Root Directory'#0'IceCast.exe'#0#0);
end;




procedure TfrmSetup.btnOkClick(Sender: TObject);
begin

  // Quick validation: If PFL enabled, require a selection.
  if chkPfl.Checked and (GetSelectedDeviceId(cbCueOut) = '') then
    begin

      MessageDlg('Please select a cue/headphones output device.',
                 mtWarning,
                 [mbOK],
                 0);
      ModalResult := mrNone;
      Exit;
    end;
  ModalResult := mrOk;
end;


procedure TfrmSetup.chkBroadcastSettingsClick(Sender: TObject);
begin

  pnlBroadCastSettings.BringToFront;
  chkBroadcastSettings.Checked := True;
  chkGeneralSettings.Checked := False;
end;


procedure TfrmSetup.chkGeneralSettingsClick(Sender: TObject);
begin

  pnlGeneralSettings.BringToFront;
  chkBroadcastSettings.Checked := False;
  chkGeneralSettings.Checked := True;
end;


procedure TfrmSetup.chkPflClick(Sender: TObject);
begin

  UpdateCueEnableUi();
end;


procedure TfrmSetup.FormCreate(Sender: TObject);
begin

  UpdateCueEnableUi();
end;


procedure TfrmSetup.FormDestroy(Sender: TObject);
begin

  // Free objects stored in combo items.
  FreeEndpointComboItems(cbMainOut);
  FreeEndpointComboItems(cbCueOut);
  FreeEndpointComboItems(cbMicIn);
end;


// To prevent memory leaks.
procedure TfrmSetup.FreeEndpointComboItems(cb: TComboBox);
var
  i: Integer;

begin

  if not Assigned(cb) then
    Exit;

  for i := 0 to cb.Items.Count - 1 do
    begin
      cb.Items.Objects[i].Free;
      cb.Items.Objects[i] := nil;
    end;

  cb.Items.Clear;
end;


procedure TfrmSetup.FormShow(Sender: TObject);
begin

  //ApplyDarkWindowFrame(Handle);
end;


function TfrmSetup.SetRecorderBufferDuration(ASetCaption: Boolean = False): Integer;
var
  bufsize: Integer;

begin

  bufsize := tbRecCapBufferSize.Position;

  if (bufsize <= 10) then
    Result := 0
  else
    Result := (REFTIMES_PER_MILLISEC) * bufsize;

  if ASetCaption then
    if (bufsize <= 30) then
      lblBufferDuration.Caption := 'The audioclient will automaticly adjust the buffer duration.'
    else
      lblBufferDuration.Caption := Format('Capture buffer duration: %d milliseconds.',
                                          [bufsize])
end;

end.
