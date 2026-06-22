// FactoryX
//
// Copyright: FactoryX. All rights reserved.
//
// Media serverject: Media Foundation - MFPack - Samples
// Media serverject location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: frmSetup.pas
// Kind: Pascal Unit
// Release date: 24-01-2026
// Language: ENU
//
// Revision Version: 3.2.0
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
// 05/05/2026 All                 Bauhaus release  SDK 10.0.26100.4654 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 or higher.
//          Please, read documentation carefully!
//
// Related objects: -
// Related projects: MfPackX320
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.26100.4654
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
  WinApi.Windows,
  WinApi.WinApiTypes,
  WinApi.ActiveX,
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
  {ActiveX}
  WinApi.ActiveX.PropSys,
  WinApi.ActiveX.PropIdl,
  {CoreAudioApi}
  WinApi.CoreAudioApi.FunctionDiscoveryKeys_devpkey,
  WinApi.CoreAudioApi.MMDeviceApi,
  WinApi.CoreAudioApi.MMDevApiUtils,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfApi,
  WinApi.MediaFoundationApi.MfObjects,
  WinApi.MediaFoundationApi.MfIdl,
  WinApi.MediaFoundationApi.MfMetLib,
  {Application}
  RDJ_Common,
  RDJ.Setup,
  RDJ.RdjPro.CaptureEngine,
  RDJ.RdjPro.Compositor,

  MPxpButton,
  MfTrackBar;

type

  TEndpointItem = class
  public

    DeviceId: string; // IMMDevice.GetId() string
    State: DWORD;     // DEVICE_STATE_*
  end;

type

  TCameraDeviceItem = class
  public

    DeviceProperties: TDeviceProperties;

    constructor Create(const ADeviceProperties: TDeviceProperties);
    destructor Destroy(); override;
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
    edAudioRecordingsDirName: TEdit;
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
    Label12: TLabel;
    Label15: TLabel;
    Bevel7: TBevel;
    lblMicIn: TLabel;
    cbMicIn: TComboBox;
    chkMicDeckEnabled: TMPxpButton;
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
    btnGetCaddyPath: TMPxpButton;
    btnGetCaddyConfigPath: TMPxpButton;
    btnGetCaddyJsonPath: TMPxpButton;
    pnlGeneralSettings: TPanel;
    pnlButtons: TPanel;
    edDataBaseDirName: TEdit;
    Label19: TLabel;
    btnAudioRecordingsDirName: TMPxpButton;
    btnDataBaseDirName: TMPxpButton;
    Label20: TLabel;
    edArtworkDirName: TEdit;
    btnArtworkDirName: TMPxpButton;
    Label21: TLabel;
    edtCaddyArtworkPath: TEdit;
    btnCaddyArtworkPath: TMPxpButton;
    Label35: TLabel;
    cbSelectCamera: TComboBox;
    btnRefreshCameras: TMPxpButton;
    Label25: TLabel;
    Bevel10: TBevel;
    Label22: TLabel;
    edtCaddyVideoPath: TEdit;
    btnCaddyVideoPath: TMPxpButton;
    Label24: TLabel;
    edtCaddyContentTypeURL: TEdit;
    Label23: TLabel;
    edVideoRecordingsDirName: TEdit;
    btnVideoRecordingsDirName: TMPxpButton;
    Label27: TLabel;
    tbMp4SegmentSize: TMfTrackBar;
    lblMp4SegmentSize: TLabel;
    Label8: TLabel;
    Bevel8: TBevel;

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
    procedure btnGetCaddyPathClick(Sender: TObject);
    procedure btnGetCaddyConfigPathClick(Sender: TObject);
    procedure btnGetCaddyJsonPathClick(Sender: TObject);
    procedure btnAudioRecordingsDirNameClick(Sender: TObject);
    procedure btnDataBaseDirNameClick(Sender: TObject);
    procedure btnArtworkDirNameClick(Sender: TObject);
    procedure btnCaddyArtworkPathClick(Sender: TObject);
    procedure cbSelectCameraChange(Sender: TObject);
    procedure chkBroadcastSettingsClick(Sender: TObject);
    procedure btnCaddyVideoPathClick(Sender: TObject);
    procedure btnVideoRecordingsDirNameClick(Sender: TObject);
    procedure tbMp4SegmentSizeChange(Sender: TObject);

  private

    FMp4SegmentValueMs: Integer;


    // Called when destoying the form.
    procedure FreeComboBoxObjects(cb: TComboBox);

    procedure PopulateChannels(const Current: Integer);
    procedure PopulateLoopbackDecks(const Current: Integer);

    procedure PopulateEndpoints(cb: TComboBox;
                                const CurrentDeviceId: string;
                                const ADataFlow: EDataFlow);

    procedure PopulateVideoCaptureDevices(cb: TComboBox;
                                          const CurrentSymbolicLink: string);

    function GetSelectedDeviceId(cb: TComboBox): string;
    procedure UpdateCueEnableUi();
    function PickPath(ADirOnly: Boolean;
                      AFilter: PWideChar): string;

    function SetRecorderBufferDuration(ASetCaption: Boolean = False): Integer;

  public

    class function Execute(var ASetup: TRDJSetup): Boolean;

  published


  end;


implementation

{$R *.dfm}

uses
  System.Math;


constructor TCameraDeviceItem.Create(const ADeviceProperties: TDeviceProperties);
begin

  inherited Create();

  DeviceProperties := ADeviceProperties;
end;


destructor TCameraDeviceItem.Destroy();
begin

  DeviceProperties.Reset();

  inherited Destroy();
end;


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


function SnapMs(const AValueMs,
                      AStepMs,
                      AMinMs,
                      AMaxMs: Integer): Integer;
begin

  Result := ((AValueMs + (AStepMs div 2)) div AStepMs) * AStepMs;

  if (Result < AMinMs) then
    Result := AMinMs
  else
    if (Result > AMaxMs) then
      Result := AMaxMs;
end;

{ TfrmSetup }

class function TfrmSetup.Execute(var ASetup: TRDJSetup): Boolean;
var
  frm: TfrmSetup;
  DevItem: TCameraDeviceItem;

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

    frm.PopulateVideoCaptureDevices(frm.cbSelectCamera,
                                    ASetup.CameraSymbolicLink);

    frm.UpdateCueEnableUi();

    // Microphone
    frm.cbMicIn.ItemIndex := ASetup.MicDeviceCbItemIndex;
    frm.chkMicDeckEnabled.Checked := ASetup.MicDeckEnabled;

    // Audio recorder
    frm.tbRecCapBufferSize.Position := ASetup.AudioRecorderCaptureBufferMs;
    frm.tbRecCapBufferSize.Tag := Ord(ASetup.AudioRecorderAutoBufferSize);
    frm.tbSysLatency.Position := ASetup.AudioRecorderSystemLatency;
    frm.chkDontOverWrite.Checked := ASetup.AudioRecorderDontOverWriteAudioFiles;
    frm.chkUsePCMFormat.Checked := ASetup.AudioRecorderUsePCMFormat;
    frm.chkDisableMMCSS.Checked := ASetup.AudioRecorderDisableMMCSS;
    frm.chkEnableStreamSwitchDetection.Checked := ASetup.AudioRecorderEnableStreamSwitchDetection;
    frm.cbxOutputFormat.ItemIndex := ASetup.AudioRecorderAudioFormat;

    // Caddy / json
    frm.edtCaddyPath.Text := ASetup.CaddyDir;
    frm.edtCaddyConfigPath.Text := ASetup.CaddyConfigFile;
    frm.edtCaddyJsonNowPlayingPath.Text := ASetup.CaddyNowPlayingJsonFile;
    frm.edtCaddyArtworkPath.Text := ASetup.CaddyArtworkPath;
    frm.edtCaddyVideoPath.Text := ASetup.CaddyVideoPath;
    frm.edtCaddyContentTypeURL.Text := ASetup.CaddyContentTypeURL;
    frm.edtCaddyCmdLine.Text := ASetup.CaddyCommand;

    // MSE MP4
    frm.tbMp4SegmentSize.Position := ASetup.MsePublicSegmentTargetMs;

    // Save audiorecordings path
    frm.edAudioRecordingsDirName.Text := ASetup.AudioRecordingsDir;
    frm.edAudioRecordingsDirName.Hint := ASetup.AudioRecordingsPath;

    // Save videorecordings path
    frm.edVideoRecordingsDirName.Text := ASetup.VideoRecordingsDir;
    frm.edVideoRecordingsDirName.Hint := ASetup.VideoRecordingsPath;

    // Save Database path
    frm.edDataBaseDirName.Text := ASetup.DatabaseDir;
    frm.edDataBaseDirName.Hint := ASetup.DatabasePath;

    // Save Local artwork path
    frm.edArtworkDirName.Text := ASetup.LocalArtworkDir;
    frm.edArtworkDirName.Hint := ASetup.LocalArtworkPath;

    // Media server
    //frm.chkEnableRdjPro.Checked := ASetup.MediaServerEnabled;
    //frm.edRdjJsonFile.Text := ASetup.MetadataJsonFile;

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

        ASetup.AudioRecorderCaptureBufferMs := frm.tbRecCapBufferSize.Position;
        ASetup.AudioRecorderAutoBufferSize := LongBool(frm.tbRecCapBufferSize.Tag);
        ASetup.AudioRecorderSystemLatency := frm.tbSysLatency.Position;
        ASetup.AudioRecorderDontOverWriteAudioFiles := frm.chkDontOverWrite.Checked;
        ASetup.AudioRecorderUsePCMFormat := frm.chkUsePCMFormat.Checked;
        ASetup.AudioRecorderDisableMMCSS := frm.chkDisableMMCSS.Checked;
        ASetup.AudioRecorderEnableStreamSwitchDetection := frm.chkEnableStreamSwitchDetection.Checked;
        ASetup.AudioRecorderAudioFormat := frm.cbxOutputFormat.ItemIndex;

        // Caddy/json settings
        ASetup.CaddyDir := frm.edtCaddyPath.Text;
        ASetup.CaddyConfigFile := frm.edtCaddyConfigPath.Text;
        ASetup.CaddyNowPlayingJsonFile := frm.edtCaddyJsonNowPlayingPath.Text;
        ASetup.CaddyArtworkPath := frm.edtCaddyArtworkPath.Text;
        ASetup.CaddyVideoPath := frm.edtCaddyVideoPath.Text;
        ASetup.CaddyContentTypeURL := frm.edtCaddyContentTypeURL.Text;
        ASetup.CaddyCommand := frm.edtCaddyCmdLine.Text;

        // MSE MP4
        ASetup.MsePublicSegmentTargetMs := frm.FMp4SegmentValueMs;

        // Audio recording
        ASetup.AudioRecordingsDir := frm.edAudioRecordingsDirName.Text;
        ASetup.AudioRecordingsPath := frm.edAudioRecordingsDirName.Hint;

        // Video recording
        ASetup.VideoRecordingsDir := frm.edVideoRecordingsDirName.Text;
        ASetup.VideoRecordingsPath := frm.edVideoRecordingsDirName.Hint;

        // Database
        ASetup.DatabaseDir := frm.edDataBaseDirName.Text;
        ASetup.DatabasePath := frm.edDataBaseDirName.Hint;

        // Artwork
        ASetup.LocalArtworkDir := frm.edArtworkDirName.Text;
        ASetup.LocalArtworkPath := frm.edArtworkDirName.Hint;

        // Camera
        if (frm.cbSelectCamera.ItemIndex >= 0) then
          begin

            DevItem := TCameraDeviceItem(frm.cbSelectCamera.Items.Objects[frm.cbSelectCamera.ItemIndex]);

            if Assigned(DevItem) then
              begin

                if (DevItem.DeviceProperties.lpFriendlyName <> nil) then
                  ASetup.CameraName := DevItem.DeviceProperties.lpFriendlyName
                else
                  ASetup.CameraName := '';

                if (DevItem.DeviceProperties.lpSymbolicLink <> nil) then
                  ASetup.CameraSymbolicLink := DevItem.DeviceProperties.lpSymbolicLink
                else
                  ASetup.CameraSymbolicLink := '';
              end;
          end;
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
    FreeComboBoxObjects(cb);

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


procedure TfrmSetup.PopulateVideoCaptureDevices(cb: TComboBox;
                                                const CurrentSymbolicLink: string);
var
  Devices: TDevicePropertiesArray;
  I: Integer;
  Item: TCameraDeviceItem;
  DisplayName: string;
  SymbolicLink: string;
  SelIdx: Integer;
  hr: HRESULT;

begin

  cb.Items.BeginUpdate;

  try

    FreeComboBoxObjects(cb);

    SelIdx := -1;
    SetLength(Devices,
              0);

    hr := EnumCaptureDeviceSources(MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE_VIDCAP_GUID,
                                   Devices);
    if FAILED(hr) then
      Exit;

    for I := 0 to Length(Devices) - 1 do
      begin

        if Devices[I].lpDisplayName <> nil then
          DisplayName := Devices[I].lpDisplayName
        else
          if Devices[I].lpFriendlyName <> nil then
            DisplayName := Devices[I].lpFriendlyName
          else
            DisplayName := '(Unknown camera)';

        if Devices[I].lpSymbolicLink <> nil then
          SymbolicLink := Devices[I].lpSymbolicLink
        else
          SymbolicLink := '';

        Item := TCameraDeviceItem.Create(Devices[I]);

        cb.Items.AddObject(DisplayName,
                           Item);

        if (CurrentSymbolicLink <> '') and
           SameText(CurrentSymbolicLink,
                    SymbolicLink) then
          SelIdx := cb.Items.Count - 1;
      end;

    if (SelIdx < 0) and (cb.Items.Count > 0) then
      SelIdx := 0;

    cb.ItemIndex := SelIdx;

  finally

    cb.Items.EndUpdate;
  end;
end;


procedure TfrmSetup.tbAudioBufferDurationChange(Sender: TObject);
begin

  lblBuffSize.Caption := Format('%d ms',
                                [tbAudioBufferDuration.Position]);
end;

// Because this trackbar has no step property, we need to improvise.
procedure TfrmSetup.tbMp4SegmentSizeChange(Sender: TObject);
begin

  FMp4SegmentValueMs := SnapMs(tbMp4SegmentSize.Position,
                               100,   // snap step: 100 ms
                               50,    // min: 0.05 sec
                               9000); // max: 9 sec

  lblMp4SegmentSize.Caption := FormatFloat('0.00 sec',
                                           FMp4SegmentValueMs / 1000);
end;


procedure TfrmSetup.tbRecCapBufferSizeChange(Sender: TObject);
begin

  lblAudioRecBufSize.Caption := Format('%d ms',
                                       [tbMp4SegmentSize.Position]);
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


procedure TfrmSetup.btnCaddyArtworkPathClick(Sender: TObject);
begin

  edtCaddyArtworkPath.Text := PickPath(True,
                                      'Covers Directory'#0'*.jpg'#0#0);
end;


procedure TfrmSetup.btnCaddyVideoPathClick(Sender: TObject);
begin

  edtCaddyVideoPath.Text := PickPath(True,
                                     'Video Directory'#0'*.mp4'#0#0);
end;


procedure TfrmSetup.btnAudioRecordingsDirNameClick(Sender: TObject);
begin

  edAudioRecordingsDirName.Text :=  ExtractFileName(ExtractFileDir(PickPath(False,
                                                                   'Audio Recordings Directory'#0'*.*'#0#0)));
end;


procedure TfrmSetup.btnArtworkDirNameClick(Sender: TObject);
begin

  edArtworkDirName.Text :=  ExtractFileName(ExtractFileDir(PickPath(False,
                                                                     'Artwork Directory'#0'*.*'#0#0)));
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


procedure TfrmSetup.btnVideoRecordingsDirNameClick(Sender: TObject);
begin

  edVideoRecordingsDirName.Text :=  ExtractFileName(ExtractFileDir(PickPath(False,
                                                                   'Video Recordings Directory'#0'*.*'#0#0)));
end;


procedure TfrmSetup.cbSelectCameraChange(Sender: TObject);
var
  Cam: TCameraDeviceItem;

begin

  if (cbSelectCamera.ItemIndex < 0) then
    Exit;

  Cam := TCameraDeviceItem(cbSelectCamera.Items.Objects[cbSelectCamera.ItemIndex]);

  if not Assigned(Cam) then
    Exit;

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
  FreeComboBoxObjects(cbMainOut);
  FreeComboBoxObjects(cbCueOut);
  FreeComboBoxObjects(cbMicIn);
end;


// To prevent memory leaks.
procedure TfrmSetup.FreeComboBoxObjects(cb: TComboBox);
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
