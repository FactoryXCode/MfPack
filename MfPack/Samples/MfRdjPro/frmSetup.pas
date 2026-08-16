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
  WinApi.Winsock2,
  WinApi.IpTypes,
  WinApi.IpHlpApi,
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

  TLocalIPv4Item = class
  public

    Address: string;

    constructor Create(const AAddress: string);
  end;


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
    chkOverrideSleepMode: TMPxpButton;
    Bevel11: TBevel;
    lblLocalNetwork: TLabel;
    lblLocalNetworkHint: TLabel;
    cbLocalIPv4: TComboBox;
    btnDiscoverNetwork: TMPxpButton;
    btnUseLocalIPv4: TMPxpButton;
    btnRemoveLocalIPv4: TMPxpButton;

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
    procedure btnDiscoverNetworkClick(Sender: TObject);
    procedure btnUseLocalIPv4Click(Sender: TObject);
    procedure btnRemoveLocalIPv4Click(Sender: TObject);

  private

    FMp4SegmentValueMs: Integer;
    FOriginalCaddyLanAddress: string;
    FSelectedCaddyLanAddress: string;
    FUseLocalIPv4: Boolean;


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
    procedure SetArtworkPath(const APath: string);
    procedure PopulateLocalIPv4Addresses();
    function SelectedLocalIPv4Address(): string;
    procedure UpdateLocalNetworkUi();
    function UpdateManagedCaddyLanAddress(const AConfigFileName,
                                                ACaddyRoot,
                                                ALanAddress: string;
                                          out AErrorMessage,
                                              AWarningMessage: string): Boolean;
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
  System.Math,
  Vcl.Clipbrd;

const

  RDJ_IF_TYPE_SOFTWARE_LOOPBACK = 24;
  // IF_OPER_STATUS values returned by GetAdaptersAddresses start at 1.
  // Delphi XE7's Winapi.IpTypes declaration incorrectly starts the enum at 0.
  RDJ_IF_OPER_STATUS_UP = 1;
  RDJ_CADDY_LAN_MARKER = '# RDJ Pro managed LAN address: ';


constructor TLocalIPv4Item.Create(const AAddress: string);
begin

  inherited Create();

  Address := AAddress;
end;


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
    DEVICE_STATE_ACTIVE:     Result := '[Active]';
    DEVICE_STATE_DISABLED:   Result := '[Disabled]';
    DEVICE_STATE_NOTPRESENT: Result := '[Not present]';
    DEVICE_STATE_UNPLUGGED:  Result := '[Unplugged]';
  else
    Result := Format('[State 0x%x]',
                     [S]);
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
    frm.chkOverrideSleepMode.Checked := ASetup.SystemOverrideSleepMode;
    frm.chkEnableStreamSwitchDetection.Checked := ASetup.AudioRecorderEnableStreamSwitchDetection;
    frm.cbxOutputFormat.ItemIndex := ASetup.AudioRecorderAudioFormat;

    // Caddy / json
    frm.edtCaddyPath.Text := ASetup.CaddyDir;
    frm.edtCaddyConfigPath.Text := ASetup.CaddyConfigFile;
    frm.edtCaddyJsonNowPlayingPath.Text := ASetup.CaddyNowPlayingJsonFile;
    frm.SetArtworkPath(ASetup.CaddyArtworkPath);
    frm.edtCaddyVideoPath.Text := ASetup.CaddyVideoPath;
    frm.edtCaddyContentTypeURL.Text := ASetup.CaddyContentTypeURL;
    frm.edtCaddyCmdLine.Text := ASetup.CaddyCommand;
    frm.FOriginalCaddyLanAddress := Trim(ASetup.CaddyLanAddress);
    frm.FSelectedCaddyLanAddress := frm.FOriginalCaddyLanAddress;
    frm.FUseLocalIPv4 := frm.FOriginalCaddyLanAddress <> '';

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

        ASetup.SystemOverrideSleepMode := frm.chkOverrideSleepMode.Checked;

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
        ASetup.CaddyLanAddress := frm.FSelectedCaddyLanAddress;

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

        // Paths\Artwork is a compatibility alias for Caddy\Artwork.
        ASetup.LocalArtworkPath := ASetup.CaddyArtworkPath;
        ASetup.LocalArtworkDir := ExtractFileName(ExcludeTrailingPathDelimiter(ASetup.CaddyArtworkPath));

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

        if (Devices[I].lpDisplayName <> nil) then
          DisplayName := Devices[I].lpDisplayName
        else
          if (Devices[I].lpFriendlyName <> nil) then
            DisplayName := Devices[I].lpFriendlyName
          else
            DisplayName := '(Unknown camera)';

        if (Devices[I].lpSymbolicLink <> nil) then
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
var
  Path: string;

begin

  Path := PickPath(True,
                   'Covers Directory'#0'*.jpg'#0#0);
  if (Path <> '') then
    SetArtworkPath(Path);
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
var
  Path: string;

begin

  Path := PickPath(True,
                   'Artwork Directory'#0'*.*'#0#0);
  if (Path <> '') then
    SetArtworkPath(Path);
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


procedure TfrmSetup.SetArtworkPath(const APath: string);
var
  Path: string;

begin

  Path := ExcludeTrailingPathDelimiter(Trim(APath));
  edtCaddyArtworkPath.Text := Path;
  edArtworkDirName.Text := ExtractFileName(Path);
  edArtworkDirName.Hint := Path;
end;


function TfrmSetup.UpdateManagedCaddyLanAddress(const AConfigFileName,
                                                      ACaddyRoot,
                                                      ALanAddress: string;
                                                out AErrorMessage,
                                                    AWarningMessage: string): Boolean;
var
  AddressHeader: string;
  AddressToken: string;
  AddressTokens: TStringList;
  BackupFileName: string;
  CaddyExe: string;
  ConfigFileName: string;
  DesiredAddress: string;
  I: Integer;
  Line: string;
  Lines: TStringList;
  ManagedAddress: string;
  MarkerIndex: Integer;
  SiteIndex: Integer;
  TempFileName: string;

  function RunCaddy(const AArguments: string;
                    out AExitCode: DWORD): Boolean;
  var
    CommandLine: string;
    ProcessInfo: TProcessInformation;
    StartupInfo: TStartupInfo;
    WaitResult: DWORD;

  begin

    Result := False;
    AExitCode := DWORD(-1);
    FillChar(StartupInfo,
             SizeOf(StartupInfo),
             0);
    StartupInfo.cb := SizeOf(StartupInfo);
    FillChar(ProcessInfo,
             SizeOf(ProcessInfo),
             0);

    CommandLine := '"' + CaddyExe + '" ' + AArguments;
    UniqueString(CommandLine);

    if not CreateProcess(nil,
                         PChar(CommandLine),
                         nil,
                         nil,
                         False,
                         CREATE_NO_WINDOW,
                         nil,
                         PChar(ExtractFileDir(CaddyExe)),
                         StartupInfo,
                         ProcessInfo) then
      Exit;

    try

      WaitResult := WaitForSingleObject(ProcessInfo.hProcess,
                                        15000);
      if (WaitResult <> WAIT_OBJECT_0) then
        begin

          TerminateProcess(ProcessInfo.hProcess,
                           ERROR_TIMEOUT);
          Exit;
        end;

      if not GetExitCodeProcess(ProcessInfo.hProcess,
                                AExitCode) then
        Exit;

      Result := True;

    finally

      CloseHandle(ProcessInfo.hThread);
      CloseHandle(ProcessInfo.hProcess);
    end;
  end;


  function FindManagedMarker(): Integer;
  var
    J: Integer;

  begin

    Result := -1;
    for J := 0 to Lines.Count - 1 do
      if SameText(Copy(Trim(Lines[J]),
                       1,
                       Length(RDJ_CADDY_LAN_MARKER)),
                  RDJ_CADDY_LAN_MARKER) then
        Exit(J);
  end;


  function FindSiteLine(const AStartIndex: Integer): Integer;
  var
    J: Integer;
    S: string;

  begin

    Result := -1;
    for J := Max(0,
                 AStartIndex) to Lines.Count - 1 do
      begin

        S := Trim(Lines[J]);
        if (S = '') or
           (S[1] = '#') or
           (S[Length(S)] <> '{') then
          Continue;

        // A site block is top-level and is not a global option or named snippet.
        if (Lines[J] = S) and
           (S[1] <> '{') and
           (S[1] <> '(') then
          Exit(J);
      end;
  end;


  function JoinAddressTokens(): string;
  var
    J: Integer;

  begin

    Result := '';
    for J := 0 to AddressTokens.Count - 1 do
      begin

        if (Result <> '') then
          Result := Result + ', ';
        Result := Result + Trim(AddressTokens[J]);
      end;
  end;


var
  ExitCode: DWORD;

begin

  Result := False;
  AErrorMessage := '';
  AWarningMessage := '';
  ConfigFileName := Trim(AConfigFileName);
  DesiredAddress := Trim(ALanAddress);

  if (ConfigFileName = '') or
     (not FileExists(ConfigFileName)) then
    begin

      AErrorMessage := 'The Caddy configuration file was not found:'#13#10 +
                       ConfigFileName;
      Exit;
    end;

  CaddyExe := IncludeTrailingPathDelimiter(Trim(ACaddyRoot)) + 'caddy.exe';

  if (Trim(ACaddyRoot) = '') or (not FileExists(CaddyExe)) then
    CaddyExe := IncludeTrailingPathDelimiter(ExtractFileDir(ConfigFileName)) + 'caddy.exe';

  if not FileExists(CaddyExe) then
    begin

      AErrorMessage := 'Caddy could not be found, so the configuration cannot be validated:'#13#10 + CaddyExe;
      Exit;
    end;

  Lines := TStringList.Create;
  AddressTokens := TStringList.Create;
  try

    Lines.LoadFromFile(ConfigFileName);
    MarkerIndex := FindManagedMarker();
    ManagedAddress := '';

    if (MarkerIndex >= 0) then
      begin

        ManagedAddress := Trim(Copy(Trim(Lines[MarkerIndex]),
                                    Length(RDJ_CADDY_LAN_MARKER) + 1,
                                    MaxInt));
        SiteIndex := FindSiteLine(MarkerIndex + 1);
      end
    else
      SiteIndex := FindSiteLine(0);

    if (SiteIndex < 0) then
      begin

        AErrorMessage := 'No top-level Caddy site block was found. The configuration was not changed.';
        Exit;
      end;

    Line := Trim(Lines[SiteIndex]);
    AddressHeader := Trim(Copy(Line,
                               1,
                               Length(Line) - 1));
    ExtractStrings([','],
                   [],
                   PChar(AddressHeader),
                   AddressTokens);

    if (ManagedAddress <> '') then
      begin

        AddressToken := 'http://' + ManagedAddress;
        for I := AddressTokens.Count - 1 downto 0 do
          if SameText(Trim(AddressTokens[I]),
                      AddressToken) then
            AddressTokens.Delete(I);
      end;

    if DesiredAddress <> '' then
      begin

        AddressToken := 'http://' + DesiredAddress;
        for I := 0 to AddressTokens.Count - 1 do
          if SameText(Trim(AddressTokens[I]),
                      AddressToken) then
            begin

              AddressToken := '';
              Break;
            end;

        if (AddressToken <> '') then
          AddressTokens.Add(AddressToken);
      end;

    if (AddressTokens.Count = 0) then
      begin

        AErrorMessage := 'Removing the LAN address would leave the Caddy site without an address. ' +
                         'The configuration was not changed.';
        Exit;
      end;

    Lines[SiteIndex] := JoinAddressTokens() + ' {';

    if (MarkerIndex >= 0) then
      begin

        if (DesiredAddress = '') then
          Lines.Delete(MarkerIndex)
        else
          Lines[MarkerIndex] := RDJ_CADDY_LAN_MARKER + DesiredAddress;
      end
    else
      if (DesiredAddress <> '') then
        Lines.Insert(SiteIndex,
                     RDJ_CADDY_LAN_MARKER + DesiredAddress);

    TempFileName := ConfigFileName + '.rdj.tmp';
    BackupFileName := ConfigFileName + '.rdj.bak';
    Lines.SaveToFile(TempFileName);

    try
      // Run Caddy as a service.
      if (not RunCaddy('validate --config "' + TempFileName + '" --adapter caddyfile',
                       ExitCode)) or
         (ExitCode <> 0) then
        begin

          AErrorMessage := Format('Caddy rejected the proposed LAN configuration (exit code %d). '#13#10 +
                                  'The active configuration was not changed.',
                                  [ExitCode]);
          Exit;
        end;

      TFile.Copy(ConfigFileName,
                 BackupFileName,
                 True);

      if not MoveFileEx(PChar(TempFileName),
                        PChar(ConfigFileName),
                        MOVEFILE_REPLACE_EXISTING or MOVEFILE_WRITE_THROUGH) then
        begin

          AErrorMessage := 'The validated Caddy configuration could not replace the active file: ' +
                           SysErrorMessage(GetLastError());
          Exit;
        end;

      if (not RunCaddy('reload --config "' + ConfigFileName + '" --adapter caddyfile',
                       ExitCode)) or (ExitCode <> 0) then
        AWarningMessage := 'The Caddy configuration was safely updated, but Caddy could not be reloaded. ' +
                           'The LAN change will be used the next time Caddy starts.';

      Result := True;

    finally

      if FileExists(TempFileName) then
        DeleteFile(TempFileName);
    end;

  finally

    AddressTokens.Free;
    Lines.Free;
  end;
end;


procedure TfrmSetup.btnOkClick(Sender: TObject);
var
  DesiredLanAddress: string;
  ErrorMessage: string;
  WarningMessage: string;

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

  if FUseLocalIPv4 then
    DesiredLanAddress := Trim(FSelectedCaddyLanAddress)
  else
    DesiredLanAddress := '';

  if not SameText(DesiredLanAddress,
                  FOriginalCaddyLanAddress) then
    begin

      try

        if not UpdateManagedCaddyLanAddress(edtCaddyConfigPath.Text,
                                           edtCaddyPath.Text,
                                           DesiredLanAddress,
                                           ErrorMessage,
                                           WarningMessage) then
          begin

            MessageDlg(ErrorMessage,
                       mtError,
                       [mbOK],
                       0);
            ModalResult := mrNone;
            Exit;
          end;

      except

        on E: Exception do
          begin

            MessageDlg('The Caddy LAN configuration could not be updated:'#13#10 +
                       E.Message + #13#10#13#10 +
                       'The active configuration was not intentionally changed.',
                       mtError,
                       [mbOK],
                       0);
            ModalResult := mrNone;
            Exit;
          end;
      end;

      if (WarningMessage <> '') then
        begin

          MessageDlg(WarningMessage,
                     mtWarning,
                     [mbOK],
                     0);
        end;
    end;

  FSelectedCaddyLanAddress := DesiredLanAddress;
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
  FreeComboBoxObjects(cbLocalIPv4);
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
  PopulateLocalIPv4Addresses();
end;


procedure TfrmSetup.PopulateLocalIPv4Addresses();
var
  Adapter: PIP_ADAPTER_ADDRESSES;
  Adapters: PIP_ADAPTER_ADDRESSES;
  Address: PIP_ADAPTER_UNICAST_ADDRESS;
  AddressBytes: PByte;
  BufferSize: ULONG;
  DisplayName: string;
  ErrorCode: DWORD;
  FriendlyName: string;
  IpAddress: string;
  IsPrivateAddress: Boolean;
  SockAddr: PSockAddrIn;

begin

  FreeComboBoxObjects(cbLocalIPv4);
  lblLocalNetworkHint.Caption := 'Searching active network adapters...';

  BufferSize := 0;
  ErrorCode := GetAdaptersAddresses(AF_INET,
                                    GAA_FLAG_SKIP_ANYCAST or
                                    GAA_FLAG_SKIP_MULTICAST or
                                    GAA_FLAG_SKIP_DNS_SERVER,
                                    nil,
                                    nil,
                                    @BufferSize);

  if (ErrorCode <> ERROR_BUFFER_OVERFLOW) or
     (BufferSize = 0) then
    begin

      lblLocalNetworkHint.Caption := 'No active local IPv4 address was found.';
      Exit;
    end;

  GetMem(Adapters,
         BufferSize);
  try

    ErrorCode := GetAdaptersAddresses(AF_INET,
                                      GAA_FLAG_SKIP_ANYCAST or
                                      GAA_FLAG_SKIP_MULTICAST or
                                      GAA_FLAG_SKIP_DNS_SERVER,
                                      nil,
                                      Adapters,
                                      @BufferSize);
    if ErrorCode <> NO_ERROR then
      begin

        lblLocalNetworkHint.Caption := Format('Network discovery failed (error %d).',
                                              [ErrorCode]);
        Exit;
      end;

    Adapter := Adapters;
    while Assigned(Adapter) do
      begin

        if (Ord(Adapter.OperStatus) = RDJ_IF_OPER_STATUS_UP) and
           (Adapter.IfType <> RDJ_IF_TYPE_SOFTWARE_LOOPBACK) then
          begin

            FriendlyName := Trim(string(Adapter.FriendlyName));
            Address := Adapter.FirstUnicastAddress;

            while Assigned(Address) do
              begin
                if (Address.DadState = IpDadStatePreferred) and
                   Assigned(Address.Address.lpSockaddr) and
                   (Address.Address.lpSockaddr.sa_family = AF_INET) then
                  begin
                    SockAddr := PSockAddrIn(Address.Address.lpSockaddr);
                    AddressBytes := PByte(@SockAddr.sin_addr);

                    IpAddress := Format('%d.%d.%d.%d',
                                        [AddressBytes[0],
                                         AddressBytes[1],
                                         AddressBytes[2],
                                         AddressBytes[3]]);

                    if (AddressBytes[0] <> 0) and
                       (AddressBytes[0] <> 127) then
                      begin
                        DisplayName := IpAddress;

                        if (FriendlyName <> '') then
                          DisplayName := DisplayName + ' - ' + FriendlyName;

                        IsPrivateAddress := (AddressBytes[0] = 10) or
                                            ((AddressBytes[0] = 172) and
                                             (AddressBytes[1] >= 16) and
                                             (AddressBytes[1] <= 31)) or
                                            ((AddressBytes[0] = 192) and
                                             (AddressBytes[1] = 168));

                        if IsPrivateAddress then
                          cbLocalIPv4.Items.InsertObject(0,
                                                         DisplayName,
                                                         TLocalIPv4Item.Create(IpAddress))
                        else
                          cbLocalIPv4.Items.AddObject(DisplayName,
                                                      TLocalIPv4Item.Create(IpAddress));
                      end;
                  end;

                Address := Address.Next;
              end;
          end;

        Adapter := Adapter.Next;
      end;

  finally

    FreeMem(Adapters);
  end;

  if (cbLocalIPv4.Items.Count > 0) then
    begin
      cbLocalIPv4.ItemIndex := 0;

      for ErrorCode := 0 to DWORD(cbLocalIPv4.Items.Count - 1) do
        if SameText(TLocalIPv4Item(cbLocalIPv4.Items.Objects[ErrorCode]).Address,
                    FSelectedCaddyLanAddress) then
          begin
            cbLocalIPv4.ItemIndex := Integer(ErrorCode);
            Break;
          end;

      UpdateLocalNetworkUi();
    end
  else
    begin

      lblLocalNetworkHint.Caption := 'No active local IPv4 address was found.';
      UpdateLocalNetworkUi();
    end;
end;


function TfrmSetup.SelectedLocalIPv4Address(): string;
var
  Item: TLocalIPv4Item;

begin

  Result := '';
  if (cbLocalIPv4.ItemIndex < 0) or (cbLocalIPv4.ItemIndex >= cbLocalIPv4.Items.Count) then
    Exit;

  Item := TLocalIPv4Item(cbLocalIPv4.Items.Objects[cbLocalIPv4.ItemIndex]);
  if Assigned(Item) then
    Result := Item.Address;
end;


procedure TfrmSetup.btnDiscoverNetworkClick(Sender: TObject);
begin

  PopulateLocalIPv4Addresses();
end;


procedure TfrmSetup.UpdateLocalNetworkUi();
var
  IpAddress: string;

begin

  btnUseLocalIPv4.Enabled := cbLocalIPv4.Items.Count > 0;
  btnRemoveLocalIPv4.Enabled := FUseLocalIPv4 or (FOriginalCaddyLanAddress <> '');

  if FUseLocalIPv4 then
    begin
      IpAddress := FSelectedCaddyLanAddress;
      lblLocalNetworkHint.Caption := 'LAN access will use http://' + IpAddress +
                                     '/. Click OK to validate and update Caddy.';
      Exit;
    end;

  if (FOriginalCaddyLanAddress <> '') then
    lblLocalNetworkHint.Caption := 'LAN access will be removed from Caddy when you click OK.'
  else
    lblLocalNetworkHint.Caption := 'Select an address and click Use address. No Caddy changes are made until OK.';
end;


procedure TfrmSetup.btnUseLocalIPv4Click(Sender: TObject);
begin

  FSelectedCaddyLanAddress := SelectedLocalIPv4Address();
  if (FSelectedCaddyLanAddress = '') then
    begin
      MessageDlg('No local IPv4 address is selected.',
                 mtInformation,
                 [mbOK],
                 0);
      Exit;
    end;

  FUseLocalIPv4 := True;
  Clipboard.AsText := FSelectedCaddyLanAddress;
  UpdateLocalNetworkUi();
end;


procedure TfrmSetup.btnRemoveLocalIPv4Click(Sender: TObject);
begin

  FUseLocalIPv4 := False;
  FSelectedCaddyLanAddress := '';
  UpdateLocalNetworkUi();
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
