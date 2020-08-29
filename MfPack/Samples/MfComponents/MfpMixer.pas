// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: Media Foundation - MFPack
// Project location: http://sourceforge.net/projects/MFPack
// Module: MfpMixer.pas
// Kind: Pascal Unit Component
// Release date: 05-08-2016
// Language: ENU
//
// Version: 3.0.0
//
// Description: Sample component to get/set the proper mixer (and properties)
//              used in Vista, 7, 8 and 10
//
// Company: FactoryX
// Intiator(s): Tony (maXcomX), Peter (OzShips), Ramyses De Macedo Rodrigues.
// Contributor(s): Tony Kalf (maXcomX), Peter Larson (ozships), Ramyses De Macedo Rodrigues.
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 13/08/2020 All                 Enigma release. New layout and namespaces
//------------------------------------------------------------------------------
//
// Remarks: This unit contains samples of how to create the soundmixerdevice
//          objects and managing them.
//
// Related objects: -
// Related projects: MfPackX300
// Known Issues: -
//
// Compiler version: 23 up to 33
// SDK version: 10.0.19041.0
//
// Todo: -
//
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
//
// Users may distribute this source code provided that this header is included
// in full at the top of the file.
//
//==============================================================================

unit MfpMixer;

interface

uses
  {WinApi}
  WinApi.Windows,
  WinApi.CommCtrl,
  WinApi.Messages,
  WinApi.WinApiTypes,
  WinApi.Coml2Api,
  WinApi.ComBaseApi,
  {System}
  System.Classes,
  System.Win.ComObj,
  {ActiveX}
  WinApi.ActiveX.PropSys,
  WinApi.ActiveX.PropIdl,
  WinApi.ActiveX.ObjBase,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfUtils,
  {WinApi.CoreAudioApi}
  WinApi.CoreAudioApi.Functiondiscoverykeys_devpkey,
  WinApi.CoreAudioApi.MMDeviceApi,
  WinApi.CoreAudioApi.Endpointvolume;

const
  // Defines the maximum and minimum in- and output values
  MAX_INPUT_VALUE  : Single = 1.0;
  MIN_INPUT_VALUE  : Single = 0.0;
  MAX_OUTPUT_VALUE : Single = 1.0;
  MIN_OUTPUT_VALUE : Single = 0.0;

type

  TChannelVolume = record
    ChannelId: UINT;
    ChannelName: widestring;
    VolumeLevel: Single;
  end;

  TChannels = array of TChannelVolume;


  // Callback method for endpoint-volume-change notifications.
  TOnNotify = function (pNotify: AUDIO_VOLUME_NOTIFICATION_DATA): HRESULT of object;

  TMfpMixer = class(TComponent)
  private
    { private fields }

    fChannels: UINT;       // Total number of channels
    arChannels: TChannels; // The array containing properties for each channel

    FDefaultDevice: IMMDevice;
    FDeviceEnumerator: IMMDeviceEnumerator;
    FAudioEndpoint: IAudioEndpointVolume;
    epVolEvents: IAudioEndpointVolumeCallback;

    fDataFlow: EDataFlow; // The data-flow direction for the endpoint device.
    fRole: ERole;         // The role of the endpoint device.

    // Client's proprietary event-context GUID
    g_guidMyContext: TGUID;

    FOnNotify: TOnNotify;

    { private methods }
    function GetMute(): BOOL;
    procedure SetMute(Value: BOOL);
    function GetMasterVolume(): Single;
    procedure SetMasterVolume(Value: Single);
    function GetChannelVolume(Index: UINT): Single;
    procedure SetChannelVolume(Index: UINT; chVolume: Single);
    procedure SetDeviceDataFlow(value: EDataFlow);
    procedure SetDeviceRole(value: ERole);


  protected
    { protected fields }
    fHwnd: HWnd; // Handle to this mixer.

    { protected methods }
    procedure WindProc(var Msg: TMessage); virtual;
    function Notify(pNotify: AUDIO_VOLUME_NOTIFICATION_DATA): HRESULT;

  public
    { public fields }

    { public methods }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function GetDeviceName(const DeviceId: Integer;
                           out devicename: WideString): HRESULT;

    property ChannelVolume[Index: UINT]: Single read GetChannelVolume write SetChannelVolume;

  published
    { published methods }
    property OnNotify: TOnNotify read FOnNotify;

    property ChannelCount: UINT read fChannels;
    property MasterVolume: Single read GetMasterVolume write SetMasterVolume;
    property Mute: BOOL read GetMute write SetMute default BOOL(False);
    property DeviceDataFlow: EDataFlow read fDataFlow write SetDeviceDataFlow default eRender;
    property DeviceRole: ERole read fRole write SetDeviceRole default eMultimedia;

  end;

procedure Register;

// ---------------------------------------------------------------------------

implementation

procedure Register;
begin
  RegisterComponents('MfPack Samples', [TMfpMixer]);
end;


// TMfpMixer
//==========

// Constructor
constructor TMfpMixer.Create(AOwner: TComponent);
var
  hr: HResult;
  i: Integer;
  chname: WideString;
  chvol: Single;

label
  done;

begin
  inherited Create(AOwner);

  CoInitialize(Nil);

  hr := CoCreateGuid(g_guidMyContext);
  if FAILED(hr) then
    goto done;

  hr := CoCreateInstance(CLSID_MMDeviceEnumerator,
                         Nil,
                         INT(CLSCTX_INPROC_SERVER),
                         IID_IMMDeviceEnumerator,
                         FDeviceEnumerator);
  if FAILED(hr) then
    goto done;


  // You can easily modify for the default capture device.
  // Change the value of the first parameter in the call to the from eRender to eCapture.
  hr := FDeviceEnumerator.GetDefaultAudioEndpoint(fDataFlow,
                                                  fRole,
                                                  FDefaultDevice);
  if FAILED(hr) then
    goto done;


  hr := FDefaultDevice.Activate(IID_IAudioEndpointVolume,
                                INT(CLSCTX_INPROC_SERVER),
                                Nil,
                                IUnknown(FAudioEndpoint));
  if FAILED(hr) then
    goto done;

  hr := FAudioEndpoint.RegisterControlChangeNotify(EPVolEvents);
  if FAILED(hr) then
    goto done;

  // Get all the channels
  hr := FAudioEndpoint.GetChannelCount(fChannels);
  if FAILED(hr) then
    goto done;

  // Set the length of the array
  SetLength(arChannels, fChannels);

  // Get the channelnames
  for i := 0 to fChannels - 1 do
    begin
      GetDeviceName(i,
                    chName);

      chvol := GetChannelVolume(i);

      // Fill the record
      with arChannels[i] do
        begin
          ChannelId := i;
          ChannelName := chName;
          VolumeLevel := chvol;
        end;
    end;

done:
  if ((csDesigning in ComponentState) = False) then
    if FAILED(hr) then
      begin
        OleCheck(hr);
        Destroy;
      end;
end;


// Destructor
destructor TMfpMixer.Destroy;
begin

  if (FDeviceEnumerator <> Nil) then
    OleCheck(FAudioEndpoint.UnregisterControlChangeNotify(EPVolEvents));

  SafeRelease(FDeviceEnumerator); //SafeRelease is declared in unit MfpUtils.pas
  SafeRelease(FDefaultDevice);
  SafeRelease(FAudioEndpoint);

  inherited Destroy;

end;


function TMfpMixer.Notify(pNotify: AUDIO_VOLUME_NOTIFICATION_DATA): HRESULT;
begin

  if (@pNotify = Nil) then
    begin
      Result := E_INVALIDARG;
      Exit;
    end;

  if (fHwnd <> 0) And (pNotify.guidEventContext <> g_guidMyContext) then
    begin
      {PostMessage(GetDlgItem(fHwnd, IDC_CHECK_MUTE),
                  BM_SETCHECK,
                  WPARAM(pNotify.bMuted) , //  ? BST_CHECKED : BST_UNCHECKED
                  LPARAM(0));
      }
      PostMessage(fHwnd,
                  TBM_SETPOS,
                  WPARAM(True),
                  LPARAM(UINT32(100 * Trunc(pNotify.fMasterVolume + 0.5))));


    end;

  Result := S_OK;

end;

//
function TMfpMixer.GetDeviceName(const DeviceId: integer;
                                 out devicename: WideString): HRESULT;
var
  hr: HResult;
  psPropertyStore: IPropertyStore;
  pvvar: PROPVARIANT;

label
  done;

begin

  PropVariantInit(pvvar);

  hr := FDefaultDevice.OpenPropertyStore(STGM_READ,
                                        psPropertyStore);
  if SUCCEEDED(hr) then
    begin
      // Get the Endpoint device friendly name or description
      // (in WCHAR string format) for the endpoint device.
      // Possible values are:
      // - PKEY_Device_FriendlyName
      // - PKEY_Device_DeviceDesc
      // - PKEY_DeviceInterface_FriendlyName
      hr := psPropertyStore.GetValue(PKEY_Device_LocationInfo,
                                     pvvar);

      if FAILED(hr) then
        begin
          OleCheck(hr);
          goto done;
        end;

      devicename := pvvar.pwszVal;

    end;

done:
  PropVariantClear(pvvar);
  Result := hr;
end;

//
function TMfpMixer.GetMute(): BOOL;
var
  hr: HResult;
  Res: BOOL;

begin
  hr := FAudioEndpoint.GetMute(INT(Res));

  if FAILED(hr) then
    OleCheck(hr);

  Result := Res;
end;

//
procedure TMfpMixer.SetMute(Value: BOOL);
var
  hr: HResult;

begin
  // This is a workaround on the BOOL issue.
  // See comment at DCBOOL in MfPack.WTypes.pas.
  hr := FAudioEndpoint.SetMute(INT(Value),
                               g_guidMyContext);
  OleCheck(hr);
end;

//
function TMfpMixer.GetMasterVolume(): Single;
var
  hr: HResult;
  sVolLevel: Single;

begin
  hr := FAudioEndpoint.GetMasterVolumeLevelScalar(sVolLevel);

  if FAILED(hr) then
    begin
      OleCheck(hr);
      Result := 0.0;
    end
  else
    Result := sVolLevel;
end;

//
procedure TMfpMixer.SetMasterVolume(value: Single);
var
  hr: HResult;

begin
  if (value < MIN_INPUT_VALUE) then
    value := MIN_INPUT_VALUE;
  if (value > MAX_INPUT_VALUE) then
    value := MAX_INPUT_VALUE;

  hr := FAudioEndpoint.SetMasterVolumeLevelScalar(value,
                                                  GUID_NULL);

  OleCheck(hr);
end;

//
function TMfpMixer.GetChannelVolume(Index: UINT): Single;
var
  hr: HResult;
  chvol: Single;

begin

  hr := FAudioEndpoint.GetChannelVolumeLevelScalar(Index,
                                                   chvol);
  if FAILED(hr) then
    begin
      OleCheck(hr);
      chvol := 0.0;
    end;

  Result := chvol;
end;

//
procedure TMfpMixer.SetChannelVolume(Index: UINT;
                                     chVolume: Single);
var
  hr: HResult;
  sVal: Single;

begin

  if (chVolume < MIN_INPUT_VALUE) then
    sVal := MIN_INPUT_VALUE
  else if (chVolume > MAX_INPUT_VALUE) then
    sVal := MAX_INPUT_VALUE
  else
    sVal := chVolume;


  hr := FAudioEndpoint.SetChannelVolumeLevelScalar(Index,
                                                   sVal,
                                                   GUID_NULL);
  OleCheck(hr);
end;


procedure TMfpMixer.SetDeviceDataFlow(value: EDataFlow);
begin
  if (fDataFlow <> value) then
    fDataFlow := value;
end;


procedure TMfpMixer.SetDeviceRole(value: ERole);
begin
  if (fRole <> value) then
    fRole := value;
end;


procedure TMfpMixer.WindProc(var Msg: TMessage);
var
  Handled: Boolean;

begin

  case Msg.Msg of
    0: Handled := True; //dummy
  else
    Handled := False;
  end;

  if Handled then
    Msg.Result := 0
  else
    Msg.Result := DefWindowProc(fHWnd,
                                Msg.Msg,
                                Msg.WParam,
                                Msg.LParam);
end;

end.
