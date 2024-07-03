// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: Media Foundation - MFPack
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: MfAudioEndPoint.pas
// Kind: Pascal Unit Component
// Release date: 13-08-2020
// Language: ENU
//
// Version: 3.1.6
//
// Description: Component to manage capture or render endpoints and properties.
//              It also provides an audio endpoint callback.
//
// Company: FactoryX
// Intiator(s): Tony (maXcomX), Peter (OzShips), Ramyses De Macedo Rodrigues.
// Contributor(s): Tony Kalf (maXcomX), Peter Larson (ozships).
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 30/06/2024 All                 RammStein release  SDK 10.0.26100.0 (Windows 11)
// 12/06/2024 Tony                Removed EDataFlowEx.
//------------------------------------------------------------------------------
//
// Remarks: -
//
// Related objects: -
// Related projects: MfPackX317
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.26100.0
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
// Non commercial users may distribute this sourcecode provided that this
// header is included in full at the top of the file.
// Commercial users are not allowed to distribute this sourcecode as part of
// their product.
//
//==============================================================================
unit MfAudioEndPoint;

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
  System.SysUtils,
  System.Classes,
  System.Win.ComObj,
  {ActiveX}
  WinApi.ActiveX.PropSys,
  WinApi.ActiveX.PropIdl,
  WinApi.ActiveX.ObjBase,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfUtils,
  {CoreAudioApi}
  WinApi.CoreAudioApi.FunctionDiscoveryKeys_devpkey,
  WinApi.CoreAudioApi.MMDeviceApi,
  WinApi.CoreAudioApi.MMDevApiUtils,
  WinApi.CoreAudioApi.Endpointvolume;

const
  // Defines the maximum and minimum in- and output values
  MAX_INPUT_VALUE  : Single = 1.0;
  MIN_INPUT_VALUE  : Single = 0.0;
  MAX_OUTPUT_VALUE : Single = 1.0;
  MIN_OUTPUT_VALUE : Single = 0.0;

  // Message ID's
  WM_MIXERNOTIFY            = WM_APP + 200;

type
  eState = (DEVICE_STATE_ACTIVE      = $00000001,
            DEVICE_STATE_DISABLED    = $00000002,
            DEVICE_STATE_NOTPRESENT  = $00000004,
            DEVICE_STATE_UNPLUGGED   = $00000008,
            DEVICE_STATEMASK_ALL     = $0000000F);

type

  // Callback method for endpoint-volume-change notifications from IAudioEndpointVolumeCallback.
  TOnNotify = procedure (Sender: TObject; pNotify: PAUDIO_VOLUME_NOTIFICATION_DATA) of object;

  // Callback COM interface
  TOnEndPointNotify = class(TInterfacedObject, IAudioEndpointVolumeCallback)
    function OnNotify(pNotify: PAUDIO_VOLUME_NOTIFICATION_DATA): HRESULT; stdcall;
  end;

  TMfAudioEndPoint = class(TComponent)
  private
    dwDeviceID: DWord;  // Device ID
    uiChannels: UINT;   // Total number of channels
    dwEndPointsCount: DWord;
    wsDeviceName: string;
    wsDeviceInterfaceName: string;
    wsDeviceDesc: string;

    fEndPointDevices: TEndPointDeviceArray;  // Array containing properties for each endpointdevice found on this system.

    fSelectedIMMDevice: IMMDevice;
    fDeviceEnumerator: IMMDeviceEnumerator;
    fAudioEndpoint: IAudioEndpointVolumeEx;
    fOnEndPointNotify: TOnEndPointNotify;

    fDataFlow: EDataFlow; // The data-flow direction for the endpoint device.

    fRole: ERole;       // The role of the endpoint device.
                        // The IMMDeviceEnumerator.GetDefaultAudioEndpoint and
                        // IMMNotificationClient.OnDefaultDeviceChanged methods use the
                        // constants defined in the ERole enumeration.
                        // eConsole         = Games, system notification sounds, and voice commands.
                        // eMultimedia      = Music, movies, narration, and live music recording.
                        // eCommunications  = Voice communications (talking to another person).

    fState: eState;     // The state or states of the endpoints that are to be included in the collection
                        // Possible values or combinations are:
                        //   DEVICE_STATE_ACTIVE, DEVICE_STATE_DISABLED, DEVICE_STATE_NOTPRESENT or DEVICE_STATE_UNPLUGGED
                        //   To include all endpoints, regardless of state, set dwStateMask = DEVICE_STATEMASK_ALL

    g_guidEventContext: TGuid; // Client's proprietary event-context GUID
    FOnNotify: TOnNotify;      // Component's Eventhandler

    function GetGuidContextAsString(): string;

    // The GetChannels method gets a count of the channels in the audio stream that enters or leaves the audio endpoint device.
    function GetChannels(): UINT;

    function GetMute(): BOOL;
    procedure SetMute(aValue: BOOL);

    function GetMasterScalarVolume(): Single;
    procedure SetMasterScalarVolume(aValue: Single);

    function GetMasterDbVolume(): Single;
    procedure SetMasterDbVolume(aValue: Single);

    procedure SetDataFlow(aValue: EDataFlow);
    procedure SetDeviceState(aValue: string);

    procedure SetEndPointDevice(aValue: DWord);

  protected
    // Catches all messages for this object and posts them to the OnNotify eventhandler.
    procedure WindProc(var Msg: TMessage);

  public
    fHwnd: THandle;    // Handle to this mixer.

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    // Gets the volume range
    procedure GetVolumeRange(out pflVolumeMindB: Single;
                             out pflVolumeMaxdB: Single;
                             out pflVolumeIncrementdB: Single);
    // The GetChannelVolumeLevel method gets the volume level, in decibels,
    // of the specified channel in the audio stream that enters or leaves the audio endpoint device.
    function GetChannelScalarVolume(Index: UINT): Single;
    // The SetChannelVolumeLevelScalar method sets the normalized,
    // audio-tapered volume level of the specified channel in the audio stream that
    // enters or leaves the audio endpoint device.
    procedure SetChannelScalarVolume(Index: UINT; chVolume: Single);

    function GetStateAsString(): string;
    function SupportsHardware(HardwareSupportMask: DWord = 0): Boolean;
    function VolumeStepUp(const GuidEventContext: TGuid): HResult;
    function VolumeStepDown(const GuidEventContext: TGuid): HResult;
    function RegisterAudioEndpointVolumeCallback(pNotify: IAudioEndpointVolumeCallback): HResult;
    function UnregisterAudioEndpointVolumeCallback(pNotify: IAudioEndpointVolumeCallback): HResult;

    // HELPERS /////////////////////////////////////////////////////////////////////

    // Get the Endpoint device friendly name or description
    // (in WCHAR string format) for the endpoint device.
    function GetAudioDeviceDescriptions(DefaultDevice: IMMDevice; // call GetDefaultAudioEndPointDevice first to obtain the default device.
                                        const DevicePkey: PROPERTYKEY; // Possible values are: PKEY_Device_FriendlyName, PKEY_Device_DeviceDesc or PKEY_DeviceInterface_FriendlyName.
                                        out deviceDesc: WideString): HResult;

    // Get EndpointDevices
    function GetAudioEndPoints(const flow: EDataFlow;
                               state: eState;
                               out endpointdevices: TEndPointDeviceArray): HResult;

    // Get the default audio endpoint
    function GetDefaultAudioEndPointDevice(out audioEndPoint: IMMDevice): HResult;

    // Non visual properties
    property Handle: THandle read fHwnd;
    property IMMDevice: IMMDevice read fSelectedIMMDevice;
    property ChannelVolume[_Index: UINT]: Single read GetChannelScalarVolume write SetChannelScalarVolume;
    property Devices: TEndPointDeviceArray read FEndPointDevices;
    property EndPointsCount: DWord read dwEndPointsCount;

  published
    { published methods }
    // Read-only properties
    property DeviceName: string read wsDeviceName;
    property DeviceInterfaceName: string read wsDeviceInterfaceName;
    property DeviceDescription: string read wsDeviceDesc;
    property DeviceGuidContext: string read GetGuidContextAsString;  // Guidcontext of this control
    property DeviceRole: ERole read fRole;
    property State: eState read fState default DEVICE_STATE_ACTIVE;
    property Channels: UINT read GetChannels;

    // read/write properties
    property DeviceID: DWord read dwDeviceID write SetEndPointDevice default 0;
    property DeviceDataFlow: EDataFlow read fDataFlow write SetDataFlow default eRender;
    property DeviceState: string read GetStateAsString write SetDeviceState;
    property MasterScalarVolume: Single read GetMasterScalarVolume write SetMasterScalarVolume;
    property MasterDbVolume: Single read GetMasterDbVolume write SetMasterDbVolume;
    property Mute: BOOL read GetMute write SetMute default BOOL(False);
    property OnNotify: TOnNotify read FOnNotify write FOnNotify;
  end;

var
  hhwnd: THandle;

procedure Register;


implementation


procedure Register;
begin
  RegisterComponents('MfPack Core Audio Samples', [TMfAudioEndPoint]);
end;

// Constructor
constructor TMfAudioEndPoint.Create(AOwner: TComponent);
var
  hr: HResult;
label
  done;
begin
  inherited Create(AOwner);
  CoInitialize(Nil);

  // Get handle for this mixer
  fHwnd := AllocateHWnd(WindProc);
  hhwnd := fHwnd;

  // Create the contextguid
  hr := CoCreateGuid(g_guidEventContext);
  if FAILED(hr) then
    goto done;

  // We want to know what the current default endpoint device is.
  hr := CoCreateInstance(CLSID_MMDeviceEnumerator,
                         Nil,
                         INT(CLSCTX_INPROC_SERVER),
                         IID_IMMDeviceEnumerator,
                         FDeviceEnumerator);
  if FAILED(hr) then
    goto done;

  // If you need the default capture device,
  // change the value of the first parameter (fDataFlow) in the call to the from eRender to eCapture.
  fRole := eMultimedia;
  fDataFlow := eRender;
  fState := DEVICE_STATE_ACTIVE;
  dwDeviceID := 0;

  // Get all EndpointDevices and store them in an array
  hr := GetEndpointDevices(fDataFlow,
                           DWord(fState),
                           FEndPointDevices,
                           dwEndPointsCount);
  if FAILED(hr) then
    goto done;

  if Assigned(FEndPointDevices) then
    SetEndPointDevice(dwDeviceID);

done:
  if ((csDesigning in ComponentState) = False) then
    if FAILED(hr) then
      begin
        OleCheck(hr);
        Abort;
      end;
end;

// Destructor
destructor TMfAudioEndPoint.Destroy;
begin
  // Unregister callback interface
  if (FDeviceEnumerator <> Nil) then
    OleCheck(FAudioEndpoint.UnregisterControlChangeNotify(FOnEndPointNotify));

  // SafeRelease is declared in unit WinApi.MediaFoundationApi.MfUtils.pas
  SafeRelease(FDeviceEnumerator);
  SafeRelease(fSelectedIMMDevice);
  SafeRelease(FAudioEndpoint);
  // Free the Handle
  DeallocateHWnd(fHwnd);
  SetLength(fEndPointDevices, 0);
  CoUninitialize;
  inherited Destroy;
end;


procedure TMfAudioEndPoint.WindProc(var Msg: TMessage);
var
  msg_lp: PAUDIO_VOLUME_NOTIFICATION_DATA;

begin
  inherited;

  if (Msg.Msg = WM_MIXERNOTIFY) then // Check for mixer messages
    begin
      if (Msg.WParam = S_OK) then
        begin
          msg_lp := PAUDIO_VOLUME_NOTIFICATION_DATA(Msg.LParam);

{$IFDEF DEBUG}
          // Show the output in messages window
          OutputDebugString(pChar('==> OnEndPointNotify.OnNotify messages:'));
          OutputDebugString(pChar(''));
          OutputDebugString(pChar('guidEventContext: ' + GuidToString(msg_lp^.guidEventContext)));
          OutputDebugString(pChar('Muted: ' + BoolToStr(msg_lp^.bMuted)));
          OutputDebugString(pChar('MasterVolume: ' + FloatToStr(msg_lp^.fMasterVolume)));
          OutputDebugString(pChar('Channels: ' + IntToStr(msg_lp^.nChannels)));
          OutputDebugString(pChar('ChannelVolume Left: ' + IntToStr(Round(msg_lp^.afChannelVolumes[0] * 100))));
          if (msg_lp^.nChannels >= 2) then
            OutputDebugString(pChar('ChannelVolume Right: ' + IntToStr(Round(msg_lp^.afChannelVolumes[1] * 100))));
          OutputDebugString(pChar(''));
          OutputDebugString(pChar('<== OnEndPointNotify.OnNotify messages END'));
{$ENDIF}

          // Send received struct to TControl's OnNotify eventhandler.
          if Assigned(FOnNotify) then
            FOnNotify(Self, msg_lp);

          // Set properties
          uiChannels := msg_lp^.nChannels;
          Mute := msg_lp^.bMuted;
        end
    end
      // Any other messages are passed to DefWindowProc, which tells Windows to handle the message.
      // NOTE: The first parameter, fHwnd, is the handle of the window receiving this message.
      //       It is obtained from the call to AllocateHWnd in the Constructor.
  else
    msg.Result := DefWindowProc(fHwnd,
                                Msg.Msg,
                                Msg.WParam,
                                Msg.LParam);
end;


procedure TMfAudioEndPoint.GetVolumeRange(out pflVolumeMindB: Single;
                                          out pflVolumeMaxdB: Single;
                                          out pflVolumeIncrementdB: Single);
var
  hr: HResult;
begin
  hr := FAudioEndpoint.GetVolumeRange(pflVolumeMindB,
                                      pflVolumeMaxdB,
                                      pflVolumeIncrementdB);
  if FAILED(hr) then
    begin
      OleCheck(hr);
      pflVolumeMindB := 0;
      pflVolumeMaxdB := 0;
      pflVolumeIncrementdB := 0;
    end;
end;


function TMfAudioEndPoint.GetGuidContextAsString(): string;
begin
  Result := GuidToString(g_guidEventContext);
end;


function TMfAudioEndPoint.GetChannels(): UINT;
var
  hr: HResult;
  channels: UINT;
begin
  hr := FAudioEndpoint.GetChannelCount(channels);
  if SUCCEEDED(hr) then
    Result := channels
  else
    begin
      OleCheck(hr);
      Result := 0;
    end;
end;


function TMfAudioEndPoint.GetMute(): BOOL;
var
  hr: HResult;
  Res: BOOL;
begin
  hr := FAudioEndpoint.GetMute(INT(Res));
  if FAILED(hr) then
    OleCheck(hr);
  Result := Res;
end;


procedure TMfAudioEndPoint.SetMute(aValue: BOOL);
var
  hr: HResult;
begin
  // This is a workaround on the BOOL issue.
  // See comment at DCBOOL in WinApi.WinApiTypes.pas
  hr := FAudioEndpoint.SetMute(INT(aValue),
                               g_guidEventContext);
  if FAILED(hr) then
    OleCheck(hr);
end;


function TMfAudioEndPoint.GetMasterScalarVolume(): Single;
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

procedure TMfAudioEndPoint.SetMasterScalarVolume(aValue: Single);
var
  hr: HResult;
begin
  if (aValue < MIN_INPUT_VALUE) then
    aValue := MIN_INPUT_VALUE;
  if (aValue > MAX_INPUT_VALUE) then
    aValue := MAX_INPUT_VALUE;
  hr := FAudioEndpoint.SetMasterVolumeLevelScalar(aValue,
                                                  @g_guidEventContext);
  OleCheck(hr);
end;


function TMfAudioEndPoint.GetChannelScalarVolume(Index: UINT): Single;
var
  hr: HResult;
  fLevelDB: Single;
begin
  hr := FAudioEndpoint.GetChannelVolumeLevel(Index,
                                             fLevelDB);
  if SUCCEEDED(hr) then
    Result := fLevelDB
  else
    begin
      OleCheck(hr);
      Result := 0;
    end;
end;

procedure TMfAudioEndPoint.SetChannelScalarVolume(Index: UINT;
                                                  chVolume: Single);
var
  hr: HResult;
begin
  hr := FAudioEndpoint.SetChannelVolumeLevel(Index,
                                             chVolume,
                                             @g_GuidEventContext);
  OleCheck(hr);
end;


function TMfAudioEndPoint.GetStateAsString(): string;
begin
  case fState of
    DEVICE_STATE_ACTIVE:     Result := DEV_STATE_ACTIVE;
    DEVICE_STATE_DISABLED:   Result := DEV_STATE_DISABLED;
    DEVICE_STATE_NOTPRESENT: Result := DEV_STATE_NOTPRESENT;
    DEVICE_STATE_UNPLUGGED:  Result := DEV_STATE_UNPLUGGED;
    DEVICE_STATEMASK_ALL:    Result := DEV_STATEMASK_ALL;
  end;
end;

function TMfAudioEndPoint.GetMasterDbVolume(): Single;
var
  hr: HResult;
  sVolLevel: Single;

begin
  hr := FAudioEndpoint.GetMasterVolumeLevel(sVolLevel);
  if FAILED(hr) then
    begin
      OleCheck(hr);
      Result := 0.0;
    end
  else
    Result := sVolLevel;
end;

procedure TMfAudioEndPoint.SetMasterDbVolume(aValue: Single);
var
  hr: HResult;
begin
  if (aValue < MIN_INPUT_VALUE) then
    aValue := MIN_INPUT_VALUE;
  if (aValue > MAX_INPUT_VALUE) then
    aValue := MAX_INPUT_VALUE;
  hr := FAudioEndpoint.SetMasterVolumeLevel(aValue,
                                            @g_GuidEventContext);
  OleCheck(hr);
end;

procedure TMfAudioEndPoint.SetDataFlow(aValue: EDataFlow);
begin
  // Only these 2 members of the eDataFlow struct are alowed!
  if aValue in [eRender, eCapture] then
    begin
      fDataFlow := aValue;
      OleCheck(GetAudioEndPoints(aValue,
                                 fState,
                                 FEndPointDevices));
      dwDeviceID := 0;
    end;
end;

procedure TMfAudioEndPoint.SetDeviceState(aValue: string);
begin
  if (aValue = DEV_STATE_ACTIVE) then
    fState := DEVICE_STATE_ACTIVE
  else if (aValue = DEV_STATE_DISABLED) then
    fState := DEVICE_STATE_DISABLED
  else if (aValue = DEV_STATE_NOTPRESENT) then
    fState := DEVICE_STATE_NOTPRESENT
  else if (aValue = DEV_STATE_UNPLUGGED) then
    fState := DEVICE_STATE_UNPLUGGED
  else fstate := DEVICE_STATEMASK_ALL;
  // Find device endpoints with given value
  OleCheck( GetEndpointDevices(fDataFlow,
                               DWord(fState),
                               FEndPointDevices,
                               dwEndPointsCount) );
end;


procedure TMfAudioEndPoint.SetEndPointDevice(aValue: DWord);
var
  hr: HResult;
begin
  if not Assigned(FEndPointDevices) then
    Exit;

  if (dwDeviceID <> aValue) then
    dwDeviceID := aValue;

  // Check boundaries
  if (dwDeviceID > dwEndPointsCount -1) then
    dwDeviceID := dwEndPointsCount -1;

  if dwEndPointsCount > 0 then
    begin
      hr := FDeviceEnumerator.GetDevice(FEndPointDevices[dwDeviceID].pwszID,
                                        fSelectedIMMDevice);
      if Succeeded(hr) then
        begin
          wsDeviceName := WideCharToString(FEndPointDevices[dwDeviceID].DeviceName);
          wsDeviceInterfaceName := WideCharToString(FEndPointDevices[dwDeviceID].DevInterfaceName);
          wsDeviceDesc := WideCharToString(FEndPointDevices[dwDeviceID].DeviceDesc);

          // We have to unregister the current endpoint, before getting a new one.
          hr := UnregisterAudioEndpointVolumeCallback(fOnEndPointNotify);
          if (hr = E_POINTER) then
            hr := S_OK;  // The first time there will be no registered Audio Endpoint Volume Callback, so we ignore the error and set hr to S_OK.

          // Remember: After unregister the callback the refcount will be decreased!
          //           See the documentation about Un/RegisterAudioEndpointVolumeCallback.
          //           So, there is no need to relase the interface, this will be done automaticly

          if Succeeded(hr) then
            begin
              hr := fSelectedIMMDevice.Activate(IID_IAudioEndpointVolume,
                                                INT(CLSCTX_INPROC_SERVER),
                                                Nil,
                                                Pointer(FAudioEndpoint));

              if Succeeded(hr) then
                begin
                  // Create and register the endpointnotifier
                  FOnEndPointNotify := TOnEndPointNotify.Create;
                  if not Assigned(FOnEndPointNotify) then
                    begin
                      hr := E_POINTER;
                      OleCheck(hr);
                      Exit;
                    end;
                  hr := RegisterAudioEndpointVolumeCallback(fOnEndPointNotify);
                end;
            end;
        end;

      if Failed(hr) then
        begin
          OleCheck(hr);
          dwDeviceID := 0;
          Exit;
        end;
    end
  else
    begin
      wsDeviceName := '?';
      wsDeviceInterfaceName := 'No Active Endpoint devices found!';
      wsDeviceDesc := '?';
      dwEndPointsCount := 0;
    end;
end;


function TMfAudioEndPoint.SupportsHardware(HardwareSupportMask: DWord = 0): Boolean;
var
  hr: HResult;
begin

  if Not Assigned(FAudioEndpoint) then
    begin
      Result := False;
      Exit;
    end;

  // Parameter HardwareSupportMask is a DWORD variable into which the method writes a hardware
  // support mask that indicates the hardware capabilities of the audio endpoint device.
  // The method can set the mask to 0 or to the bitwise-OR combination of one or more
  // ENDPOINT_HARDWARE_SUPPORT_XXX constants:
  //  | Constant                         | value	   | Description
  //  |----------------------------------|-----------|--------------------------------------------------------------
  //  | ENDPOINT_HARDWARE_SUPPORT_VOLUME | $00000001 | The audio endpoint device supports a hardware volume control.
  //  | ENDPOINT_HARDWARE_SUPPORT_MUTE   | $00000002 | The audio endpoint device supports a hardware mute control.
  //  | ENDPOINT_HARDWARE_SUPPORT_METER  | $00000004 | The audio endpoint device supports a hardware peak meter.
  if (HardwareSupportMask = 0) then
    hr := FAudioEndpoint.QueryHardwareSupport(ENDPOINT_HARDWARE_SUPPORT_VOLUME or
                                              ENDPOINT_HARDWARE_SUPPORT_MUTE or
                                              ENDPOINT_HARDWARE_SUPPORT_METER)
  else
    hr := FAudioEndpoint.QueryHardwareSupport(HardwareSupportMask);
  Result := Succeeded(hr);
end;


function TMfAudioEndPoint.VolumeStepUp(const GuidEventContext: TGuid): HResult;
var
  hr: HResult;
begin
  if Assigned(FAudioEndpoint) then
    hr := FAudioEndpoint.VolumeStepUp(@GuidEventContext)
  else
    hr := E_POINTER;
  Result := hr;
end;


function TMfAudioEndPoint.VolumeStepDown(const GuidEventContext: TGuid): HResult;
var
  hr: HResult;
begin
  if Assigned(FAudioEndpoint) then
    hr := FAudioEndpoint.VolumeStepDown(@GuidEventContext)
  else
    hr := E_POINTER;
  Result := hr;
end;



function TMfAudioEndPoint.RegisterAudioEndpointVolumeCallback(pNotify: IAudioEndpointVolumeCallback): HResult;
var
  hr: HResult;
begin
  // The RegisterControlChangeNotify method registers a client's notification callback interface.
  if Assigned(FAudioEndpoint) then
    hr := FAudioEndpoint.RegisterControlChangeNotify(pNotify)
  else
    hr := E_POINTER;
  Result := hr;
end;

function TMfAudioEndPoint.UnregisterAudioEndpointVolumeCallback(pNotify: IAudioEndpointVolumeCallback): HResult;
var
  hr: HResult;
begin
  if Assigned(FAudioEndpoint) then
    hr := FAudioEndpoint.UnregisterControlChangeNotify(pNotify)
  else
    hr := E_POINTER;
  Result := hr;
end;


// HELPERS /////////////////////////////////////////////////////////////////////

// Get deviceproperty descriptions
function TMfAudioEndPoint.GetAudioDeviceDescriptions(DefaultDevice: IMMDevice;      // index starts with 0, which is always the default endpoit device.
                                                     const DevicePkey: PROPERTYKEY; // Possible values are: PKEY_Device_FriendlyName, PKEY_Device_DeviceDesc or PKEY_DeviceInterface_FriendlyName.
                                                     out deviceDesc: WideString): HResult;
begin
  // MMDevApiUtils.GetDeviceDescriptions
  Result := GetDeviceDescriptions(DefaultDevice,
                                  DevicePkey,
                                  deviceDesc);
end;


// Get GetEndpointDevices
function TMfAudioEndPoint.GetAudioEndPoints(const flow: EDataFlow;
                                            state: eState;
                                            out endpointdevices: TEndPointDeviceArray): HResult;
begin
  // MMDevApiUtils.GetEndpointDevices
  Result := GetEndpointDevices(Flow,
                               DWord(State),
                               FEndPointDevices,
                               dwEndPointsCount);
end;


// Get a reference to the endpoint of the default communication device for
// rendering an audio stream.
function TMfAudioEndPoint.GetDefaultAudioEndPointDevice(out audioEndPoint: IMMDevice): HResult;
begin
  // MMDevApiUtils.GetDefaultEndPointAudioDevice
  Result := GetDefaultEndPointAudioDevice(audioEndPoint);
end;



//
// Callback interface implementation ///////////////////////////////////////////
//
function TOnEndPointNotify.OnNotify(pNotify: PAUDIO_VOLUME_NOTIFICATION_DATA): HRESULT;
var
  hr: HResult;
begin
  hr := S_OK;

  //TMfAudioEndPoint
  // About event-context GUID:
  // Each of the methods in the preceding list accepts an input parameter named pguidEventContext,
  // which is a pointer to an event-context GUID. Before sending notifications to clients,
  // the method copies the event-context GUID pointed to by pguidEventContext into the
  // guidEventContext member of the AUDIO_VOLUME_NOTIFICATION_DATA structure that it supplies to
  // clients through their OnNotify methods.
  // If pguidEventContext is Nil, the value of the guidEventContext member is set to GUID_NULL.
  //
  // In its implementation of the OnNotify method, a client can inspect the event-context GUID from
  // that call to discover whether it or another client is the source of the volume-change event.

  // send WM_MIXERNOTIFY message to TControl's WindProc.
  // Note: As an alternative you might use the Messages.SendStructMessage wich is introduced in Delphi 2009 (compiler version 20.0).
  //       See: https://docs.embarcadero.com/products/rad_studio/delphiAndcpp2009/HelpUpdate2/EN/html/delphivclwin32/Messages_SendStructMessage.html
  if SendMessage(hHwnd,
                 WM_MIXERNOTIFY,
                 WPARAM(S_OK),
                 LPARAM(pNotify)) > 0 then
   begin
     Dispose(pNotify);
     hr := E_POINTER;
   end;

  Result := hr;
end;

end.
