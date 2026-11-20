// FactoryX
//
// Copyright: � FactoryX. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: MfAudioEndPoint.pas
// Kind: Pascal Unit
// Release date: 13-08-2020
// Language: ENU
//
// Revision Version: 4.1.0
// Description: Component to manage capture or render endpoints and properties.
//              It also provides an audio endpoint callback.
//
// Company: FactoryX
// Intiator(s): Tony (maXcomX).
// Contributor(s): Tony Kalf (maXcomX), Carmen (carmenh).
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 15/11/2026 All                 Eels release  SDK 10.0.28000.2705 (Windows 11)
// 12/06/2024 Tony                Removed EDataFlowEx.
// 16/07/2025 Tony                updated some code and fixed some issues.
// 05/02/2026 Carmen/Tony         Rewritten: thread-safe, events (no messages), IMMNotificationClient + RefreshDefaultDevice.
//------------------------------------------------------------------------------
//
// Remarks: - Requires Windows 10 or higher.
//          - OnNotify event still uses PAUDIO_VOLUME_NOTIFICATION_DATA. In the new code the pointer is a COPY,
//            valid only during the event call (main thread). Do NOT store it.
//
// Related objects: -
// Related projects: MfPackX410
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
unit MfAudioEndPoint;

interface

uses

  {WinApi}
  WinApi.Windows,
  WinApi.Messages,
  WinApi.WinApiTypes,
  WinApi.WinError,
  WinApi.ComBaseApi,
  {System}
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  System.Types,
  System.Win.ComObj,
  {ActiveX}
  WinApi.ActiveX.PropSys,
  WinApi.ActiveX.PropIdl,
  WinApi.ActiveX.ObjBase,
  {CoreAudioApi}
  WinApi.CoreAudioApi.FunctionDiscoveryKeys_devpkey,
  WinApi.CoreAudioApi.MMDeviceApi,
  WinApi.CoreAudioApi.MMDevApiUtils,
  WinApi.CoreAudioApi.Endpointvolume;

type

  // Callback method for endpoint-volume-change notifications from IAudioEndpointVolumeCallback.
  // NOTE: pNotify points to a COPY, valid only during this event call.
  TOnNotify = procedure (Sender: TObject;
                         pNotify: PAUDIO_VOLUME_NOTIFICATION_DATA) of object;


  TMfAudioEndPoint = class; // forward

  // Volume callback COM interface
  TOnEndPointNotify = class(TInterfacedObject, IAudioEndpointVolumeCallback)
  private

    FOwner: TMfAudioEndPoint;
    FCallbackLock: TCriticalSection;
  public

    constructor Create(AOwner: TMfAudioEndPoint);
    destructor Destroy; override;

    function OnNotify(pNotify: PAUDIO_VOLUME_NOTIFICATION_DATA): HRESULT; stdcall;
    procedure DetachOwner; inline;
  end;


  // Device notification COM interface (default-device switching etc.)
  TOnDeviceNotify = class(TInterfacedObject, IMMNotificationClient)
  private

    FOwner: TMfAudioEndPoint;
    FCallbackLock: TCriticalSection;
    function QueueRefresh(Msg: UINT;Flow: WPARAM;Role: LPARAM): HRESULT;

  public

    constructor Create(AOwner: TMfAudioEndPoint);
    destructor Destroy(); override;

    procedure DetachOwner; inline;
    function OnDeviceStateChanged(pwstrDeviceId: LPCWSTR;
                                  dwNewState: DWord): HResult; stdcall;
    function OnDeviceAdded(pwstrDeviceId: LPCWSTR): HResult; stdcall;
    function OnDeviceRemoved(pwstrDeviceId: LPCWSTR): HResult; stdcall;
    function OnDefaultDeviceChanged(flow: EDataFlow;
                                    role: ERole;
                                    pwstrDefaultDeviceId: PWideChar): HResult; stdcall;
    function OnPropertyValueChanged(pwstrDeviceId: LPCWSTR;
                                    const key: PROPERTYKEY): HResult; stdcall;
  end;


  TMfAudioEndPoint = class(TComponent)
  private

    FLock: TCriticalSection;
    FNotificationWindow: HWND;

    dwDeviceIndex: DWord;  // Device ID (index into Devices[])
    uiChannels: UINT;   // Total number of channels (last notify snapshot)
    dwEndPointsCount: DWord;

    wsDeviceName: string;
    wsDeviceInterfaceName: string;
    wsDeviceDesc: string;
    FDeviceID: string;  // format {0.0.0.00000000}.{ef4f5772-aeac-426a-8d69-a6bcf7153472}

    fEndPointDevices: TEndPointDeviceArray;  // Cached endpoints list (MfPack record)

    fSelectedIMMDevice: IMMDevice;
    fDeviceEnumerator: IMMDeviceEnumerator;
    fAudioEndpoint: IAudioEndpointVolumeEx;

    fOnEndPointNotify: TOnEndPointNotify;
    fOnDeviceNotify: IMMNotificationClient;
    FOnNotify: TOnNotify;

    FguidEventContext: TGuid;

    fDataFlow: EDataFlow;
    fRole: ERole;
    fState: eState;

    FFollowDefaultDevice: Boolean;
    FShuttingDown: Integer; // 0=running, 1=shutting down (guards callbacks)

    // Keep object refs for DetachOwner; interface refs own lifetime (refcount)
    fOnDeviceNotifyObj: TOnDeviceNotify;
    fOnEndPointNotifyIntf: IAudioEndpointVolumeCallback;

    procedure CreateNotificationWindow;
    procedure DeliverNotification(Msg: UINT;WParam: WPARAM;LParam: LPARAM);
    procedure ShutdownCallbacks;

    function GetGuidContextAsString(): string;
    function GetChannels(): UINT;

    function GetMute(): Boolean;
    procedure SetMute(aValue: Boolean);

    function GetMasterScalarVolume(): Single;
    procedure SetMasterScalarVolume(aValue: Single);

    function GetMasterDbVolume(): Single;
    procedure SetMasterDbVolume(aValue: Single);

    procedure SetDataFlow(aValue: EDataFlow);
    procedure SetDeviceState(aValue: string);
    procedure SetEndPointDevice(aValue: DWord);
    procedure SetDeviceID(aValue: string);
    procedure DoVolumeNotifyOnMainThread(pNotifyCopy: PAUDIO_VOLUME_NOTIFICATION_DATA);

    procedure RebuildEndpointCache();
    function FindDeviceIndexByIdStr(const DeviceId: WideString): Integer;

  public

    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;

    // Ensure callbacks are unregistered and owners detached before memory is freed.
    procedure BeforeDestruction(); override;

    // NEW: Sync this control to the current Windows default endpoint for (DeviceDataFlow, DeviceRole)
    function GetDefaultDevice(): HResult;

    // Bind to a device by IMMDevice endpoint ID string (as returned by IMMDevice.GetId).
    // This avoids any dependency on friendly names / descriptions.
    function BindToDeviceId(const ADeviceId: string): HResult;

    // Existing methods (do not rename)
    procedure GetVolumeRange(out pflVolumeMindB: Single;
                             out pflVolumeMaxdB: Single;
                             out pflVolumeIncrementdB: Single);

    function GetChannelScalarVolume(Index: UINT): Single;
    procedure SetChannelScalarVolume(Index: UINT; chVolume: Single);

    function GetStateAsString(): string;
    function SupportsHardware(HardwareSupportMask: DWord = 0): Boolean;
    function VolumeStepUp(const GuidEventContext: TGuid): HResult;
    function VolumeStepDown(const GuidEventContext: TGuid): HResult;
    function RegisterAudioEndpointVolumeCallback(pNotify: IAudioEndpointVolumeCallback): HResult;
    function UnregisterAudioEndpointVolumeCallback(pNotify: IAudioEndpointVolumeCallback): HResult;

    function GetAudioDeviceDescriptions(DefaultDevice: IMMDevice;
                                        const DevicePkey: PROPERTYKEY;
                                        out deviceDesc: WideString): HResult;

    function GetAudioEndPoints(const flow: EDataFlow;
                               state: eState;
                               out endpointdevices: TEndPointDeviceArray): HResult;

    function GetDefaultAudioEndPointDevice(out audioEndPoint: IMMDevice): HResult;

    // Non visual properties
    property IMMDeviceInterface: IMMDevice read fSelectedIMMDevice;
    property ChannelVolume[_Index: UINT]: Single read GetChannelScalarVolume write SetChannelScalarVolume;
    property Devices: TEndPointDeviceArray read fEndPointDevices;
    property EndPointsCount: DWord read dwEndPointsCount;

  published

    // Read-only properties
    property DeviceName: string read wsDeviceName;
    property DeviceInterfaceName: string read wsDeviceInterfaceName;
    property DeviceDescription: string read wsDeviceDesc;
    property DeviceGuidContext: string read GetGuidContextAsString;

    property DeviceRole: ERole read fRole;
    property State: eState read fState default DEVICE_STATE_ACTIVE;
    property Channels: UINT read GetChannels;

    // read/write properties
    property DeviceID: string read FDeviceID write SetDeviceID;

    property DeviceIndex: DWord read dwDeviceIndex write SetEndPointDevice default 0;
    property DeviceDataFlow: EDataFlow read fDataFlow write SetDataFlow default eRender;
    property DeviceState: string read GetStateAsString write SetDeviceState;

    property MasterScalarVolume: Single read GetMasterScalarVolume write SetMasterScalarVolume stored False;
    property MasterDbVolume: Single read GetMasterDbVolume write SetMasterDbVolume stored False;
    // Mute is live endpoint state, not form configuration. Use Delphi's native
    // Boolean for DFM RTTI and never persist the current device state in a DFM.
    property Mute: Boolean read GetMute write SetMute stored False default False;

    // NEW (opt-in): automatically follow Windows default device changes (speakers <-> headphones etc.)
    property FollowDefaultDevice: Boolean read FFollowDefaultDevice write FFollowDefaultDevice default False;

    property OnNotify: TOnNotify read FOnNotify write FOnNotify;
  end;

procedure Register;


implementation


procedure Register;
begin

  RegisterComponents('MfPack Core Audio Samples',
                     [TMfAudioEndPoint]);
end;


function _CopyNotifyStruct(pNotify: PAUDIO_VOLUME_NOTIFICATION_DATA): PAUDIO_VOLUME_NOTIFICATION_DATA;
var
  cb: NativeUInt;
  Channels: Cardinal;

begin

  Result := nil;

  if (pNotify = nil) then
    Exit;

  Channels := pNotify^.nChannels;
  if (Channels = 0) or (Channels > 64) then
    Exit;

  cb := SizeOf(AUDIO_VOLUME_NOTIFICATION_DATA) + NativeUInt(Channels - 1) * SizeOf(Single);

  GetMem(Result,
         cb);

  if (Result = nil) then
    Exit;

  try

    Move(pNotify^,
         Result^,
         cb);
  except

    if (Result <> nil) then
      begin

        FreeMem(Result);
        Result := nil;
      end;
  end;
end;


{ TOnEndPointNotify }

const
  WM_MFPACK_VOLUME = WM_APP + 721;
  WM_MFPACK_REFRESH = WM_APP + 722;
  WM_MFPACK_DEFAULT = WM_APP + 723;
  EndpointWindowClass: PWideChar = 'MfPack.EndpointNotifications.4.1';

function EndpointGetWindowData(Wnd: HWND;Index: Integer): NativeInt; stdcall;
  external 'user32.dll' name {$IFDEF WIN64}'GetWindowLongPtrW'{$ELSE}'GetWindowLongW'{$ENDIF};
function EndpointSetWindowData(Wnd: HWND;Index: Integer;Value: NativeInt): NativeInt; stdcall;
  external 'user32.dll' name {$IFDEF WIN64}'SetWindowLongPtrW'{$ELSE}'SetWindowLongW'{$ENDIF};

function EndpointWindowProc(Wnd: HWND;Msg: UINT;WParam: WPARAM;LParam: LPARAM): LRESULT; stdcall;
var
  Owner: TMfAudioEndPoint;
begin
  if Msg = WM_NCCREATE then
    // CREATESTRUCTW begins with the lpCreateParams pointer on both Windows targets.
    EndpointSetWindowData(Wnd,GWLP_USERDATA,NativeInt(PPointer(LParam)^));
  Owner := TMfAudioEndPoint(EndpointGetWindowData(Wnd,GWLP_USERDATA));
  if (Owner <> nil) and (Msg >= WM_MFPACK_VOLUME) and (Msg <= WM_MFPACK_DEFAULT) then
    begin
      // Exceptions must not cross the native window-procedure boundary.
      try
        Owner.DeliverNotification(Msg,WParam,LParam);
      except
      end;
      Exit(0);
    end;
  if Msg = WM_NCDESTROY then
    EndpointSetWindowData(Wnd,GWLP_USERDATA,0);
  Result := DefWindowProcW(Wnd,Msg,WParam,LParam);
end;


procedure TMfAudioEndPoint.CreateNotificationWindow;
var
  WindowClass: TWndClassW;
begin
  FillChar(WindowClass,SizeOf(WindowClass),0);
  WindowClass.lpfnWndProc := @EndpointWindowProc;
  WindowClass.hInstance := GetModuleHandleW(nil);
  WindowClass.lpszClassName := EndpointWindowClass;
  if (RegisterClassW(WindowClass) = 0) and (GetLastError <> ERROR_CLASS_ALREADY_EXISTS) then
    RaiseLastOSError;
  FNotificationWindow := CreateWindowExW(0,EndpointWindowClass,nil,0,0,0,0,0,0,0,GetModuleHandleW(nil),Self);
  if FNotificationWindow = 0 then
    RaiseLastOSError;
end;


procedure TMfAudioEndPoint.DeliverNotification(Msg: UINT;WParam: WPARAM;LParam: LPARAM);
begin
  if Msg = WM_MFPACK_VOLUME then
    begin
      try
        if FShuttingDown = 0 then
          DoVolumeNotifyOnMainThread(PAUDIO_VOLUME_NOTIFICATION_DATA(LParam));
      finally
        FreeMem(Pointer(LParam));
      end;
    end
  else if FShuttingDown = 0 then
    begin
      if Msg = WM_MFPACK_REFRESH then
        begin
          RebuildEndpointCache;
          if FFollowDefaultDevice then
            GetDefaultDevice;
        end
      else if FFollowDefaultDevice and (Integer(WParam) = Ord(fDataFlow)) and
              (Integer(LParam) = Ord(fRole)) then
        GetDefaultDevice;
    end;
end;


constructor TOnEndPointNotify.Create(AOwner: TMfAudioEndPoint);
begin
  inherited Create;
  FCallbackLock := TCriticalSection.Create;
  FOwner := AOwner;
end;


destructor TOnEndPointNotify.Destroy;
begin
  FCallbackLock.Free;
  inherited Destroy;
end;


procedure TOnEndPointNotify.DetachOwner;
begin
  FCallbackLock.Enter;
  try
    FOwner := nil;
  finally
    FCallbackLock.Leave;
  end;
end;


function TOnEndPointNotify.OnNotify(pNotify: PAUDIO_VOLUME_NOTIFICATION_DATA): HRESULT;
var
  CopyPtr: PAUDIO_VOLUME_NOTIFICATION_DATA;
begin
  Result := S_OK;
  FCallbackLock.Enter;
  try
    if (FOwner = nil) or (pNotify = nil) or (FOwner.FShuttingDown <> 0) then
      Exit;
    if (pNotify^.nChannels = 0) or (pNotify^.nChannels > 64) then
      Exit(E_INVALIDARG);
    try
      CopyPtr := _CopyNotifyStruct(pNotify);
    except
      Exit(E_OUTOFMEMORY);
    end;
    if CopyPtr = nil then
      Exit(E_OUTOFMEMORY);
    if not PostMessage(FOwner.FNotificationWindow,WM_MFPACK_VOLUME,0,LPARAM(CopyPtr)) then
      begin
        Result := HRESULT_FROM_WIN32(GetLastError);
        FreeMem(CopyPtr);
      end;
  finally
    FCallbackLock.Leave;
  end;
end;


constructor TOnDeviceNotify.Create(AOwner: TMfAudioEndPoint);
begin
  inherited Create;
  FCallbackLock := TCriticalSection.Create;
  FOwner := AOwner;
end;


destructor TOnDeviceNotify.Destroy;
begin
  FCallbackLock.Free;
  inherited Destroy;
end;


procedure TOnDeviceNotify.DetachOwner;
begin
  FCallbackLock.Enter;
  try
    FOwner := nil;
  finally
    FCallbackLock.Leave;
  end;
end;


function TOnDeviceNotify.QueueRefresh(Msg: UINT;Flow: WPARAM;Role: LPARAM): HRESULT;
begin
  Result := S_OK;
  FCallbackLock.Enter;
  try
    if (FOwner <> nil) and (FOwner.FShuttingDown = 0) then
      if not PostMessage(FOwner.FNotificationWindow,Msg,Flow,Role) then
        Result := HRESULT_FROM_WIN32(GetLastError);
  finally
    FCallbackLock.Leave;
  end;
end;


function TOnDeviceNotify.OnDeviceStateChanged(pwstrDeviceId: LPCWSTR;
                                             dwNewState: DWORD): HRESULT;
begin
  Result := QueueRefresh(WM_MFPACK_REFRESH,0,0);
end;


function TOnDeviceNotify.OnDeviceAdded(pwstrDeviceId: LPCWSTR): HRESULT;
begin
  Result := QueueRefresh(WM_MFPACK_REFRESH,0,0);
end;


function TOnDeviceNotify.OnDeviceRemoved(pwstrDeviceId: LPCWSTR): HRESULT;
begin
  Result := QueueRefresh(WM_MFPACK_REFRESH,0,0);
end;


function TOnDeviceNotify.OnDefaultDeviceChanged(flow: EDataFlow;
                                               role: ERole;
                                               pwstrDefaultDeviceId: PWideChar): HRESULT;
begin
  Result := QueueRefresh(WM_MFPACK_DEFAULT,WPARAM(Ord(flow)),LPARAM(Ord(role)));
end;


function TOnDeviceNotify.OnPropertyValueChanged(pwstrDeviceId: LPCWSTR;
                                               const key: PROPERTYKEY): HRESULT;
begin
  Result := QueueRefresh(WM_MFPACK_REFRESH,0,0);
end;


{ TMfAudioEndPoint }

constructor TMfAudioEndPoint.Create(AOwner: TComponent);
var
  hr: HResult;

begin

  inherited Create(AOwner);

  FLock := TCriticalSection.Create;
  CreateNotificationWindow;

  // Defaults
  fRole := eMultimedia;
  fDataFlow := eRender;
  fState := DEVICE_STATE_ACTIVE;
  dwDeviceIndex := 0;
  uiChannels := 0;
  dwEndPointsCount := 0;
  FFollowDefaultDevice := False;

  OleCheck(CoCreateGuid(FguidEventContext));

  // IMMDeviceEnumerator
  hr := CoCreateInstance(CLSID_MMDeviceEnumerator,
                            nil,
                            INT(CLSCTX_INPROC_SERVER),
                            IID_IMMDeviceEnumerator,
                            fDeviceEnumerator);
  OleCheck(hr);

  // Register device notifications.
  fOnDeviceNotifyObj := TOnDeviceNotify.Create(Self);
  fOnDeviceNotify := fOnDeviceNotifyObj as IMMNotificationClient;
  OleCheck(fDeviceEnumerator.RegisterEndpointNotificationCallback(fOnDeviceNotify));

  // Internal endpoint volume callback.
  fOnEndPointNotify := TOnEndPointNotify.Create(Self);

  // The COM interface owns callback lifetime, including in-flight notifications.
  fOnEndPointNotifyIntf := fOnEndPointNotify as IAudioEndpointVolumeCallback;

  // Build cache and bind selected device index.
  RebuildEndpointCache();

  if (dwEndPointsCount > 0) then
    SetEndPointDevice(dwDeviceIndex)
  else
    begin

      wsDeviceName := '?';
      wsDeviceInterfaceName := 'No Active Endpoint devices found!';
      wsDeviceDesc := '?';
    end;
end;


procedure TMfAudioEndPoint.ShutdownCallbacks;
begin

  if InterlockedExchange(FShuttingDown,1) <> 0 then
    Exit;

  // Make sure we do not try to auto-follow during teardown.
  FFollowDefaultDevice := False;

  // Unregister device notifications (never raise from teardown).
  try
    if Assigned(fDeviceEnumerator) and Assigned(fOnDeviceNotify) then
      fDeviceEnumerator.UnregisterEndpointNotificationCallback(fOnDeviceNotify);
  except
  end;

  // Unregister endpoint volume callback.
  try
    if Assigned(fAudioEndpoint) and Assigned(fOnEndPointNotifyIntf) then
      fAudioEndpoint.UnregisterControlChangeNotify(fOnEndPointNotifyIntf);
  except
  end;

  // Detach owners so any queued callbacks become no-ops.
  if Assigned(fOnEndPointNotify) then
    fOnEndPointNotify.DetachOwner;

  if Assigned(fOnDeviceNotifyObj) then
    fOnDeviceNotifyObj.DetachOwner;

  // Release interface refs (refcounted lifetime).
  fOnEndPointNotifyIntf := nil;
  fOnDeviceNotify := nil;

end;


procedure TMfAudioEndPoint.BeforeDestruction();
begin
  ShutdownCallbacks;
  inherited BeforeDestruction;
end;


destructor TMfAudioEndPoint.Destroy();
var
  Msg: TMsg;
begin

  try
    ShutdownCallbacks;
    if FNotificationWindow <> 0 then
      begin
        // Detach serializes with posting. No producer can enqueue after this drain.
        while PeekMessage(Msg,FNotificationWindow,WM_MFPACK_VOLUME,WM_MFPACK_VOLUME,PM_REMOVE) do
          FreeMem(Pointer(Msg.lParam));
        DestroyWindow(FNotificationWindow);
        FNotificationWindow := 0;
      end;

    fAudioEndpoint := nil;
    fSelectedIMMDevice := nil;
    fDeviceEnumerator := nil;

    fOnDeviceNotifyObj := nil;
    fOnEndPointNotify := nil;
    ClearEndpointDevices(fEndPointDevices);
  finally

    FreeAndNil(FLock);
    inherited Destroy;
  end;
end;


procedure TMfAudioEndPoint.DoVolumeNotifyOnMainThread(pNotifyCopy: PAUDIO_VOLUME_NOTIFICATION_DATA);
begin

  if (pNotifyCopy = nil) then
    Exit;

  FLock.Enter();

  try

    uiChannels := pNotifyCopy^.nChannels;
  finally

    FLock.Leave();
  end;

  if Assigned(FOnNotify) then
    FOnNotify(Self,
              pNotifyCopy);
end;


procedure TMfAudioEndPoint.RebuildEndpointCache;
begin

  ClearEndpointDevices(fEndPointDevices);
  dwEndPointsCount := 0;
  OleCheck(GetEndpointDevices(fDataFlow,
                             DWord(fState),
                             fEndPointDevices,
                             dwEndPointsCount));
end;


function TMfAudioEndPoint.FindDeviceIndexByIdStr(const DeviceId: WideString): Integer;
var
  i: Integer;

begin

  Result := -1;
  for i := 0 to Integer(dwEndPointsCount) - 1 do
    if (fEndPointDevices[i].pwszID <> nil) and (lstrcmpiW(fEndPointDevices[i].pwszID,PWideChar(DeviceId)) = 0) then
      Exit(i);
end;


function TMfAudioEndPoint.BindToDeviceId(const ADeviceId: string): HResult;
var
  idx: Integer;

begin

  Result := E_INVALIDARG;
  if ADeviceId = '' then
    Exit;

  // Ensure cache is current.
  RebuildEndpointCache();
  idx := FindDeviceIndexByIdStr(WideString(ADeviceId));
  if (idx < 0) then
    Exit(E_NOTFOUND);

  SetEndPointDevice(DWord(idx));
  Result := S_OK;
end;


function TMfAudioEndPoint.GetDefaultDevice: HResult;
var
  Dev: IMMDevice;
  pId: PWideChar;
  IdStr: WideString;
  Idx: Integer;

begin

  Result := GetDefaultEndPointAudioDevice(Dev,
                                          FRole,
                                          FDataFlow);
  if Failed(Result) or (Dev = nil) then
    Exit;

  pId := nil;
  Result := Dev.GetId(pId);
  if Failed(Result) then
    Exit;

  try

    IdStr := WideString(pId);
  finally

    CoTaskMemFree(pId);
  end;

  // Ensure cache is current and find index
  RebuildEndpointCache();
  Idx := FindDeviceIndexByIdStr(IdStr);

  if (Idx >= 0) then
    begin

      // Bind to the default device by index (keeps old semantics consistent)
      SetEndPointDevice(DWord(Idx));
      Result := S_OK;
    end
  else
    Result := E_NOTFOUND;
end;


function TMfAudioEndPoint.GetGuidContextAsString: string;
begin

  Result := UnicodeString(GuidToString(FguidEventContext));
end;


function TMfAudioEndPoint.GetChannels: UINT;
var
  cc: UINT;

begin

  cc := 0;
  if Assigned(fAudioEndpoint) then
    fAudioEndpoint.GetChannelCount(cc);

  if (cc = 0) then
  begin

    FLock.Enter();

    try

      cc := uiChannels;
    finally

      FLock.Leave();
    end;
  end;

  Result := cc;
end;


function TMfAudioEndPoint.GetMute: Boolean;
var
  b: INT; // MfPack BOOL workaround

begin

  Result := False;

  if not Assigned(fAudioEndpoint) then
    Exit;

  b := 0;
  if Succeeded(fAudioEndpoint.GetMute(b)) then
    Result := b <> 0;
end;


procedure TMfAudioEndPoint.SetMute(aValue: Boolean);
begin

  if (csLoading in ComponentState) or (csDesigning in ComponentState) then
    Exit;
  if Assigned(fAudioEndpoint) then
    OleCheck(fAudioEndpoint.SetMute(Ord(aValue),@FguidEventContext));
end;


function TMfAudioEndPoint.GetMasterScalarVolume(): Single;
var
  v: Single;

begin

  v := 0.0;
  if Assigned(fAudioEndpoint) then
    fAudioEndpoint.GetMasterVolumeLevelScalar(v);
  Result := v;
end;


procedure TMfAudioEndPoint.SetMasterScalarVolume(aValue: Single);
begin

  if (csLoading in ComponentState) or (csDesigning in ComponentState) then
    Exit;
  if Assigned(fAudioEndpoint) then
    OleCheck(fAudioEndpoint.SetMasterVolumeLevelScalar(aValue,
                                              @FguidEventContext));
end;


function TMfAudioEndPoint.GetMasterDbVolume(): Single;
var
  v: Single;

begin

  v := 0.0;
  if Assigned(fAudioEndpoint) then
    fAudioEndpoint.GetMasterVolumeLevel(v);
  Result := v;
end;


procedure TMfAudioEndPoint.SetMasterDbVolume(aValue: Single);
begin

  if (csLoading in ComponentState) or (csDesigning in ComponentState) then
    Exit;
  if Assigned(fAudioEndpoint) then
    OleCheck(fAudioEndpoint.SetMasterVolumeLevel(aValue,
                                        @FguidEventContext));
end;


procedure TMfAudioEndPoint.GetVolumeRange(out pflVolumeMindB,
                                          pflVolumeMaxdB,
                                          pflVolumeIncrementdB: Single);
begin

  pflVolumeMindB := 0;
  pflVolumeMaxdB := 0;
  pflVolumeIncrementdB := 0;
  if Assigned(fAudioEndpoint) then
    fAudioEndpoint.GetVolumeRange(pflVolumeMindB,
                                  pflVolumeMaxdB,
                                  pflVolumeIncrementdB);
end;


function TMfAudioEndPoint.GetChannelScalarVolume(Index: UINT): Single;
var
  snLevel: Single;

begin

  snLevel := 0.0;
  if Assigned(fAudioEndpoint) then
    fAudioEndpoint.GetChannelVolumeLevelScalar(Index,
                                               snLevel);
  Result := snLevel;
end;


procedure TMfAudioEndPoint.SetChannelScalarVolume(Index: UINT; chVolume: Single);
begin
  if Assigned(fAudioEndpoint) then
    fAudioEndpoint.SetChannelVolumeLevelScalar(Index,
                                               chVolume,
                                               @FguidEventContext);
end;


function TMfAudioEndPoint.GetStateAsString: string;
begin

  Result := GetDeviceStateAsString(DWord(fState));
end;


procedure TMfAudioEndPoint.SetDeviceState(aValue: string);
var
  S1: string;

begin

  S1 := Trim(aValue);

  if (lstrcmpiW(PWideChar(S1),PWideChar(UnicodeString(DEV_STATE_ACTIVE))) = 0) then
    fState := DEVICE_STATE_ACTIVE
  else
    if (lstrcmpiW(PWideChar(S1),PWideChar(UnicodeString(DEV_STATE_DISABLED))) = 0) then
      fState := DEVICE_STATE_DISABLED
  else
    if (lstrcmpiW(PWideChar(S1),PWideChar(UnicodeString(DEV_STATE_NOTPRESENT))) = 0) then
      fState := DEVICE_STATE_NOTPRESENT
  else
    if (lstrcmpiW(PWideChar(S1),PWideChar(UnicodeString(DEV_STATE_UNPLUGGED))) = 0) then
      fState := DEVICE_STATE_UNPLUGGED
  else
    if (lstrcmpiW(PWideChar(S1),PWideChar(UnicodeString(DEV_STATEMASK_ALL))) = 0) then
    fState := DEVICE_STATEMASK_ALL
  else
    fState := DEVICE_STATE_ACTIVE;

  RebuildEndpointCache();
  if (dwEndPointsCount > 0) then
    SetEndPointDevice(dwDeviceIndex);
end;


procedure TMfAudioEndPoint.SetDataFlow(aValue: EDataFlow);
begin

  if (fDataFlow <> aValue) then
    begin

      fDataFlow := aValue;
      RebuildEndpointCache();
      if (dwEndPointsCount > 0) then
        SetEndPointDevice(dwDeviceIndex);
    end;
end;


procedure TMfAudioEndPoint.SetDeviceID(aValue: string);
var
  hr: HResult;
  s: string;

begin

  s := Trim(aValue);

  // Store (always) so the property reflects the last requested value.
  FDeviceID := s;

  // Empty means: do not change selection here (DeviceIndex remains the selector).
  if (s = '') then
    Exit;

  // If enumerator is not ready yet, we cannot bind right now.
  if not Assigned(fDeviceEnumerator) then
    Exit;

  hr := BindToDeviceId(s);

  // If binding fails, keep the previous active endpoint (do not raise).
  // The caller can still check (e.g. by reading DeviceName / EndPointsCount) after Refresh.
  if Failed(hr) then
    Exit;
end;


procedure TMfAudioEndPoint.SetEndPointDevice(aValue: DWord);
var
  hr: HResult;
  imDevice: IMMDevice;
  epVolume: IAudioEndpointVolumeEx;

begin

  if not Assigned(fDeviceEnumerator) then
    Exit;

  dwDeviceIndex := aValue;

  if (dwEndPointsCount > 0) and (dwDeviceIndex > dwEndPointsCount - 1) then
    dwDeviceIndex := dwEndPointsCount - 1;

  if (dwEndPointsCount = 0) then
    Exit;

  hr := fDeviceEnumerator.GetDevice(fEndPointDevices[dwDeviceIndex].pwszID,
                                    imDevice);
  OleCheck(hr);

  // Unregister old callback
  if Assigned(fAudioEndpoint) and Assigned(fOnEndPointNotify) then
    fAudioEndpoint.UnregisterControlChangeNotify(fOnEndPointNotifyIntf);

  hr := imDevice.Activate(IID_IAudioEndpointVolumeEx,
                          CLSCTX_INPROC_SERVER,
                          nil,
                          Pointer(epVolume));
  OleCheck(hr);

  // Update published strings (from cache)
  wsDeviceName := WideCharToString(fEndPointDevices[dwDeviceIndex].DeviceName);
  wsDeviceInterfaceName := WideCharToString(fEndPointDevices[dwDeviceIndex].DevInterfaceName);
  wsDeviceDesc := WideCharToString(fEndPointDevices[dwDeviceIndex].DeviceDesc);
  FDeviceID := WideCharToString(fEndPointDevices[dwDeviceIndex].pwszID);

  // Swap refs (thread-safe)
  FLock.Enter();

  try

    fSelectedIMMDevice := imDevice;
    fAudioEndpoint := epVolume;
  finally

    FLock.Leave();
  end;

  // Register callback again.
  if Assigned(fAudioEndpoint) and Assigned(fOnEndPointNotify) then
    OleCheck(fAudioEndpoint.RegisterControlChangeNotify(fOnEndPointNotifyIntf));
end;


function TMfAudioEndPoint.SupportsHardware(HardwareSupportMask: DWord): Boolean;
var
  MaskOut: DWORD;

begin

  Result := False;
  if not Assigned(fAudioEndpoint) then
    Exit;

  MaskOut := 0;
  if Failed(fAudioEndpoint.QueryHardwareSupport(MaskOut)) then
    Exit(False);

  if (HardwareSupportMask = 0) then
    Result := (MaskOut <> 0)
  else
    Result := ((MaskOut and HardwareSupportMask) = HardwareSupportMask);
end;


function TMfAudioEndPoint.VolumeStepUp(const GuidEventContext: TGuid): HResult;
begin

  if not Assigned(fAudioEndpoint) then
    Exit(E_POINTER);
  Result := fAudioEndpoint.VolumeStepUp(@GuidEventContext);
end;


function TMfAudioEndPoint.VolumeStepDown(const GuidEventContext: TGuid): HResult;
begin

  if not Assigned(fAudioEndpoint) then
    Exit(E_POINTER);
  Result := fAudioEndpoint.VolumeStepDown(@GuidEventContext);
end;


function TMfAudioEndPoint.RegisterAudioEndpointVolumeCallback(pNotify: IAudioEndpointVolumeCallback): HResult;
begin
  if (pNotify = nil) or not Assigned(fAudioEndpoint) then
    Exit(E_POINTER);
  Result := fAudioEndpoint.RegisterControlChangeNotify(pNotify);
end;


function TMfAudioEndPoint.UnregisterAudioEndpointVolumeCallback(pNotify: IAudioEndpointVolumeCallback): HResult;
begin

  if (pNotify = nil) or not Assigned(fAudioEndpoint) then
    Exit(E_POINTER);
  Result := fAudioEndpoint.UnregisterControlChangeNotify(pNotify);
end;


function TMfAudioEndPoint.GetAudioDeviceDescriptions(DefaultDevice: IMMDevice;
                                                     const DevicePkey: PROPERTYKEY;
                                                     out deviceDesc: WideString): HResult;
begin

  Result := GetDeviceDescriptions(DefaultDevice,
                                  DevicePkey,
                                  deviceDesc);
end;


function TMfAudioEndPoint.GetAudioEndPoints(const flow: EDataFlow;
                                            state: eState;
                                            out endpointdevices: TEndPointDeviceArray): HResult;
var
  cnt: DWord;

begin

  cnt := 0;
  Result := GetEndpointDevices(flow,
                               state,
                               endpointdevices,
                               cnt);
  // Caller owns this independent array; never alias its task-allocated names.
end;


function TMfAudioEndPoint.GetDefaultAudioEndPointDevice(out audioEndPoint: IMMDevice): HResult;
begin

  Result := GetDefaultEndPointAudioDevice(audioEndPoint,
                                          fRole,
                                          fDataFlow);
end;

initialization
  RegisterClass(TMfAudioEndPoint);

finalization
  UnregisterClassW(EndpointWindowClass,GetModuleHandleW(nil));

end.
