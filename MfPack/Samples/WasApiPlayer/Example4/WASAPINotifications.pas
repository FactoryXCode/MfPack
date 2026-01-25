unit WASAPINotifications;

interface

uses

  WinApi.Windows,
  WinApi.WinApiTypes,
  WinApi.CoreAudioApi.MMDeviceApi;  // IMMNotificationClient, EDataFlow, ERole

type
  // Implement this in your engine to receive notifications.
  IWasApiDeviceNotifySink = interface
    ['{C6B4A1A2-6D15-4A2B-9CC0-2F0D27B1B2D1}']
    procedure OnWasApiDefaultDeviceChanged(Flow: EDataFlow; Role: ERole; const DeviceId: UnicodeString);
    procedure OnWasApiDeviceStateChanged(const DeviceId: UnicodeString; NewState: DWORD);
    procedure OnWasApiDeviceRemoved(const DeviceId: UnicodeString);
  end;

  // COM callback object
  TWasApiNotificationClient = class(TInterfacedObject, IMMNotificationClient)
  private

    FSink: IWasApiDeviceNotifySink;
  public

    constructor Create(const ASink: IWasApiDeviceNotifySink);

    // IMMNotificationClient
    function OnDeviceStateChanged(pwstrDeviceId: LPCWSTR; dwNewState: DWORD): HRESULT; stdcall;
    function OnDeviceAdded(pwstrDeviceId: LPCWSTR): HRESULT; stdcall;
    function OnDeviceRemoved(pwstrDeviceId: LPCWSTR): HRESULT; stdcall;
    function OnDefaultDeviceChanged(flow: EDataFlow; role: ERole; pwstrDefaultDeviceId: LPCWSTR): HRESULT; stdcall;
    function OnPropertyValueChanged(pwstrDeviceId: LPCWSTR; const
                                    key: PROPERTYKEY): HRESULT; stdcall;
  end;


implementation


function WStrToUStr(P: LPCWSTR): UnicodeString;
begin

  if (P <> nil) then
    Result := P
  else
    Result := '';
end;


constructor TWasApiNotificationClient.Create(const ASink: IWasApiDeviceNotifySink);
begin
  inherited Create;
  FSink := ASink;
end;

function TWasApiNotificationClient.OnDeviceStateChanged(pwstrDeviceId: LPCWSTR;
                                                        dwNewState: DWORD): HRESULT;
var
  Id: UnicodeString;
begin
  Id := WStrToUStr(pwstrDeviceId);
  if FSink <> nil then
    FSink.OnWasApiDeviceStateChanged(Id, dwNewState);
  Result := S_OK;
end;

function TWasApiNotificationClient.OnDeviceAdded(pwstrDeviceId: LPCWSTR): HRESULT;
begin
  // optional
  Result := S_OK;
end;

function TWasApiNotificationClient.OnDeviceRemoved(pwstrDeviceId: LPCWSTR): HRESULT;
var
  Id: UnicodeString;
begin
  Id := WStrToUStr(pwstrDeviceId);
  if FSink <> nil then
    FSink.OnWasApiDeviceRemoved(Id);
  Result := S_OK;
end;

function TWasApiNotificationClient.OnDefaultDeviceChanged(flow: EDataFlow; role: ERole; pwstrDefaultDeviceId: LPCWSTR): HRESULT;
var
  Id: UnicodeString;
begin
  Id := WStrToUStr(pwstrDefaultDeviceId);
  if FSink <> nil then
    FSink.OnWasApiDefaultDeviceChanged(flow, role, Id);
  Result := S_OK;
end;

function TWasApiNotificationClient.OnPropertyValueChanged(pwstrDeviceId: LPCWSTR; const key: PROPERTYKEY): HRESULT;
begin
  // optional
  Result := S_OK;
end;

end.

