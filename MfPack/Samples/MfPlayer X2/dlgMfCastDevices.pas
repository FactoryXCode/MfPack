// FactoryX
//
// Copyright � FactoryX, Netherlands/Australia/Germany. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: dlgMfCastDevices.pas
// Kind: Pascal Unit
// Release date: 29-07-2026
// Language: ENU
//
// Revision Version: 4.0.0
// Description: ChromeCast GUI dialog.
//
//==============================================================================
unit dlgMfCastDevices;

interface

uses
  {WinApi}
  WinApi.Windows,
  WinApi.Messages,
  {System}
  System.SysUtils,
  System.Classes,
  {Vcl}
  Vcl.Controls,
  Vcl.Forms,
  Vcl.ComCtrls,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  {Cast}
  MfCast,
  MfCastTypes,
  MfCastInterfaces;

const
  WM_MFCAST_DEVICE_ADD_OR_UPDATE = WM_APP + 410;
  WM_MFCAST_DEVICE_REMOVE        = WM_APP + 411;
  WM_MFCAST_DISCOVERY_ERROR      = WM_APP + 412;

type
  PMfCastDevice = ^TMfCastDevice;
  PMfCastString = ^string;
  PMfCastErrorInfo = ^TMfCastErrorInfo;

  TCastDevicesDlg = class(TForm)
    pnlBtns: TPanel;
    btnCancel: TButton;
    chkAutoRefresh: TCheckBox;
    lvCastDevices: TListView;
    btnCast: TButton;
    btnRefresh: TButton;
    lblConnectionStatus: TEdit;

    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnRefreshClick(Sender: TObject);
    procedure btnCastClick(Sender: TObject);
    procedure lvCastDevicesSelectItem(Sender: TObject; Item: TListItem;
                                      Selected: Boolean);
    procedure lvCastDevicesDblClick(Sender: TObject);
    procedure chkAutoRefreshClick(Sender: TObject);
  private

    FCast: TMfCast;
    FPreviousDeviceAdded: TMfCastDeviceEvent;
    FPreviousDeviceUpdated: TMfCastDeviceEvent;
    FPreviousDeviceRemoved: TMfCastDeviceRemovedEvent;
    FPreviousStateChanged: TMfCastStateChangedEvent;
    FPreviousMediaStatus: TMfCastMediaStatusEvent;
    FPreviousError: TMfCastErrorEvent;
    FCallbacksInstalled: Boolean;
    FDevices: TMfCastDeviceArray;
    FSelectedDevice: TMfCastDevice;

    procedure SetCast(const AValue: TMfCast);
    procedure InstallCallbacks();
    procedure RestoreCallbacks();
    procedure ClearDevices();
    procedure ReloadDevices();
    procedure AddOrUpdateDevice(const ADevice: TMfCastDevice);
    procedure RemoveDevice(const ADeviceId: string);
    function FindDeviceIndex(const ADevice: TMfCastDevice): Integer;
    function FindDeviceIndexById(const ADeviceId: string): Integer;
    function DeviceIdentity(const ADevice: TMfCastDevice): string;
    procedure UpdateListItem(const AIndex: Integer);
    procedure UpdateSelection();
    procedure UpdateDeviceCountStatus();
    procedure SetStatus(const AText: string);
    procedure StartOrRefreshDiscovery();

    procedure ControllerDeviceAdded(const ADevice: TMfCastDevice);
    procedure ControllerDeviceUpdated(const ADevice: TMfCastDevice);
    procedure ControllerDeviceRemoved(const ADeviceId: string);
    procedure ControllerStateChanged(const AOldState,
                                     ANewState: TMfCastState);
    procedure ControllerMediaStatus(const AStatus: TMfCastMediaStatus);
    procedure ControllerError(const AError: TMfCastErrorInfo);

    procedure WmMfCastDeviceAddOrUpdate(var Msg: TMessage); message WM_MFCAST_DEVICE_ADD_OR_UPDATE;
    procedure WmMfCastDeviceRemove(var Msg: TMessage); message WM_MFCAST_DEVICE_REMOVE;
    procedure WmMfCastDiscoveryError(var Msg: TMessage); message WM_MFCAST_DISCOVERY_ERROR;

  public

    function Execute(const ACast: TMfCast;
                     out ADevice: TMfCastDevice): Boolean;

    property Cast: TMfCast read FCast write SetCast;
    property SelectedDevice: TMfCastDevice read FSelectedDevice;
  end;

var
  CastDevicesDlg: TCastDevicesDlg;

implementation

{$R *.dfm}


function TCastDevicesDlg.Execute(const ACast: TMfCast;
                                 out ADevice: TMfCastDevice): Boolean;
begin

  ADevice.Reset;
  Cast := ACast;

  Result := (ShowModal = mrOk);
  if Result then
    ADevice := FSelectedDevice;
end;

procedure TCastDevicesDlg.SetCast(const AValue: TMfCast);
begin

  RestoreCallbacks();
  FCast := AValue;
end;


procedure TCastDevicesDlg.InstallCallbacks();
begin

  if FCallbacksInstalled or not Assigned(FCast) then
    Exit;

  FPreviousDeviceAdded := FCast.OnDeviceAdded;
  FPreviousDeviceUpdated := FCast.OnDeviceUpdated;
  FPreviousDeviceRemoved := FCast.OnDeviceRemoved;
  FPreviousStateChanged := FCast.OnStateChanged;
  FPreviousMediaStatus := FCast.OnMediaStatus;
  FPreviousError := FCast.OnError;
  FCast.OnDeviceAdded := ControllerDeviceAdded;
  FCast.OnDeviceUpdated := ControllerDeviceUpdated;
  FCast.OnDeviceRemoved := ControllerDeviceRemoved;
  FCast.OnStateChanged := ControllerStateChanged;
  FCast.OnMediaStatus := ControllerMediaStatus;
  FCast.OnError := ControllerError;
  FCallbacksInstalled := True;
end;


procedure TCastDevicesDlg.RestoreCallbacks();
begin

  if FCallbacksInstalled and Assigned(FCast) then
    begin
      FCast.OnDeviceAdded := FPreviousDeviceAdded;
      FCast.OnDeviceUpdated := FPreviousDeviceUpdated;
      FCast.OnDeviceRemoved := FPreviousDeviceRemoved;
      FCast.OnStateChanged := FPreviousStateChanged;
      FCast.OnMediaStatus := FPreviousMediaStatus;
      FCast.OnError := FPreviousError;
    end;

  FCallbacksInstalled := False;
end;


procedure TCastDevicesDlg.FormShow(Sender: TObject);
begin

  FSelectedDevice.Reset;
  ClearDevices;
  InstallCallbacks;

  btnRefresh.Enabled := Assigned(FCast);
  btnCast.Enabled := False;

  if not Assigned(FCast) then
    begin
      SetStatus('ChromeCast controller is not initialized.');
      Exit;
    end;

  ReloadDevices;

  // Cached devices are already useful and must remain immediately selectable.
  // A synchronous mDNS response window is only needed for an empty list; the
  // Refresh button still performs a full scan on demand.
  if chkAutoRefresh.Checked and (lvCastDevices.Items.Count = 0) then
    StartOrRefreshDiscovery
  else
    UpdateDeviceCountStatus;
end;


procedure TCastDevicesDlg.FormClose(Sender: TObject; var Action: TCloseAction);
begin

  if Assigned(FCast) then
    FCast.StopDiscovery;

  RestoreCallbacks;
end;


procedure TCastDevicesDlg.StartOrRefreshDiscovery;
var
  hr: HRESULT;

begin

  if not Assigned(FCast) then
    Exit;

  SetStatus('Searching for ChromeCast devices...');
  hr := FCast.RefreshDiscovery;

  if FAILED(hr) then
    SetStatus('ChromeCast discovery failed. HRESULT $' + IntToHex(DWORD(hr), 8))
  else
    UpdateDeviceCountStatus;
end;


procedure TCastDevicesDlg.btnRefreshClick(Sender: TObject);
begin
  StartOrRefreshDiscovery;
end;


procedure TCastDevicesDlg.chkAutoRefreshClick(Sender: TObject);
begin

  if not Assigned(FCast) then
    Exit;

  if chkAutoRefresh.Checked then
    StartOrRefreshDiscovery
  else
    begin
      FCast.StopDiscovery;
      UpdateDeviceCountStatus;
    end;
end;


procedure TCastDevicesDlg.btnCastClick(Sender: TObject);
var
  Index: Integer;

begin

  if not Assigned(lvCastDevices.Selected) then
    Exit;

  Index := lvCastDevices.Selected.Index;
  if (Index < 0) or (Index >= Length(FDevices)) then
    Exit;

  FSelectedDevice := FDevices[Index];
  ModalResult := mrOk;
end;


procedure TCastDevicesDlg.lvCastDevicesDblClick(Sender: TObject);
begin

  if btnCast.Enabled then
    btnCastClick(btnCast);
end;


procedure TCastDevicesDlg.lvCastDevicesSelectItem(Sender: TObject;
                                                  Item: TListItem; Selected: Boolean);
begin

  UpdateSelection();
end;


procedure TCastDevicesDlg.UpdateSelection();
begin

  btnCast.Enabled := Assigned(lvCastDevices.Selected);
end;


procedure TCastDevicesDlg.ClearDevices();
begin

  SetLength(FDevices,
            0);
  lvCastDevices.Items.Clear;
  UpdateSelection;
end;


function TCastDevicesDlg.DeviceIdentity(const ADevice: TMfCastDevice): string;
begin

  Result := Trim(ADevice.Id);

  if (Result = '') then
    Result := Trim(ADevice.ServiceInstance);

  if (Result = '') then
    Result := Trim(ADevice.HostName);

  if (Result = '') then
    Result := Trim(ADevice.Address) + ':' + IntToStr(ADevice.Port);
end;


function TCastDevicesDlg.FindDeviceIndex(const ADevice: TMfCastDevice): Integer;
begin

  Result := FindDeviceIndexById(DeviceIdentity(ADevice));
end;


function TCastDevicesDlg.FindDeviceIndexById(const ADeviceId: string): Integer;
var
  I: Integer;

begin

  Result := -1;

  for I := 0 to Length(FDevices) - 1 do
    begin
      if SameText(DeviceIdentity(FDevices[I]),
                                 ADeviceId) or
         ((FDevices[I].Id <> '') and
         SameText(FDevices[I].Id,
                  ADeviceId)) then
        begin
          Result := I;
          Exit;
       end;
    end;
end;


procedure TCastDevicesDlg.UpdateListItem(const AIndex: Integer);
var
  Item: TListItem;
  DeviceName: string;
  DeviceModel: string;

begin

  if (AIndex < 0) or (AIndex >= Length(FDevices)) then
    Exit;

  if AIndex < lvCastDevices.Items.Count then
    Item := lvCastDevices.Items[AIndex]
  else
    Item := lvCastDevices.Items.Add;

  DeviceName := Trim(FDevices[AIndex].FriendlyName);

  if (DeviceName = '') then
    DeviceName := Trim(FDevices[AIndex].HostName);

  if (DeviceName = '') then
    DeviceName := Trim(FDevices[AIndex].ServiceInstance);

  if( DeviceName = '') then
    DeviceName := 'ChromeCast';

  DeviceModel := Trim(FDevices[AIndex].ModelName);
  if (DeviceModel = '') then
    DeviceModel := 'Google Cast device';

  Item.Caption := DeviceName;

  if (Item.SubItems.Count = 0) then
    Item.SubItems.Add(DeviceModel)
  else
    Item.SubItems[0] := DeviceModel;
end;


procedure TCastDevicesDlg.AddOrUpdateDevice(const ADevice: TMfCastDevice);
var
  Index: Integer;
  WasEmpty: Boolean;

begin

  WasEmpty := Length(FDevices) = 0;
  Index := FindDeviceIndex(ADevice);

  if (Index < 0) then
    begin
      Index := Length(FDevices);
      SetLength(FDevices,
                Index + 1);
    end;

  FDevices[Index] := ADevice;
  UpdateListItem(Index);

  if WasEmpty and (lvCastDevices.Items.Count > 0) then
    begin
      lvCastDevices.Items[0].Selected := True;
      lvCastDevices.Items[0].Focused := True;
    end;

  UpdateSelection();
  UpdateDeviceCountStatus();
end;


procedure TCastDevicesDlg.RemoveDevice(const ADeviceId: string);
var
  Index: Integer;
  I: Integer;

begin

  Index := FindDeviceIndexById(ADeviceId);
  if (Index < 0) then
    Exit;

  for I := Index to Length(FDevices) - 2 do
    FDevices[I] := FDevices[I + 1];

  SetLength(FDevices,
            Length(FDevices) - 1);

  if (Index < lvCastDevices.Items.Count) then
    lvCastDevices.Items.Delete(Index);

  UpdateSelection();
  UpdateDeviceCountStatus();
end;


procedure TCastDevicesDlg.ReloadDevices();
var
  hr: HRESULT;
  Devices: TMfCastDeviceArray;
  I: Integer;

begin

 if not Assigned(FCast) then
    Exit;

  hr := FCast.GetDevices(Devices);
  if FAILED(hr) then
    Exit;

  ClearDevices;
  for I := 0 to Length(Devices) - 1 do
    AddOrUpdateDevice(Devices[I]);
end;


procedure TCastDevicesDlg.UpdateDeviceCountStatus();
var
  Count: Integer;

begin

  Count := Length(FDevices);

  case Count of
    0: SetStatus('No ChromeCast devices found.');
    1: SetStatus('1 ChromeCast device found.');
  else
    SetStatus(IntToStr(Count) + ' ChromeCast devices found.');
  end;
end;


procedure TCastDevicesDlg.SetStatus(const AText: string);
begin

  lblConnectionStatus.Text := AText;
end;


procedure TCastDevicesDlg.ControllerDeviceAdded(const ADevice: TMfCastDevice);
var
  Device: PMfCastDevice;

begin

  if Assigned(FPreviousDeviceAdded) then
    FPreviousDeviceAdded(ADevice);

  if GetCurrentThreadId = MainThreadID then
    AddOrUpdateDevice(ADevice)
  else
    begin
      New(Device);
      Device^ := ADevice;

      if not PostMessage(Handle,
                         WM_MFCAST_DEVICE_ADD_OR_UPDATE,
                         0,
                         LPARAM(Device)) then
        Dispose(Device);
  end;
end;


procedure TCastDevicesDlg.ControllerDeviceUpdated(const ADevice: TMfCastDevice);
var
  Device: PMfCastDevice;

begin

  if Assigned(FPreviousDeviceUpdated) then
    FPreviousDeviceUpdated(ADevice);

  if GetCurrentThreadId = MainThreadID then
    AddOrUpdateDevice(ADevice)
  else
    begin
      New(Device);
      Device^ := ADevice;

      if not PostMessage(Handle,
                         WM_MFCAST_DEVICE_ADD_OR_UPDATE,
                         0,
                         LPARAM(Device)) then
      Dispose(Device);
  end;
end;


procedure TCastDevicesDlg.ControllerDeviceRemoved(const ADeviceId: string);
var
  DeviceId: PMfCastString;

begin

  if Assigned(FPreviousDeviceRemoved) then
    FPreviousDeviceRemoved(ADeviceId);

  if (GetCurrentThreadId = MainThreadID) then
    RemoveDevice(ADeviceId)
  else
    begin
      New(DeviceId);
      DeviceId^ := ADeviceId;

      if not PostMessage(Handle,
                         WM_MFCAST_DEVICE_REMOVE,
                         0,
                         LPARAM(DeviceId)) then
      Dispose(DeviceId);
    end;
end;


procedure TCastDevicesDlg.ControllerStateChanged(const AOldState,
                                                 ANewState: TMfCastState);
begin

  if Assigned(FPreviousStateChanged) then
    FPreviousStateChanged(AOldState, ANewState);

  if (GetCurrentThreadId <> MainThreadID) then
    Exit;

  case ANewState of
    csDiscovering: SetStatus('Searching for ChromeCast devices...');
    csIdle:        UpdateDeviceCountStatus;
  end;
end;


procedure TCastDevicesDlg.ControllerMediaStatus(const AStatus: TMfCastMediaStatus);
begin

  if Assigned(FPreviousMediaStatus) then
    FPreviousMediaStatus(AStatus);
end;


procedure TCastDevicesDlg.ControllerError(const AError: TMfCastErrorInfo);
var
  ErrorInfo: PMfCastErrorInfo;

begin

  if Assigned(FPreviousError) then
    FPreviousError(AError);

  if (GetCurrentThreadId = MainThreadID) then
    begin
      if (AError.MessageText <> '') then
        SetStatus(AError.MessageText)
      else
        SetStatus('ChromeCast error. HRESULT $' + IntToHex(DWORD(AError.HResult), 8));
    end
  else
    begin
      New(ErrorInfo);
      ErrorInfo^ := AError;

      if not PostMessage(Handle,
                         WM_MFCAST_DISCOVERY_ERROR,
                         0,
                         LPARAM(ErrorInfo)) then
        Dispose(ErrorInfo);
    end;
end;


procedure TCastDevicesDlg.WmMfCastDeviceAddOrUpdate(var Msg: TMessage);
var
  Device: PMfCastDevice;

begin

  Device := PMfCastDevice(Msg.LParam);

  if not Assigned(Device) then
    Exit;

  try
    AddOrUpdateDevice(Device^);
  finally
    Dispose(Device);
  end;
end;


procedure TCastDevicesDlg.WmMfCastDeviceRemove(var Msg: TMessage);
var
  DeviceId: PMfCastString;

begin

  DeviceId := PMfCastString(Msg.LParam);

  if not Assigned(DeviceId) then
    Exit;

  try
    RemoveDevice(DeviceId^);
  finally
    Dispose(DeviceId);
  end;
end;


procedure TCastDevicesDlg.WmMfCastDiscoveryError(var Msg: TMessage);
var
  ErrorInfo: PMfCastErrorInfo;

begin

  ErrorInfo := PMfCastErrorInfo(Msg.LParam);

  if not Assigned(ErrorInfo) then
    Exit;

  try
    if (ErrorInfo^.MessageText <> '') then
      SetStatus(ErrorInfo^.MessageText)
    else
      SetStatus('ChromeCast error. HRESULT $' + IntToHex(DWORD(ErrorInfo^.HResult), 8));
  finally
    Dispose(ErrorInfo);
  end;
end;

end.
