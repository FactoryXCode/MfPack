// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - MediaFoundation
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: CamControl.pas
// Kind: Pascal Unit
// Release date: 25-08-2026
// Language: ENU
//
// Revision Version: 4.1.0
// Description: Live controls and saved camera defaults for an MF camera source.
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Carmen (carmenh)
// Contributor(s): Tony Kalf (maXcomX), Carmen (carmenh)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 15/11/2026 All                 Eels release  SDK 10.0.28000.2705 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 or higher.
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
//==============================================================================
// Source: https://learn.microsoft.com/en-us/windows/win32/api/mfidl/nn-mfidl-imfextendedcameracontrol
//
// Copyright (c) Microsoft. All rights reserved.
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
unit CamControl;

interface

uses

  {WinApi}
  WinApi.Windows,
  WinApi.Messages,
  WinApi.Ks,
  WinApi.KsMedia,
  WinApi.KsProxy,
  WinApi.WinApiTypes,
  {System}
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  {ActiveX}
  WinApi.ActiveX.ObjBase,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfObjects,
  WinApi.MediaFoundationApi.MfIdl;

const
  WM_MF_CAMERA_CONTROL_CHANGED = WM_APP + 81;
  MF_CAMERA_STANDARD_COUNT = 17;
  MF_CAMERA_EXTENDED_COUNT = 4;

type

  TMfCameraControlInfo = record
    Name: string;
    ControlSet: TGUID;
    Id: ULONG;
    Supported: Boolean;
    RangeKnown: Boolean;
    DefaultKnown: Boolean;
    Minimum: LONG;
    Maximum: LONG;
    Step: LONG;
    DefaultValue: LONG;
    Value: LONG;
    Flags: ULONG;
    Capabilities: ULONG;
    Status: HRESULT;
  end;

  TMfExtendedCameraInfo = record
    Name: string;
    Id: ULONG;
    Supported: Boolean;
    Flags: UInt64;
    Capabilities: UInt64;
    Status: HRESULT;
  end;

  TMfCameraControlNotification = class(TInterfacedObject,
                                       IMFCameraControlNotify)

  private
    FLock: TCriticalSection;
    FWindow: HWND;
    FGeneration: NativeUInt;

    procedure Post(const AStatus: HRESULT);

  public
    constructor Create(const AWindow: HWND;
                       const AGeneration: NativeUInt);
    destructor Destroy(); override;
    procedure Detach();
    procedure OnChange(const controlSet: TGUID;
                       const id: UINT32); stdcall;
    procedure OnError(hrStatus: HRESULT); stdcall;
  end;

  TMfCameraControls = class

  private
    FSource: IMFMediaSource;
    FKs: IKsControl;
    FExtended: IMFExtendedCameraController;
    FManager: IMFCameraConfigurationManager;
    FAttributes: IMFAttributes;
    FSymbolicLink: string;
    FStandard: array[0..MF_CAMERA_STANDARD_COUNT - 1] of TMfCameraControlInfo;
    FExtendedInfo: array[0..MF_CAMERA_EXTENDED_COUNT - 1] of TMfExtendedCameraInfo;
    FMonitor: IMFCameraControlMonitor;
    FNotify: IMFCameraControlNotify;
    FNotifyObject: TMfCameraControlNotification;
    FSensorModule: HMODULE;
    FGeneration: NativeUInt;
    FDefaultsStatus: HRESULT;
    FMonitorStatus: HRESULT;
    FHasStreamed: Boolean;
    FIniRestoreStatus: HRESULT;

    procedure InitializeDescriptions();
    function ReadStandard(const AIndex: Integer): HRESULT;
    function ReadRange(const AIndex: Integer): HRESULT;
    function ReadStoreRange(const AIndex: Integer): HRESULT;
    function GetStandard(const AIndex: Integer): TMfCameraControlInfo;
    function GetExtended(const AIndex: Integer): TMfExtendedCameraInfo;
    function LoadDefaults(out ACollection: IMFCameraControlDefaultsCollection): HRESULT;

  public

    constructor Create();
    destructor Destroy(); override;

    function Attach(const ASource: IMFMediaSource;
                    const ASymbolicLink: string): HRESULT;

    procedure Detach();
    procedure Refresh();

    function ApplyStandard(const AIndex: Integer;
                           const AValue: LONG;
                           const AFlags: ULONG): HRESULT;

    function ApplyExtended(const AIndex: Integer;
                           const AFlags: UInt64): HRESULT;
    function SaveIniSetting(const AIndex: Integer): HRESULT;

    function RemoveIniSetting(const AIndex: Integer): HRESULT;
    function DescribeIniSettings(const AItems: TStrings): HRESULT;
    function ApplyIniSettings(): HRESULT;
    function SaveStandardDefault(const AIndex: Integer): HRESULT;
    function RemoveStandardDefault(const AIndex: Integer): HRESULT;
    function DescribeDefaults(const AItems: TStrings): HRESULT;
    function StartMonitor(const AWindow: HWND): HRESULT;
    procedure StopMonitor();

    function FindStandard(const ASet: TGUID;
                          const AId: ULONG): Integer;

    class function IsValidManualValue(const AInfo: TMfCameraControlInfo;
                                      const AValue: LONG): Boolean; static;

    class function ParseRange(const AData: TBytes;
                              out AMinimum: LONG;
                              out AMaximum: LONG;
                              out AStep: LONG): Boolean; static;

    property Standard[const AIndex: Integer]: TMfCameraControlInfo read GetStandard;
    property Extended[const AIndex: Integer]: TMfExtendedCameraInfo read GetExtended;
    property IniRestoreStatus: HRESULT read FIniRestoreStatus;
    property DefaultsStatus: HRESULT read FDefaultsStatus;
    property MonitorStatus: HRESULT read FMonitorStatus;
    property Generation: NativeUInt read FGeneration;
    property HasStreamed: Boolean read FHasStreamed write FHasStreamed;
  end;


implementation

uses

  {WinApi}
  WinApi.WinError,
  WinApi.ComBaseApi,

  MfCameraSettings,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfApi,
  WinApi.MediaFoundationApi.MfError;

type
  TMfCreateCameraControlMonitor = function(ASymbolicLink: PWideChar;
                                           ACallback: IMFCameraControlNotify;
                                           out AMonitor: IMFCameraControlMonitor): HRESULT; stdcall;

const
  CameraSourceStream = DWORD($FFFFFFFF);

constructor TMfCameraControlNotification.Create(const AWindow: HWND;
                                                const AGeneration: NativeUInt);

begin

  inherited Create();
  FLock := TCriticalSection.Create();
  FWindow := AWindow;
  FGeneration := AGeneration;
end;


destructor TMfCameraControlNotification.Destroy();

begin

  FLock.Free();
  inherited Destroy();
end;


procedure TMfCameraControlNotification.Detach();

begin

  FLock.Acquire();
  try
    FWindow := 0;
  finally
    FLock.Release();
  end;
end;


procedure TMfCameraControlNotification.Post(const AStatus: HRESULT);

begin

  FLock.Acquire();
  try
    if (FWindow <> 0) then
      PostMessage(FWindow,
                  WM_MF_CAMERA_CONTROL_CHANGED,
                  FGeneration,
                  LPARAM(AStatus));
  finally
    FLock.Release();
  end;
end;


procedure TMfCameraControlNotification.OnChange(const controlSet: TGUID;
                                                const id: UINT32);

begin

  Post(S_OK);
end;


procedure TMfCameraControlNotification.OnError(hrStatus: HRESULT);

begin

  Post(hrStatus);
end;


constructor TMfCameraControls.Create();

begin

  inherited Create();
  InitializeDescriptions();
  FDefaultsStatus := E_NOINTERFACE;
  FMonitorStatus := E_NOINTERFACE;
end;


destructor TMfCameraControls.Destroy();

begin

  Detach();
  inherited Destroy();
end;


procedure TMfCameraControls.InitializeDescriptions();
const
  Names: array[0..MF_CAMERA_STANDARD_COUNT - 1] of string =
    ('Pan',
     'Tilt',
     'Roll',
     'Zoom',
     'Exposure',
     'Iris',
     'Focus',
     'Brightness',
     'Contrast',
     'Hue',
     'Saturation',
     'Sharpness',
     'Gamma',
     'Color enable',
     'White balance',
     'Backlight compensation',
     'Gain');
var
  I: Integer;

begin

  for I := 0 to High(FStandard) do
    begin
      FStandard[I] := Default(TMfCameraControlInfo);
      FStandard[I].Name := Names[I];
      FStandard[I].Status := E_NOINTERFACE;
      if (I < 7) then
        begin
          FStandard[I].ControlSet := PROPSETID_VIDCAP_CAMERACONTROL;
          FStandard[I].Id := I;
        end
      else
        begin
          FStandard[I].ControlSet := PROPSETID_VIDCAP_VIDEOPROCAMP;
          FStandard[I].Id := I - 7;
        end;
    end;
  FExtendedInfo[0].Name := 'Video HDR';
  FExtendedInfo[0].Id := ULONG(KSPROPERTY_CAMERACONTROL_EXTENDED_VIDEOHDR);
  FExtendedInfo[1].Name := 'Optical image stabilization';
  FExtendedInfo[1].Id := ULONG(KSPROPERTY_CAMERACONTROL_EXTENDED_OIS);
  FExtendedInfo[2].Name := 'Eye gaze correction';
  FExtendedInfo[2].Id := ULONG(KSPROPERTY_CAMERACONTROL_EXTENDED_EYEGAZECORRECTION);
  FExtendedInfo[3].Name := 'Background segmentation';
  FExtendedInfo[3].Id := ULONG(KSPROPERTY_CAMERACONTROL_EXTENDED_BACKGROUNDSEGMENTATION);
  for I := 0 to High(FExtendedInfo) do
    begin
      FExtendedInfo[I].Supported := False;
      FExtendedInfo[I].Status := E_NOINTERFACE;
      FExtendedInfo[I].Flags := 0;
      FExtendedInfo[I].Capabilities := 0;
    end;
end;


function TMfCameraControls.Attach(const ASource: IMFMediaSource;
                                  const ASymbolicLink: string): HRESULT;

begin

  Detach();
  if not Assigned(ASource) then
    Exit(E_POINTER);
  FSource := ASource;
  FSymbolicLink := ASymbolicLink;
  // These interfaces come from the MF media source. No DirectShow graph or
  // IAMCameraControl/IAMVideoProcAmp interface is used by this sample.
  if FAILED(FSource.QueryInterface(IID_IKsControl, FKs)) then
    MFGetService(FSource,
                 GUID_NULL,
                 IID_IKsControl,
                 Pointer(FKs));
  MFGetService(FSource,
               GUID_NULL,
               IID_IMFExtendedCameraController,
               Pointer(FExtended));
  FDefaultsStatus := MFCreateAttributes(FAttributes,
                                        1);
  if SUCCEEDED(FDefaultsStatus) then
    FDefaultsStatus := FAttributes.SetString(MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE_VIDCAP_SYMBOLIC_LINK,
                                             PWideChar(FSymbolicLink));
  if SUCCEEDED(FDefaultsStatus) then
    FDefaultsStatus := CoCreateInstance(CLSID_CameraConfigurationManager,
                                        nil,
                                        CLSCTX_INPROC_SERVER,
                                        IID_IMFCameraConfigurationManager,
                                        FManager);
  Refresh();
  FIniRestoreStatus := S_OK;
  try
    if TMfCameraSettings.UsesIni(TMfCameraSettings.FileName()) then
      FIniRestoreStatus := ApplyIniSettings();
  except
    on E: Exception do FIniRestoreStatus := E_FAIL;
  end;
  // An unsupported optional control API must not prevent camera preview.
  Result := S_OK;
end;


procedure TMfCameraControls.Detach();

begin

  StopMonitor();
  Inc(FGeneration);
  if Assigned(FManager) then
    FManager.Shutdown();
  FManager := nil;
  FAttributes := nil;
  FExtended := nil;
  FKs := nil;
  FSource := nil;
  FSymbolicLink := '';
  FHasStreamed := False;
  FDefaultsStatus := E_NOINTERFACE;
  InitializeDescriptions();
end;


class function TMfCameraControls.ParseRange(const AData: TBytes;
                                            out AMinimum: LONG;
                                            out AMaximum: LONG;
                                            out AStep: LONG): Boolean;
var
  Description: KSPROPERTY_DESCRIPTION;
  Members: KSPROPERTY_MEMBERSHEADER;
  Stepping: KSPROPERTY_STEPPING_LONG;
  Bounds: KSPROPERTY_BOUNDS_LONG;
  Offset: NativeUInt;
  DataSize: NativeUInt;
  I: ULONG;

begin

  Result := False;
  AMinimum := 0;
  AMaximum := 0;
  AStep := 0;
  if (Length(AData) < SizeOf(Description)) then
    Exit;
  Move(AData[0],
       Description,
       SizeOf(Description));
  DataSize := Length(AData);
  if (Description.DescriptionSize > DataSize) or
     (Description.DescriptionSize < SizeOf(Description)) or
     (Description.MembersListCount = 0) then
    Exit;
  DataSize := Description.DescriptionSize;
  Offset := SizeOf(Description);
  for I := 0 to Description.MembersListCount - 1 do
    begin
      if (Offset > DataSize) or (DataSize - Offset < SizeOf(Members)) then
        Exit;
      Move(AData[Offset],
           Members,
           SizeOf(Members));
      Inc(Offset,
          SizeOf(Members));
      if (Members.MembersSize = 0) or
         (UInt64(Members.MembersSize) * Members.MembersCount > DataSize - Offset) then
        Exit;
      if (Members.MembersCount > 0) then
        begin
          if (Members.MembersFlags = KSPROPERTY_MEMBER_STEPPEDRANGES) and
             (Members.MembersSize >= SizeOf(Stepping)) then
            begin
              Move(AData[Offset],
                   Stepping,
                   SizeOf(Stepping));
              if (Stepping.SteppingDelta = 0) or (Stepping.SteppingDelta > MAXLONG) then
                Exit;
              AMinimum := Stepping.Bounds.SignedMinimum;
              AMaximum := Stepping.Bounds.SignedMaximum;
              AStep := Stepping.SteppingDelta;
              Exit(AMinimum <= AMaximum);
            end;
          if (Members.MembersFlags = KSPROPERTY_MEMBER_RANGES) and
             (Members.MembersSize >= SizeOf(Bounds)) then
            begin
              Move(AData[Offset],
                   Bounds,
                   SizeOf(Bounds));
              AMinimum := Bounds.SignedMinimum;
              AMaximum := Bounds.SignedMaximum;
              AStep := 1;
              Exit(AMinimum <= AMaximum);
            end;
        end;
      Inc(Offset,
          NativeUInt(Members.MembersSize) * Members.MembersCount);
    end;
end;


function TMfCameraControls.ReadRange(const AIndex: Integer): HRESULT;
var
  Property_: KSPROPERTY;
  Description: KSPROPERTY_DESCRIPTION;
  Returned: ULONG;
  Buffer: TBytes;

begin

  FillChar(Property_,
           SizeOf(Property_),
           0);
  Property_.Set_ := FStandard[AIndex].ControlSet;
  Property_.Id := FStandard[AIndex].Id;
  Property_.Flags := KSPROPERTY_TYPE_BASICSUPPORT;
  FillChar(Description,
           SizeOf(Description),
           0);
  Returned := 0;
  Result := FKs.KsProperty(@Property_,
                           SizeOf(Property_),
                           @Description,
                           SizeOf(Description),
                           Returned);
  // Drivers may return MORE_DATA while filling in the descriptor size.
  if (Description.DescriptionSize < SizeOf(Description)) or
     (Description.DescriptionSize > 1024 * 1024) then
    Exit(MF_E_NOT_FOUND);
  SetLength(Buffer,
            Description.DescriptionSize);
  Result := FKs.KsProperty(@Property_,
                           SizeOf(Property_),
                           @Buffer[0],
                           Length(Buffer),
                           Returned);
  if FAILED(Result) then
    Exit;
  if (Returned > ULONG(Length(Buffer))) then
    Exit(E_UNEXPECTED);
  SetLength(Buffer,
            Returned);
  FStandard[AIndex].RangeKnown := ParseRange(Buffer,
                                             FStandard[AIndex].Minimum,
                                             FStandard[AIndex].Maximum,
                                             FStandard[AIndex].Step);
  if not FStandard[AIndex].RangeKnown then
    Result := MF_E_NOT_FOUND;
end;


function TMfCameraControls.LoadDefaults(out ACollection: IMFCameraControlDefaultsCollection): HRESULT;

begin

  ACollection := nil;
  if not Assigned(FManager) then
    Exit(FDefaultsStatus);
  Result := FManager.LoadDefaults(FAttributes,
                                  ACollection);
end;


function TMfCameraControls.ReadStoreRange(const AIndex: Integer): HRESULT;
var
  Collection: IMFCameraControlDefaultsCollection;
  Control: IMFCameraControlDefaults;
  Range: MF_CAMERA_CONTROL_RANGE_INFO;

begin

  Result := LoadDefaults(Collection);
  if SUCCEEDED(Result) then
    Result := Collection.GetOrAddControl(MF_CAMERA_CONTROL_CONFIGURATION_TYPE_POSTSTART,
                                         FStandard[AIndex].ControlSet,
                                         FStandard[AIndex].Id,
                                         SizeOf(KSPROPERTY_CAMERACONTROL_S),
                                         SizeOf(KSPROPERTY_CAMERACONTROL_S),
                                         Control);
  if SUCCEEDED(Result) then
    Result := Control.GetRangeInfo(Range);
  if SUCCEEDED(Result) then
    begin
      if (Range.minValue > Range.maxValue) or (Range.stepValue <= 0) then
        Exit(E_UNEXPECTED);
      FStandard[AIndex].Minimum := Range.minValue;
      FStandard[AIndex].Maximum := Range.maxValue;
      FStandard[AIndex].Step := Range.stepValue;
      FStandard[AIndex].DefaultValue := Range.defaultValue;
      FStandard[AIndex].RangeKnown := True;
      FStandard[AIndex].DefaultKnown := True;
    end;
  // GetOrAddControl changes only this in-memory collection. Nothing is saved.
end;


function TMfCameraControls.ReadStandard(const AIndex: Integer): HRESULT;
var
  Request: KSPROPERTY_CAMERACONTROL_S;
  Reply: KSPROPERTY_CAMERACONTROL_S;
  Returned: ULONG;

begin

  // Camera-control and video-proc-amp packets have identical binary fields;
  // their property-set GUID selects the meaning of the control ID.
  FStandard[AIndex].Supported := False;
  FStandard[AIndex].RangeKnown := False;
  FStandard[AIndex].DefaultKnown := False;
  if not Assigned(FKs) then
    Exit(E_NOINTERFACE);
  FillChar(Request,
           SizeOf(Request),
           0);
  Request.Property_.Set_ := FStandard[AIndex].ControlSet;
  Request.Property_.Id := FStandard[AIndex].Id;
  Request.Property_.Flags := KSPROPERTY_TYPE_GET;
  Reply := Request;
  Returned := 0;
  Result := FKs.KsProperty(@Request,
                           SizeOf(Request),
                           @Reply,
                           SizeOf(Reply),
                           Returned);
  if FAILED(Result) then
    Exit;
  if (Returned < SizeOf(Reply)) then
    Exit(E_UNEXPECTED);
  FStandard[AIndex].Supported := True;
  FStandard[AIndex].Value := Reply.Value;
  FStandard[AIndex].Flags := Reply.Flags;
  FStandard[AIndex].Capabilities := Reply.Capabilities;
  ReadRange(AIndex);
  ReadStoreRange(AIndex);
end;


procedure TMfCameraControls.Refresh();
var
  I: Integer;
  Control: IMFExtendedCameraControl;

begin

  for I := 0 to High(FStandard) do
    FStandard[I].Status := ReadStandard(I);
  for I := 0 to High(FExtendedInfo) do
    begin
      FExtendedInfo[I].Supported := False;
      FExtendedInfo[I].Status := E_NOINTERFACE;
      if Assigned(FExtended) then
        begin
          Control := nil;
          FExtendedInfo[I].Status := FExtended.GetExtendedCameraControl(CameraSourceStream,
                                                                        FExtendedInfo[I].Id,
                                                                        Control);
          if SUCCEEDED(FExtendedInfo[I].Status) then
            begin
              FExtendedInfo[I].Supported := True;
              FExtendedInfo[I].Flags := Control.GetFlags();
              FExtendedInfo[I].Capabilities := Control.GetCapabilities();
            end;
        end;
    end;
end;


function TMfCameraControls.GetStandard(const AIndex: Integer): TMfCameraControlInfo;

begin

  if (AIndex < 0) or (AIndex >= MF_CAMERA_STANDARD_COUNT) then
    raise ERangeError.Create('Camera control index is out of range.');
  Result := FStandard[AIndex];
end;


function TMfCameraControls.GetExtended(const AIndex: Integer): TMfExtendedCameraInfo;

begin

  if (AIndex < 0) or (AIndex >= MF_CAMERA_EXTENDED_COUNT) then
    raise ERangeError.Create('Extended camera control index is out of range.');
  Result := FExtendedInfo[AIndex];
end;


function TMfCameraControls.FindStandard(const ASet: TGUID;
                                        const AId: ULONG): Integer;
var
  I: Integer;

begin

  for I := 0 to High(FStandard) do
    if IsEqualGUID(FStandard[I].ControlSet, ASet) and (FStandard[I].Id = AId) then
      Exit(I);
  Result := -1;
end;


class function TMfCameraControls.IsValidManualValue(const AInfo: TMfCameraControlInfo;
                                                    const AValue: LONG): Boolean;

begin

  Result := AInfo.Supported and AInfo.RangeKnown and (AInfo.Step > 0) and
            (AValue >= AInfo.Minimum) and (AValue <= AInfo.Maximum);
  if Result then
    Result := ((Int64(AValue) - AInfo.Minimum) mod AInfo.Step = 0);
end;


function TMfCameraControls.ApplyStandard(const AIndex: Integer;
                                         const AValue: LONG;
                                         const AFlags: ULONG): HRESULT;
var
  Request: KSPROPERTY_CAMERACONTROL_S;
  Reply: KSPROPERTY_CAMERACONTROL_S;
  Returned: ULONG;
  Info: TMfCameraControlInfo;

begin

  if (AIndex < 0) or (AIndex >= MF_CAMERA_STANDARD_COUNT) then
    Exit(E_INVALIDARG);
  Info := Standard[AIndex];
  if not Assigned(FKs) or (not Info.Supported) then
    Exit(E_NOINTERFACE);
  if (AFlags <> KSPROPERTY_CAMERACONTROL_FLAGS_AUTO) and
     (AFlags <> KSPROPERTY_CAMERACONTROL_FLAGS_MANUAL) then
    Exit(E_INVALIDARG);
  if ((Info.Capabilities and AFlags) = 0) then
    Exit(E_INVALIDARG);
  if (AFlags = KSPROPERTY_CAMERACONTROL_FLAGS_MANUAL) and
     (not IsValidManualValue(Info,
                             AValue)) then
    Exit(E_INVALIDARG);
  FillChar(Request,
           SizeOf(Request),
           0);
  Request.Property_.Set_ := Info.ControlSet;
  Request.Property_.Id := Info.Id;
  Request.Property_.Flags := KSPROPERTY_TYPE_SET;
  Request.Value := AValue;
  Request.Flags := AFlags;
  Reply := Request;
  Result := FKs.KsProperty(@Request,
                           SizeOf(Request),
                           @Reply,
                           SizeOf(Reply),
                           Returned);
  if SUCCEEDED(Result) then
    FStandard[AIndex].Status := ReadStandard(AIndex);
end;


function TMfCameraControls.ApplyExtended(const AIndex: Integer;
                                         const AFlags: UInt64): HRESULT;
var
  Info: TMfExtendedCameraInfo;
  Control: IMFExtendedCameraControl;

begin

  if (AIndex < 0) or (AIndex >= MF_CAMERA_EXTENDED_COUNT) then
    Exit(E_INVALIDARG);
  Info := Extended[AIndex];
  if not Assigned(FExtended) or (not Info.Supported) then
    Exit(E_NOINTERFACE);
  // This example exposes known flag-only modes, never arbitrary payload edits.
  if (AFlags <> 0) and (AFlags <> 1) and (AFlags <> 2) then
    Exit(E_INVALIDARG);
  if ((Info.Capabilities and AFlags) <> AFlags) then
    Exit(E_INVALIDARG);
  // HDR can require sensor reprogramming before the first captured frame.
  if (AIndex = 0) and FHasStreamed then
    Exit(MF_E_INVALIDREQUEST);
  Result := FExtended.GetExtendedCameraControl(CameraSourceStream,
                                               Info.Id,
                                               Control);
  if SUCCEEDED(Result) then
    Result := Control.SetFlags(AFlags);
  if SUCCEEDED(Result) then
    Result := Control.CommitSettings();
  if SUCCEEDED(Result) then
    Refresh();
end;



function TMfCameraControls.SaveIniSetting(const AIndex: Integer): HRESULT;
var
  Info: TMfCameraControlInfo;
begin
  if (AIndex < 0) or (AIndex >= MF_CAMERA_STANDARD_COUNT) then
    Exit(E_INVALIDARG);
  if FSymbolicLink = '' then Exit(MF_E_INVALIDREQUEST);
  Result := ReadStandard(AIndex);
  if FAILED(Result) then Exit;
  Info := Standard[AIndex];
  if (Info.Flags = KSPROPERTY_CAMERACONTROL_FLAGS_MANUAL) and
     not IsValidManualValue(Info, Info.Value) then Exit(E_INVALIDARG);
  try
    Result := TMfCameraSettings.Write(TMfCameraSettings.FileName(), FSymbolicLink,
                                      Info.ControlSet, Info.Id, Info.Value, Info.Flags);
  except
    on E: Exception do Result := E_FAIL;
  end;
end;


function TMfCameraControls.RemoveIniSetting(const AIndex: Integer): HRESULT;
var
  Info: TMfCameraControlInfo;
begin
  if (AIndex < 0) or (AIndex >= MF_CAMERA_STANDARD_COUNT) then
    Exit(E_INVALIDARG);
  if FSymbolicLink = '' then Exit(MF_E_INVALIDREQUEST);
  Info := Standard[AIndex];
  try
    Result := TMfCameraSettings.Remove(TMfCameraSettings.FileName(), FSymbolicLink,
                                       Info.ControlSet, Info.Id);
  except
    on E: Exception do Result := E_FAIL;
  end;
end;


function TMfCameraControls.DescribeIniSettings(const AItems: TStrings): HRESULT;
var
  I: Integer;
  Value: Longint;
  Flags: Cardinal;
  Status: HRESULT;
  Info: TMfCameraControlInfo;
begin
  if not Assigned(AItems) then Exit(E_POINTER);
  AItems.Clear();
  if FSymbolicLink = '' then Exit(MF_E_INVALIDREQUEST);
  Result := S_OK;
  try
    for I := 0 to MF_CAMERA_STANDARD_COUNT - 1 do
      begin
        Info := Standard[I];
        Status := TMfCameraSettings.Read(TMfCameraSettings.FileName(), FSymbolicLink,
                                         Info.ControlSet, Info.Id, Value, Flags);
        if Status = S_OK then
          AItems.Add(Format('%s: %d  Flags: $%.8x', [Info.Name, Value, Flags]))
        else if FAILED(Status) then
          begin
            AItems.Add(Format('%s: invalid/unreadable INI setting ($%.8x)',
                              [Info.Name, Cardinal(Status)]));
            if SUCCEEDED(Result) then Result := Status;
          end;
      end;
  except
    on E: Exception do Result := E_FAIL;
  end;
end;


function TMfCameraControls.ApplyIniSettings(): HRESULT;
var
  I: Integer;
  Value: Longint;
  Flags: Cardinal;
  Status: HRESULT;
  Info: TMfCameraControlInfo;
begin
  if FSymbolicLink = '' then Exit(MF_E_INVALIDREQUEST);
  Result := S_OK;
  try
    for I := 0 to MF_CAMERA_STANDARD_COUNT - 1 do
      begin
        Info := Standard[I];
        Status := TMfCameraSettings.Read(TMfCameraSettings.FileName(), FSymbolicLink,
                                         Info.ControlSet, Info.Id, Value, Flags);
        // Every loaded value goes through the same capability/range checks as live edits.
        // Keep applying other controls if one entry is corrupt or no longer supported.
        if Status = S_OK then Status := ApplyStandard(I, Value, Flags);
        if FAILED(Status) and SUCCEEDED(Result) then Result := Status;
      end;
  except
    on E: Exception do Result := E_FAIL;
  end;
  FIniRestoreStatus := Result;
end;


function TMfCameraControls.SaveStandardDefault(const AIndex: Integer): HRESULT;
var
  Collection: IMFCameraControlDefaultsCollection;
  Control: IMFCameraControlDefaults;
  Info: TMfCameraControlInfo;
  ControlBuffer: Pointer;
  DataBuffer: Pointer;
  ControlSize: ULONG;
  DataSize: ULONG;
  UnlockStatus: HRESULT;

begin

  if (AIndex < 0) or (AIndex >= MF_CAMERA_STANDARD_COUNT) then
    Exit(E_INVALIDARG);
  FStandard[AIndex].Status := ReadStandard(AIndex);
  Info := Standard[AIndex];
  if not Info.Supported then
    Exit(Info.Status);
  if (Info.Flags <> KSPROPERTY_CAMERACONTROL_FLAGS_AUTO) and
     (Info.Flags <> KSPROPERTY_CAMERACONTROL_FLAGS_MANUAL) then
    Exit(E_INVALIDARG);
  if (Info.Flags = KSPROPERTY_CAMERACONTROL_FLAGS_MANUAL) and
     (not IsValidManualValue(Info,
                             Info.Value)) then
    Exit(E_INVALIDARG);
  Result := LoadDefaults(Collection);
  if SUCCEEDED(Result) then
    Result := Collection.GetOrAddControl(MF_CAMERA_CONTROL_CONFIGURATION_TYPE_POSTSTART,
                                         Info.ControlSet,
                                         Info.Id,
                                         SizeOf(KSPROPERTY_CAMERACONTROL_S),
                                         SizeOf(KSPROPERTY_CAMERACONTROL_S),
                                         Control);
  if FAILED(Result) then
    Exit;
  Result := Control.LockControlData(ControlBuffer,
                                    ControlSize,
                                    DataBuffer,
                                    DataSize);
  if FAILED(Result) then
    Exit;
  try
    if (ControlSize < SizeOf(KSPROPERTY_CAMERACONTROL_S)) or
       (DataSize < SizeOf(KSPROPERTY_CAMERACONTROL_S)) or
       (ControlBuffer = nil) or (DataBuffer = nil) then
      Result := E_UNEXPECTED
    else
      begin
        PKSPROPERTY_CAMERACONTROL_S(ControlBuffer).Property_.Set_ := Info.ControlSet;
        PKSPROPERTY_CAMERACONTROL_S(ControlBuffer).Property_.Id := Info.Id;
        PKSPROPERTY_CAMERACONTROL_S(ControlBuffer).Property_.Flags := KSPROPERTY_TYPE_SET;
        PKSPROPERTY_CAMERACONTROL_S(ControlBuffer).Value := Info.Value;
        PKSPROPERTY_CAMERACONTROL_S(ControlBuffer).Flags := Info.Flags;
        Move(ControlBuffer^,
             DataBuffer^,
             SizeOf(KSPROPERTY_CAMERACONTROL_S));
      end;
  finally
    UnlockStatus := Control.UnlockControlData();
  end;
  if SUCCEEDED(Result) then
    Result := UnlockStatus;
  // LoadDefaults was fresh; unrelated defaults remain in the collection.
  if SUCCEEDED(Result) then
    Result := FManager.SaveDefaults(Collection);
end;


function TMfCameraControls.RemoveStandardDefault(const AIndex: Integer): HRESULT;
var
  Collection: IMFCameraControlDefaultsCollection;
  Info: TMfCameraControlInfo;

begin

  if (AIndex < 0) or (AIndex >= MF_CAMERA_STANDARD_COUNT) then
    Exit(E_INVALIDARG);
  Info := Standard[AIndex];
  Result := LoadDefaults(Collection);
  if SUCCEEDED(Result) then
    Result := Collection.RemoveControl(Info.ControlSet,
                                       Info.Id);
  if SUCCEEDED(Result) then
    Result := FManager.SaveDefaults(Collection);
end;


function TMfCameraControls.DescribeDefaults(const AItems: TStrings): HRESULT;
var
  Collection: IMFCameraControlDefaultsCollection;
  Control: IMFCameraControlDefaults;
  ControlBuffer: Pointer;
  DataBuffer: Pointer;
  ControlSize: ULONG;
  DataSize: ULONG;
  I: ULONG;
  Count: ULONG;
  Index: Integer;
  Name: string;
  UnlockStatus: HRESULT;

begin

  AItems.Clear();
  Result := LoadDefaults(Collection);
  if FAILED(Result) then
    Exit;
  Count := Collection.GetControlCount();
  if (Count = 0) then
    Exit(S_OK);
  for I := 0 to Count - 1 do
    begin
      Result := Collection.GetControl(I,
                                      Control);
      if FAILED(Result) then
        Exit;
      Result := Control.LockControlData(ControlBuffer,
                                        ControlSize,
                                        DataBuffer,
                                        DataSize);
      if FAILED(Result) then
        Exit;
      try
        if (ControlBuffer = nil) or (ControlSize < SizeOf(KSPROPERTY)) then
          Result := E_UNEXPECTED
        else
          begin
            Index := FindStandard(PKSPROPERTY(ControlBuffer).Set_,
                                  PKSPROPERTY(ControlBuffer).Id);
            if (Index >= 0) then
              Name := FStandard[Index].Name
            else
              Name := GUIDToString(PKSPROPERTY(ControlBuffer).Set_) +
                      Format(' / %d',
                             [PKSPROPERTY(ControlBuffer).Id]);
            if (Index >= 0) and (DataBuffer <> nil) and
               (DataSize >= SizeOf(KSPROPERTY_CAMERACONTROL_S)) then
              Name := Name + Format(': value %d, flags $%.8x',
                                    [PKSPROPERTY_CAMERACONTROL_S(DataBuffer).Value,
                                       PKSPROPERTY_CAMERACONTROL_S(DataBuffer).Flags]);
            if (Control.GetType() = MF_CAMERA_CONTROL_CONFIGURATION_TYPE_PRESTART) then
              Name := Name + ' (before streaming)'
            else
              Name := Name + ' (after streaming)';
            AItems.Add(Name);
          end;
      finally
        UnlockStatus := Control.UnlockControlData();
      end;
      if FAILED(Result) then
        Exit;
      if FAILED(UnlockStatus) then
        Exit(UnlockStatus);
    end;
end;


function TMfCameraControls.StartMonitor(const AWindow: HWND): HRESULT;
var
  CreateMonitor: TMfCreateCameraControlMonitor;
  I: Integer;

begin

  StopMonitor();
  if (AWindow = 0) or (FSymbolicLink = '') then
    Exit(E_INVALIDARG);
  // Load at runtime so older Windows versions can still use supported live KS controls.
  FSensorModule := LoadLibraryEx('mfsensorgroup.dll',
                                 0,
                                 $00000800); // System32 only.
  if (FSensorModule = 0) then
    Exit(E_NOINTERFACE);
  CreateMonitor := TMfCreateCameraControlMonitor(GetProcAddress(FSensorModule,
                                                                'MFCreateCameraControlMonitor'));
  if not Assigned(CreateMonitor) then
    begin
      StopMonitor();
      Exit(E_NOINTERFACE);
    end;
  FNotifyObject := TMfCameraControlNotification.Create(AWindow,
                                                       FGeneration);
  FNotify := FNotifyObject;
  Result := CreateMonitor(PWideChar(FSymbolicLink),
                          FNotify,
                          FMonitor);
  if SUCCEEDED(Result) then
    begin
      for I := 0 to High(FStandard) do
        if FStandard[I].Supported then
          FMonitor.AddControlSubscription(FStandard[I].ControlSet,
                                          FStandard[I].Id);
      for I := 0 to High(FExtendedInfo) do
        if FExtendedInfo[I].Supported then
          FMonitor.AddControlSubscription(KSPROPERTYSETID_ExtendedCameraControl,
                                          FExtendedInfo[I].Id);
      Result := FMonitor.Start();
    end;
  if FAILED(Result) then
    StopMonitor();
  FMonitorStatus := Result;
end;


procedure TMfCameraControls.StopMonitor();

begin

  if Assigned(FNotifyObject) then
    FNotifyObject.Detach();
  if Assigned(FMonitor) then
    FMonitor.Shutdown();
  FMonitor := nil;
  FNotifyObject := nil;
  FNotify := nil;
  if (FSensorModule <> 0) then
    FreeLibrary(FSensorModule);
  FSensorModule := 0;
  FMonitorStatus := E_NOINTERFACE;
end;

end.
