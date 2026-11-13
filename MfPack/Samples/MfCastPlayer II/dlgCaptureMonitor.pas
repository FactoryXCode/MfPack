// FactoryX
//
// Copyright (c) FactoryX, Netherlands/Australia/Germany. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: dlgCaptureMonitor.pas
// Kind: Pascal Unit
// Release date: 10-08-2026
// Language: ENU
//
// Revision Version: 4.0.2
// Description: Shows available monitors.
//
// Company: FactoryX
// Intiator(s): Tony (maXcomX), Carmen (carmenh).
// Contributor(s): Tony Kalf (maXcomX), Carmen (carmenh).
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 13/09/2026 All                 Explain unavailable DTS decoding and keep the
//                                audio-stream choice with the user.
// 24/08/2026 All                 Moby release  SDK 10.0.28000.2705  (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 or higher.
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
// Source: -
//==============================================================================
//
// LICENSE
//
// The contents of this file are subject to the Mozilla Public License
// Version 2.0 (the "License"); you may not use this file except in
// compliance with the License. You may obtain a copy of the License at
// https://mozilla.org/MPL/2.0/
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
unit dlgCaptureMonitor;

interface

uses

  {WinApi}
  WinApi.Windows,
  {Syatem}
  System.Classes,
  System.UITypes,
  {Vcl}
  Vcl.Controls,
  Vcl.Forms,
  Vcl.StdCtrls;

type

  TCaptureMonitorDlg = class(TForm)
    lblPrompt: TLabel;
    lstMonitors: TListBox;
    btnCast: TButton;
    btnCancel: TButton;

    procedure btnCastClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);

  private
    procedure CMDialogKey(var Message: TCMDialogKey); message CM_DIALOGKEY;

  end;

// DisplayConfig declarations for Delphi versions whose Windows units predate them.

  TCaptureDisplayHeader = record
    InfoType, Size: DWORD;
    AdapterId: TLuid;
    Id: DWORD;
  end;

  TCaptureDisplaySourceName = record
    Header: TCaptureDisplayHeader;
    DeviceName: array[0..31] of WideChar;
  end;

  TCaptureDisplayTargetName = record
    Header: TCaptureDisplayHeader;
    Flags, OutputTechnology: DWORD;
    ManufacturerId, ProductId: Word;
    ConnectorInstance: DWORD;
    FriendlyName: array[0..63] of WideChar;
    DevicePath: array[0..127] of WideChar;
  end;

  TCaptureDisplayPath = record
    SourceAdapterId: TLuid;
    SourceId, SourceModeIndex, SourceFlags: DWORD;
    TargetAdapterId: TLuid;
    TargetId, TargetModeIndex, OutputTechnology, Rotation, Scaling: DWORD;
    RefreshNumerator, RefreshDenominator, ScanLineOrdering: DWORD;
    TargetAvailable: BOOL;
    TargetFlags, Flags: DWORD;
  end;

  TCaptureDisplayMode = record
    // DISPLAYCONFIG_MODE_INFO is 64 bytes; only path identities are needed here.
    Data: array[0..63] of Byte;
  end;


  function CaptureMonitorName(const AMonitor: TMonitor): string;

  function GetDisplayConfigBufferSizes(Flags: DWORD;
                                       out PathCount: DWORD;
                                       out ModeCount: DWORD): Longint; stdcall;

  function QueryDisplayConfig(Flags: DWORD;
                              var PathCount: DWORD;
                              Paths: Pointer;
                              var ModeCount: DWORD;
                              Modes: Pointer;
                              TopologyId: Pointer): Longint; stdcall;

  function DisplayConfigGetDeviceInfo(var Header: TCaptureDisplayHeader): Longint; stdcall;


implementation

uses

  {WinApi}
  WinApi.MultiMon,
  {System}
  System.SysUtils;



{$R *.dfm}

const
  User32Lib    = 'user32.dll';



function DisplayFriendlyName(const ADeviceName: string): string;
const
  OnlyActivePaths = 2;

var
  Paths: array of TCaptureDisplayPath;
  Modes: array of TCaptureDisplayMode;
  Source: TCaptureDisplaySourceName;
  Target: TCaptureDisplayTargetName;
  PathCount: DWORD;
  ModeCount: DWORD;
  Status: Longint;
  I: Integer;
  Attempt: Integer;

begin

  Result := '';

  // The display topology can change between sizing and querying the buffers.
  for Attempt := 1 to 3 do
    begin
      if (GetDisplayConfigBufferSizes(OnlyActivePaths,
                                      PathCount,
                                      ModeCount) <> 0) then
        Exit;

    if (PathCount = 0) or (ModeCount = 0) then
      Exit;

    SetLength(Paths, PathCount);
    SetLength(Modes, ModeCount);

    Status := QueryDisplayConfig(OnlyActivePaths,
                                 PathCount,
                                 @Paths[0],
                                 ModeCount,
                                 @Modes[0],
                                 nil);

    if (Status <> ERROR_INSUFFICIENT_BUFFER) then
      Break;
    end;

  if Status <> 0 then
    Exit;

  for I := 0 to Integer(PathCount) - 1 do
    begin
      FillChar(Source,
               SizeOf(Source),
               0);

      Source.Header.InfoType := 1; // DISPLAYCONFIG_DEVICE_INFO_GET_SOURCE_NAME
      Source.Header.Size := SizeOf(Source);
      Source.Header.AdapterId := Paths[I].SourceAdapterId;
      Source.Header.Id := Paths[I].SourceId;

      if (DisplayConfigGetDeviceInfo(Source.Header) <> 0) then
        Continue;

      if not SameText(string(Source.DeviceName),
                      ADeviceName) then
        Continue;

      FillChar(Target,
               SizeOf(Target),
               0);

      Target.Header.InfoType := 2; // DISPLAYCONFIG_DEVICE_INFO_GET_TARGET_NAME
      Target.Header.Size := SizeOf(Target);
      Target.Header.AdapterId := Paths[I].TargetAdapterId;
      Target.Header.Id := Paths[I].TargetId;

      if (DisplayConfigGetDeviceInfo(Target.Header) = 0) then
        Result := Trim(string(Target.FriendlyName));

      if (Result <> '') then
        Exit;
    end;
end;


function CaptureMonitorName(const AMonitor: TMonitor): string;
var
  Info: TMonitorInfoEx;
  Device: TDisplayDevice;
  DeviceIndex: DWORD;
  FriendlyName: string;

begin

  Result := Format('Screen %d',
                   [AMonitor.MonitorNum + 1]);

  FillChar(Info,
           SizeOf(Info),
           0);

  Info.cbSize := SizeOf(Info);

  if not GetMonitorInfo(AMonitor.Handle,
                        PMonitorInfo(@Info)) then
    Exit;

  FriendlyName := DisplayFriendlyName(string(Info.szDevice));

  if (FriendlyName <> '') then
    begin
      Result := FriendlyName;
      Exit;
    end;

  // Look up the monitor attached to this display, rather than the GPU name.
  DeviceIndex := 0;

  repeat
    FillChar(Device,
             SizeOf(Device),
             0);

    Device.cb := SizeOf(Device);

    if not EnumDisplayDevices(@Info.szDevice[0],
                              DeviceIndex,
                              Device,
                              0) then
      Exit;

    if ((Device.StateFlags and DISPLAY_DEVICE_ATTACHED_TO_DESKTOP) <> 0) and (Trim(string(Device.DeviceString)) <> '') then
      begin
        Result := Trim(string(Device.DeviceString));
        Exit;
      end;

    Inc(DeviceIndex);

  until False;
end;



procedure TCaptureMonitorDlg.btnCastClick(Sender: TObject);
begin

  ModalResult := mrOk;
end;


procedure TCaptureMonitorDlg.btnCancelClick(Sender: TObject);
begin

  ModalResult := mrCancel;
end;


procedure TCaptureMonitorDlg.CMDialogKey(var Message: TCMDialogKey);
begin

  if (GetKeyState(VK_SHIFT) >= 0) and
     (GetKeyState(VK_CONTROL) >= 0) and
     (GetKeyState(VK_MENU) >= 0) then
    begin
      if (Message.CharCode = VK_RETURN) and btnCast.Enabled and btnCast.Visible then
        begin
          btnCast.Click();
          Message.Result := 1;
          Exit;
        end;

      if (Message.CharCode = VK_ESCAPE) and btnCancel.Enabled and btnCancel.Visible then
        begin
          btnCancel.Click();
          Message.Result := 1;
          Exit;
        end;
    end;

  inherited;
end;


// External

{$WARN SYMBOL_PLATFORM OFF}

function GetDisplayConfigBufferSizes; external User32Lib name 'GetDisplayConfigBufferSizes' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function QueryDisplayConfig; external User32Lib name 'QueryDisplayConfig' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function DisplayConfigGetDeviceInfo; external User32Lib name 'DisplayConfigGetDeviceInfo' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};

{$WARN SYMBOL_PLATFORM ON}
end.