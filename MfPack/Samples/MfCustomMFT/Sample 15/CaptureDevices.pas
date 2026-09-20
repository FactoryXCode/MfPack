// FactoryX
//
// Copyright: (c) FactoryX. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: CaptureDevices.pas
// Kind: Pascal Unit
// Release date: 13-08-2026
// Language: ENU
//
// Version: 4.0.0
// Description: Discovers the capture devices such as microphone and camera.
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
// Remarks: Requires Windows 10 or later.
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
// Source: Microsoft Learn.
//
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
unit CaptureDevices;

interface

uses
  {WinApi}
  WinApi.Windows,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfObjects,
  WinApi.MediaFoundationApi.MfIdl;

type
  // Keeps one Delphi interface reference per enumerated capture device.
  TCaptureDeviceList = class
  private
    FSourceType: TGUID;
    FItems: array of IMFActivate;

  public
    constructor Create(const ASourceType: TGUID);

    function Refresh(): HResult;
    function Count(): Integer;
    function Name(const AIndex: Integer): string;
    function Activate(const AIndex: Integer): IMFActivate;
  end;


implementation

uses
  {WinApi}
  WinApi.ComBaseApi,
  {System}
  System.SysUtils,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfApi;


constructor TCaptureDeviceList.Create(const ASourceType: TGUID);
begin

  inherited Create();

  FSourceType := ASourceType;
end;


{$POINTERMATH ON}

function TCaptureDeviceList.Refresh(): HResult;
var
  Attributes: IMFAttributes;
  Devices: PIMFActivate;
  DeviceCount: UINT32;
  I: Integer;

begin

  SetLength(FItems,
            0);
  Devices := nil;
  DeviceCount := 0;

  Result := MFCreateAttributes(Attributes,
                               1);
  if SUCCEEDED(Result) then
    Result := Attributes.SetGUID(MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE,
                                 FSourceType);

  if SUCCEEDED(Result) then
    Result := MFEnumDeviceSources(Attributes,
                                  Devices,
                                  DeviceCount);

  if FAILED(Result) then
    Exit;

  try
    SetLength(FItems,
              DeviceCount);

    for I := 0 to Integer(DeviceCount) - 1 do
      FItems[I] := Devices[I];

  finally
    // MFEnumDeviceSources gives the caller both the interface references and
    // the CoTaskMem array. Delphi now owns its own references in FItems.
    for I := 0 to Integer(DeviceCount) - 1 do
      Devices[I] := nil;

    CoTaskMemFree(Devices);
  end;
end;


function TCaptureDeviceList.Count(): Integer;
begin

  Result := Length(FItems);
end;


function TCaptureDeviceList.Name(const AIndex: Integer): string;
var
  Text: LPWSTR;
  Length: UINT32;
  Hr: HResult;

begin

  if (AIndex < 0) or (AIndex >= Count()) then
    raise EArgumentOutOfRangeException.Create('Capture device index');

  Text := nil;
  Length := 0;
  Hr := FItems[AIndex].GetAllocatedString(MF_DEVSOURCE_ATTRIBUTE_FRIENDLY_NAME,
                                          Text,
                                          Length);
  if FAILED(Hr) then
    raise Exception.CreateFmt('Read device name failed: $%.8x',
                              [Cardinal(Hr)]);

  try
    Result := Text;
  finally
    CoTaskMemFree(Text);
  end;
end;


function TCaptureDeviceList.Activate(const AIndex: Integer): IMFActivate;
begin

  if (AIndex < 0) or (AIndex >= Count()) then
    raise EArgumentOutOfRangeException.Create('Capture device index');

  Result := FItems[AIndex];
end;

end.
