// FactoryX
//
// Copyright (c) FactoryX. All rights reserved.
//
// Project: Media Foundation - MFPack - Cast
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: MfCastCaptureCapabilities.pas
// Kind: Delphi file
// Release date: 29-07-2026
// Language: ENU
//
// Revision Version: 4.0.1
// Description: MfPack Cast V2.1 capture codec capability negotiation.
//
// Intiator(s): Tony (maXcomX), Carmen (carmenh)
// Contributor(s): Tony Kalf (maXcomX), Carmen (carmenh).
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 24/08/2026 All                 Moby release  SDK 10.0.28000.2705  (Windows 11)ws 11)
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
// Groupname: FactoryX
// The Initial Developers of the Original Code are: Tony Kalf (maXcomX)
//                                                  Peter Larson (ozships)
// Contributor(s): Tony Kalf (maXcomX), Peter Larson (ozships)
//
// Non commercial users may distribute this source code provided that this
// header is included this source code provided that this header is included
// in full at the top of the file.
// Commercial users are not allowed to include this API as part of their
// product without implicit permission.
//
//==============================================================================
unit MfCastCaptureCapabilities;

interface

uses
  WinApi.Windows,
  MfCastTypes;

function MfCastGetCaptureCapabilities(
  const ADevice: TMfCastDevice;
  const APreference: TMfCastVideoCodecPreference;
  out ACapabilities: TMfCastCaptureCapabilities): HRESULT;

function MfCastReceiverSupportsHEVC(const ADevice: TMfCastDevice): Boolean;

implementation

uses
  WinApi.ActiveX,
  WinApi.WinError,
  System.SysUtils,
  WinApi.MediaFoundationApi.MfApi,
  WinApi.MediaFoundationApi.MfObjects,
  WinApi.MediaFoundationApi.MfTransform;

function ContainsText(const AValue: string;
                      const APart: string): Boolean;
begin
  Result := Pos(UpperCase(APart), UpperCase(AValue)) > 0;
end;


function MfCastReceiverSupportsHEVC(const ADevice: TMfCastDevice): Boolean;
var
  Model: string;

begin
  Model := Trim(ADevice.ModelName);

  // Receiver support is deliberately conservative. Generation 1 (H2G2-42),
  // generation 2/3 and an unidentified Android TV receiver are kept on H.264.
  Result := ContainsText(Model, 'CHROMECAST ULTRA') or
            ContainsText(Model, 'CHROMECAST WITH GOOGLE TV') or
            ContainsText(Model, 'GOOGLE TV STREAMER');
end;


function HasHardwareVideoEncoder(const ASubtype: TGUID): Boolean;
var
  InputType: MFT_REGISTER_TYPE_INFO;
  OutputType: MFT_REGISTER_TYPE_INFO;
  Activates: PIMFActivateArray;
  Activate: IMFActivate;
  Transform: IMFTransform;
  Count: UINT32;
  Index: UINT32;
  Hr: HRESULT;

begin
  Result := False;
  Activates := nil;
  Count := 0;

  InputType.guidMajorType := MFMediaType_Video;
  InputType.guidSubtype := MFVideoFormat_NV12;
  OutputType.guidMajorType := MFMediaType_Video;
  OutputType.guidSubtype := ASubtype;

  Hr := MFTEnumEx(MFT_CATEGORY_VIDEO_ENCODER,
                  MFT_ENUM_FLAG_HARDWARE or MFT_ENUM_FLAG_SORTANDFILTER,
                  @InputType,
                  @OutputType,
                  Activates,
                  Count);
  if FAILED(Hr) then
    Exit;

  try
    if Count > 0 then
      for Index := 0 to Count - 1 do
        begin
          Activate := Activates^[Index];
          if Assigned(Activate) then
            begin
              Transform := nil;
              Hr := Activate.ActivateObject(IID_IMFTransform, Transform);
              if SUCCEEDED(Hr) and Assigned(Transform) then
                begin
                  Result := True;
                  Transform := nil;
                  Activate.ShutdownObject();
                  Break;
                end;
            end;
        end;
  finally
    Activate := nil;
    Transform := nil;
    if Count > 0 then
      for Index := 0 to Count - 1 do
        Activates^[Index] := nil;
    if Assigned(Activates) then
      CoTaskMemFree(Activates);
  end;
end;


function MfCastGetCaptureCapabilities(
  const ADevice: TMfCastDevice;
  const APreference: TMfCastVideoCodecPreference;
  out ACapabilities: TMfCastCaptureCapabilities): HRESULT;
begin
  ACapabilities.Reset();

  // A zero value can also mean that an older/incomplete mDNS response did not
  // contain a ca field, so retain compatibility by treating zero as unknown.
  // A nonzero value without VIDEO_OUT is definitively an audio-only receiver.
  if MfCastDeviceIsAudioOnly(ADevice) then
    begin
      ACapabilities.SelectionReason :=
        'The selected Cast device reports audio output only and cannot display desktop video.';
      Result := HRESULT_FROM_WIN32(ERROR_NOT_SUPPORTED);
      Exit;
    end;

  ACapabilities.HardwareH264EncoderAvailable :=
    HasHardwareVideoEncoder(MFVideoFormat_H264);
  ACapabilities.HardwareHEVCEncoderAvailable :=
    HasHardwareVideoEncoder(MFVideoFormat_HEVC);
  ACapabilities.ReceiverSupportsH264 := True;
  ACapabilities.ReceiverSupportsHEVC := MfCastReceiverSupportsHEVC(ADevice);

  case APreference of
    cvcpHEVC:
      begin
        if not ACapabilities.ReceiverSupportsHEVC then
          begin
            ACapabilities.SelectionReason :=
              'The selected Cast receiver is not known to support HEVC.';
            Result := HRESULT_FROM_WIN32(ERROR_NOT_SUPPORTED);
            Exit;
          end;
        if not ACapabilities.HardwareHEVCEncoderAvailable then
          begin
            ACapabilities.SelectionReason :=
              'No Media Foundation hardware HEVC encoder is available.';
            Result := HRESULT_FROM_WIN32(ERROR_NOT_SUPPORTED);
            Exit;
          end;
        ACapabilities.SelectedCodec := cvcHEVC;
        ACapabilities.SelectedVideoSubtype := MFVideoFormat_HEVC;
        ACapabilities.SelectionReason :=
          'HEVC was explicitly requested and is supported by both endpoints.';
      end;

    cvcpH264:
      begin
        if not ACapabilities.HardwareH264EncoderAvailable then
          begin
            ACapabilities.SelectionReason :=
              'No Media Foundation hardware H.264 encoder is available.';
            Result := HRESULT_FROM_WIN32(ERROR_NOT_SUPPORTED);
            Exit;
          end;
        ACapabilities.SelectedCodec := cvcH264;
        ACapabilities.SelectedVideoSubtype := MFVideoFormat_H264;
        ACapabilities.SelectionReason := 'Hardware H.264 was explicitly requested.';
      end;

  else
    if ACapabilities.ReceiverSupportsHEVC and
       ACapabilities.HardwareHEVCEncoderAvailable then
      begin
        ACapabilities.SelectedCodec := cvcHEVC;
        ACapabilities.SelectedVideoSubtype := MFVideoFormat_HEVC;
        ACapabilities.SelectionReason :=
          'Automatic selection chose hardware HEVC supported by the receiver.';
      end
    else if ACapabilities.HardwareH264EncoderAvailable then
      begin
        ACapabilities.SelectedCodec := cvcH264;
        ACapabilities.SelectedVideoSubtype := MFVideoFormat_H264;
        ACapabilities.SelectionReason :=
          'Automatic selection chose the compatible hardware H.264 fallback.';
      end
    else
      begin
        ACapabilities.SelectionReason :=
          'No compatible Media Foundation hardware video encoder is available.';
        Result := HRESULT_FROM_WIN32(ERROR_NOT_SUPPORTED);
        Exit;
      end;
  end;

  Result := S_OK;
end;

end.
