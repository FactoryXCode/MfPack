// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: WinApi.DirectX - D3D11
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module:  Helpers.pas
// Kind: Pascal / Delphi unit
// Release date: 24-11-2025
// Language: ENU
//
// Revision Version: 3.1.9
// Description: Helpers unit for this project.
//
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 01/04/2026 All                 Sineead O'Connor release  SDK 10.0.26100.4654 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: -
//
// Related objects: -
// Related projects: MfPackX319
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.26100.4654
//
// Todo: -
//
//==============================================================================
// Source: -
//
// Copyright (c) FactoryX. All rights reserved.
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
unit Helpers;

interface

uses

  {WinApi}
  WinApi.Windows,
  WinApi.ActiveX,
  {System}
  System.SysUtils,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfApi,
  WinApi.MediaFoundationApi.MfObjects,
  WinApi.MediaFoundationApi.MfTransform;

  // Utility to check HRESULT
  procedure CheckHR(hr: HRESULT;
                    const Context: string = '');

  // Tests if a given resolution is supported by encoder (like CLSID_CMSH265EncoderMFT, CLSID_CMSH264EncoderMFT.264 etc.)
  // MFVideoFormat should be the same MFVideoFormat codec related to the CLSID of the encoder.
  // Note: This method is also implemented in WinApi.MediaFoundationApi.MfMetLib.pas from MfPack version 19 and up.
  function IsResolutionSupported(const CLSIDEncoder: TGUID;  // CLSID of the encoder, like CLSID_CMSH265EncoderMFT
                                 const MFVideoFormat: TGUID; // MFVideoFormat, like MFVideoFormat_HEVC
                                 const InputType: TGUID;     // The input type sub format, like MFVideoFormat_NV12
                                 Width: UInt32;
                                 Height: UInt32): Boolean;

  // Delphi MulDiv replacement for Int64 types.
  function _MulDiv64(const aNumber,
                           aNumerator,
                           aDenominator: Int64): Int64;


implementation


// Utility to check HRESULT
procedure CheckHR(hr: HRESULT;
                  const Context: string = '');
begin
  if Failed(hr) then
    raise Exception.CreateFmt('HRESULT 0x%x - %s', [hr,
                                                    Context]);
end;


function IsResolutionSupported(const CLSIDEncoder: TGUID;  // CLSID of the encoder, like CLSID_CMSH265EncoderMFT
                               const MFVideoFormat: TGUID; // MFVideoFormat, like MFVideoFormat_HEVC
                               const InputType: TGUID;     // The input type sub format, like MFVideoFormat_NV12
                               Width: UInt32;
                               Height: UInt32): Boolean;
var
  hr: HRESULT;
  Encoder: IMFTransform;
  InType: IMFMediaType;
  OutType: IMFMediaType;

begin

  Result := False;

  // Create encoder
  hr := CoCreateInstance(CLSIDEncoder,
                         nil,
                         CLSCTX_INPROC_SERVER,
                         IMFTransform,
                         Encoder);
  if Failed(hr) then
    Exit;

  // Check output type (e.g. HEVC)
  hr := MFCreateMediaType(OutType);
  if Failed(hr) then
    Exit;

  OutType.SetGUID(MF_MT_MAJOR_TYPE,
                  MFMediaType_Video);

  OutType.SetGUID(MF_MT_SUBTYPE,
                  MFVideoFormat);

  hr := Encoder.SetOutputType(0,
                              OutType,
                              0);
  if FAILED(hr) then
    Exit;

  // Check input type
  hr := MFCreateMediaType(InType);
  if Failed(hr) then
    Exit;

  InType.SetGUID(MF_MT_MAJOR_TYPE,
                 MFMediaType_Video);

  InType.SetGUID(MF_MT_SUBTYPE,
                 InputType);

  InType.SetUINT32(MF_MT_INTERLACE_MODE,
                   MFVideoInterlace_Progressive);

  MFSetAttributeSize(InType,
                     MF_MT_FRAME_SIZE,
                     Width,
                     Height);

  hr := Encoder.SetInputType(0,
                             InType,
                             0);

  Result := Succeeded(hr);
end;

// NOTE:
// MulDiv is 32-bit (Integer in/out). With range checking on, Delphi throws Range check error.
// If working with Int64, use this method, instead of MulDiv.
// Note: In later MfPack versions (> version 3.18), this method will be declared in WinApi.MediaFoundationApi.MfUtils.
function _MulDiv64(const aNumber,
                         aNumerator,
                         aDenominator: Int64): Int64;
begin
  if (aDenominator <= 0) then
    Exit(0);

  // Avoid Int64 overflow: (QpcDelta * 10_000_000) div PerfFreq
  Result := (aNumber div aDenominator) * aNumerator + (aNumber mod aDenominator) * aNumerator div aDenominator;
end;

end.
