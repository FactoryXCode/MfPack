// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: MfAudioBassTrebleTypes.pas
// Kind: Pascal Unit
// Release date: 24-06-2023
// Language: ENU
//
// Revision Version: 3.2.0
// Description: MFT layout.
//
// Company: FactoryX
// Intiator(s): Tony (maXcomX).
// Contributor(s): Tony Kalf (maXcomX)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 05/05/2026 All                 Bauhaus release  SDK 10.0.26100.4654 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 7 or higher.
//
// Related objects: -
// Related projects: MfPackX320
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.26100.4654
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
unit MfAudioBassTrebleTypes;

interface

uses
  WinApi.Windows,
  WinApi.ActiveX;

const
  // Control interface GUID
  IID_IMfBassTrebleControl: TGUID = '{B0EAC79F-7E7A-4E0F-9A4F-4A3D2C4D1E9E}';

type

  TMfRampMode = (rmOff,
                 rmFast,
                 rmSmooth,
                 rmCustom);

  // Simple optional control interface to adjust EQ at runtime.
  // Implemented by the MFT object.
  IMfBassTrebleControl = interface(IUnknown)
    ['{B0EAC79F-7E7A-4E0F-9A4F-4A3D2C4D1E9E}']

    function SetBassDb(const Db: Single): HRESULT; stdcall;        // -24..+24

    function SetTrebleDb(const Db: Single): HRESULT; stdcall;      // -24..+24

    function SetBassFreqHz(const Hz: Single): HRESULT; stdcall;    // default 100

    function SetTrebleFreqHz(const Hz: Single): HRESULT; stdcall;  // default 8000

    function GetBassDb(out Db: Single): HRESULT; stdcall;

    function GetTrebleDb(out Db: Single): HRESULT; stdcall;

    // Ramp modes.
    function SetRampMode(const Mode: TMfRampMode): HRESULT; stdcall;

    function SetRampTimeMs(const Ms: Cardinal): HRESULT; stdcall; // used when rmCustom

    function GetRampMode(out Mode: TMfRampMode): HRESULT; stdcall;

    function GetRampTimeMs(out Ms: Cardinal): HRESULT; stdcall;
  end;

implementation

end.

