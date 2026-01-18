// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - MediaFoundation
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: MfAudioHighMidLowTypes.pas
// Kind: Pascal Unit
// Release date: 13-08-2025
// Language: ENU
//
// Revision Version: 3.1.9
//
// Description: The MFT interface definition and signature type.
//
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX)
// Contributor(s): Tony (maXcomX)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 01/04/2026 All                 Sineead O'Connor release  SDK 10.0.26100.4654 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 (2H20) or later.
//          Recommended minimum Delphi version: XE7.
//
// Related objects: -
// Related projects: MfPackX319/Samples/WasApiPlayer/Example3
//
// Compiler version: 23 up to 35
// SDK version: 10.0.26100.4654
//
// Todo: -
//
//==============================================================================
// Source: -
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
unit MfAudioHighMidLowTypes;

interface

uses
  WinApi.Windows;

type

  // Simple smoothing modes for parameter changes.
  // Keep it small for teaching; can be expanded later.
  TMfRampMode = (rmOff,
                 rmFast,
                 rmSmooth,
                 rmCustom);

  // Mid band behavior.
  // Peaking = bell boost/cut, Notch = band-stop (useful for feedback removal).
  TMfMidMode = (mmPeaking,
                mmNotch);

  TBiquadCoeffs = record
    a0,
    a1,
    a2: Double;
    b1,
    b2: Double;
  end;

  TEqTuning = record
    // enable
    Enabled: Boolean;

    // Gain (dB)
    LowDb: Integer;     // e.g. -24 .. +24
    MidDb: Integer;     // e.g. -24 .. +24
    HighDb: Integer;    // e.g. -24 .. +24

    // Frequencies (Hz)
    LowFreqHz: Single;   // e.g. 60
    MidFreqHz: Single;   // e.g. 1000
    HighFreqHz: Single;  // e.g. 8000

    // Mid band shape
    MidMode: TMfMidMode; // mmPeaking / mmNotch
    MidQ: Single;        // bandwidth / Q

    // Shelf slopes
    LowShelfSlope: Single;
    HighShelfSlope: Single;

    // Smoothing
    RampMode: TMfRampMode;
    RampTimeMs: Integer;
  end;


  // Control interface for a classic 3-band mixer EQ (Low / Mid / High).
  // NOTE: Values are interpreted in the MFT and clamped there.
  IMfHighMidLowControl = interface(IUnknown)
    ['{8C934F5C-BA89-4F5B-9B67-ED55A70C4A1B}']

    function SetEnabled(const AEnabled: Boolean): HRESULT; stdcall;

    // Gains (dB)
    function SetLowDb(const Db: Single): HRESULT; stdcall;
    function SetMidDb(const Db: Single): HRESULT; stdcall;   // ignored in Notch mode.
    function SetHighDb(const Db: Single): HRESULT; stdcall;

    // Center / corner frequencies (Hz)
    function SetLowFreqHz(const Hz: Single): HRESULT; stdcall;   // 20..400 typical.
    function SetMidFreqHz(const Hz: Single): HRESULT; stdcall;   // 200..6000 typical.
    function SetHighFreqHz(const Hz: Single): HRESULT; stdcall;  // 2000..20000 typical.

    // Bandwidth (Q) for the Mid band (affects peaking and notch).
    function SetMidQ(const Q: Single): HRESULT; stdcall;          // 0.3..6 typical.

    // Shelf slope / transition steepness (RBJ 'S' parameter).
    function SetLowShelfSlope(const S: Single): HRESULT; stdcall;  // 0.1..4.0 typical.
    function SetHighShelfSlope(const S: Single): HRESULT; stdcall; // 0.1..4.0 typical.

    // Mid band mode.
    function SetMidMode(const Mode: TMfMidMode): HRESULT; stdcall;

    // Optional: smoothing of changes to avoid zipper noise.
    function SetRampMode(const Mode: TMfRampMode): HRESULT; stdcall;
    function SetRampTimeMs(const Ms: Integer): HRESULT; stdcall;

    function GetBiquadCoeffs(out Low,
                             Mid,
                             High: TBiquadCoeffs;
                             out SampleRate: Double): HRESULT;
  end;


  function GetDefaultEqTuning(): TEqTuning;


implementation

uses
  MfAudioHighMidLowMFT;


function GetDefaultEqTuning(): TEqTuning;
begin

  FillChar(Result,
           SizeOf(Result),
           0);

  Result.Enabled := True;

  // Gains (dB)
  Result.LowDb := 0;
  Result.MidDb := 0;
  Result.HighDb := 0;

  // Frequencies (Hz) - mixer-ish defaults.
  Result.LowFreqHz := 80.0;
  Result.MidFreqHz := 1000.0;
  Result.HighFreqHz := 8000.0;

  // Mid band shape.
  Result.MidMode := mmPeaking;
  Result.MidQ := 1.0;

  // Shelf slopes.
  Result.LowShelfSlope := 1.0;
  Result.HighShelfSlope := 1.0;

  // Smoothing.
  Result.RampMode := rmSmooth;
  Result.RampTimeMs := 200;
end;

end.
