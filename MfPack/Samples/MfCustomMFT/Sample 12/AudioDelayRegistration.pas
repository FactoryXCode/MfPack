// FactoryX
//
// Copyright: (c) FactoryX. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: AudioDelayRegistration.pas
// Kind: Pascal Unit
// Release date: 13-08-2026
// Language: ENU
//
// Version: 4.0.0
// Description: Defines the CLSID, friendly name, and control interface
//              of the registered audio delay MFT.
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
unit AudioDelayRegistration;

// Public identity and control contract of the registered audio effect.

interface

uses
  {WinApi}
  WinApi.Windows;

const
  CLSID_FactoryXAudioDelayMFT: TGUID = '{A63BF709-564A-4F5B-A6B4-9EA48E0FC9AE}';
  FACTORYX_AUDIO_DELAY_MFT_NAME      = 'FactoryX PCM Audio Delay MFT';

type
  // The IID and method signature must match Sample 9. Keeping this small
  // client-side declaration avoids linking the MFT implementation into the
  // registered player executable.
  IMfAudioDelayControl = interface(IUnknown)
  ['{48222613-78FC-4A40-B95B-7E67E22443EE}']
    function SetEffect(const ADelayMs: Cardinal;
                       const AWetPercent: Cardinal): HResult; stdcall;
  end;

implementation

end.
