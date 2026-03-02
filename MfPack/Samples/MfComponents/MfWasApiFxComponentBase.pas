// FactoryX
//
// Copyright: FactoryX. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: MfWasApiFxComponentBase.pas
// Kind: Pascal Unit
// Release date: 24-01-2026
// Language: ENU
//
// Revision Version: 3.1.9
// Description: Shared base for non-visual FX components that provide an IMFTransform.
//
// Company: FactoryX
// Intiator(s): Tony (maXcomX), Carmen (carmenh).
// Contributor(s): Tony Kalf (maXcomX), Carmen (carmenh).
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 01/13/2026 All                 Sineead O'Connor release  SDK 10.0.26100.4654 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 or higher.
//          Please, read documentation carefully!
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
// =============================================================================
// Source: FactoryX.Code.
//         https://github.com/BillyDM/awesome-audio-dsp/blob/main/sections/DSP_COOKBOOKS.md
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
unit MfWasApiFxComponentBase;

interface

uses

  {Winapi}
  Winapi.Windows,
  {System}
  System.Classes,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfTransform,
  {Application}
  MfWasApiFxIntf;

type

  TMfWasApiFxComponentBase = class(TComponent, IMfWasApiFxProvider)

    function GetMFT: IMFTransform; stdcall;

    // Optional capability flags (useful for validation/UI).
    function SupportsPcmBits(BitsPerSample: Integer): Boolean; stdcall; // 16/24/32
    function SupportsFloat32(): Boolean; stdcall;

  protected

    // Derived classes create their FMft + control interfaces here (lazy).
    procedure CheckForMft(); virtual; abstract;
    // Derived classes return their IMFTransform instance here (after EnsureMft).
    function GetMftInstance(): IMFTransform; virtual; abstract;
    // Helper: do nothing at design-time.
    function IsDesigning(): Boolean; //inline;

  public

    constructor Create(AOwner: TComponent); reintroduce; //override;
    procedure AfterConstruction(); override;
    destructor Destroy(); override;

  end;


implementation


constructor TMfWasApiFxComponentBase.Create(AOwner: TComponent);
begin

  inherited Create(AOwner);

  // Do NOT create any MFT by default, this creates memory leaks!
  //  CheckForMft();
end;


procedure TMfWasApiFxComponentBase.AfterConstruction();
begin
  inherited;

end;


destructor TMfWasApiFxComponentBase.Destroy();
begin

  inherited;
end;


function TMfWasApiFxComponentBase.IsDesigning(): Boolean;
begin

  Result := (csDesigning in ComponentState) or (csLoading in ComponentState);
end;


function TMfWasApiFxComponentBase.GetMFT(): IMFTransform;
begin

  if not IsDesigning() then
    CheckForMft();

  Result := GetMftInstance();
end;


function TMfWasApiFxComponentBase.SupportsPcmBits(BitsPerSample: Integer): Boolean;
begin

  Result := (BitsPerSample = 16) or
            (BitsPerSample = 24) or
            (BitsPerSample = 32);
end;


function TMfWasApiFxComponentBase.SupportsFloat32(): Boolean;
begin

  Result := True;
end;

end.

