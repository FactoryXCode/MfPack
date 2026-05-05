// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: MfLowMidHighEqComponent.pas
// Kind: Pascal Unit
// Release date: 24-01-2026
// Language: ENU
//
// Revision Version: 3.2.0
// Description: 3-band EQ (Low / Mid / High) rack component wrapper.
//              Provides an IMFTransform (TMfLowMidHighEqMFT) to TMfWasApiEffectsRack.
//
// Company: FactoryX
// Intiator(s): Tony (maXcomX), Carmen (carmenh).
// Contributor(s): Tony Kalf (maXcomX), Carmen (carmenh).
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 05/05/2026 All                 Bauhaus release  SDK 10.0.26100.4654 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 or higher.
//
// Related objects: MfParametricEqMFT
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
unit MfLowMidHighEqComponent;

interface

uses

  {Winapi}
  Winapi.Windows,
  Winapi.ActiveX,
  {System}
  System.SysUtils,
  System.Classes,
  System.Math,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfTransform,
  {Application}
  MfWasApiFxComponentBase,
  MfLowMidHighEqMFT;

type

  TMfLowMidHighEqEffect = class(TMfWasApiFxComponentBase)
  private

    FMft: IMFTransform;
    FMftObj: TMfLowMidHighEqMFT;
    FSettings: TMfLowMidHighEqSettings;

    function GetEnabled: LongBool;
    procedure SetEnabled(const Value: LongBool);
    procedure SetRampMs(const Value: Integer);

    procedure SetLowFreqHz(const Value: Single);
    procedure SetLowGainDb(const Value: Single);
    procedure SetLowQ(const Value: Single);

    procedure SetMidFreqHz(const Value: Single);
    procedure SetMidGainDb(const Value: Single);
    procedure SetMidQ(const Value: Single);
    procedure SetMidMode(const Value: TMfEqMidMode);

    procedure SetHighFreqHz(const Value: Single);
    procedure SetHighGainDb(const Value: Single);
    procedure SetHighQ(const Value: Single);

    procedure ApplySettingsToMft();

  protected

    procedure CheckForMft(); override;
    function GetMftInstance(): IMFTransform; override;

  public

    constructor Create(AOwner: TComponent); reintroduce;
    destructor Destroy(); override;
    // Optional helper for callers (not part of TMfWasApiFxComponentBase).
    procedure ResetState();

  published

    // General
    property Enabled: LongBool read GetEnabled write SetEnabled default True;
    property RampMs: Integer read FSettings.RampMs write SetRampMs default 25;

    // Low shelf
    property LowFreqHz: Single read FSettings.LowFreqHz write SetLowFreqHz;
    property LowGainDb: Single read FSettings.LowGainDb write SetLowGainDb;
    property LowQ: Single read FSettings.LowQ write SetLowQ;

    // Mid
    property MidFreqHz: Single read FSettings.MidFreqHz write SetMidFreqHz;
    property MidGainDb: Single read FSettings.MidGainDb write SetMidGainDb;
    property MidQ: Single read FSettings.MidQ write SetMidQ;
    property MidMode: TMfEqMidMode read FSettings.MidMode write SetMidMode default emmPeaking;

    // High shelf
    property HighFreqHz: Single read FSettings.HighFreqHz write SetHighFreqHz;
    property HighGainDb: Single read FSettings.HighGainDb write SetHighGainDb;
    property HighQ: Single read FSettings.HighQ write SetHighQ;
  end;

procedure Register;


implementation


procedure Register;
begin

  RegisterComponents('MfPack Core Audio Samples',
                     [TMfLowMidHighEqEffect]);
end;


{ TMfLowMidHighEqEffect }

constructor TMfLowMidHighEqEffect.Create(AOwner: TComponent);
begin

  inherited Create(AOwner);

  // Defaults (musical + safe)
  FillChar(FSettings,
           SizeOf(FSettings),
           0);

  FSettings.Enabled := True;
  FSettings.RampMs := 25;

  FSettings.LowFreqHz := 80.0;
  FSettings.LowGainDb := 0.0;
  FSettings.LowQ := 0.707;

  FSettings.MidFreqHz := 1000.0;
  FSettings.MidGainDb := 0.0;
  FSettings.MidQ := 1.0;
  FSettings.MidMode := emmPeaking;

  FSettings.HighFreqHz := 8000.0;
  FSettings.HighGainDb := 0.0;
  FSettings.HighQ := 0.707;
end;


destructor TMfLowMidHighEqEffect.Destroy();
begin

  FMft := nil;
  FreeAndNil(FMftObj);

  inherited;
end;


function TMfLowMidHighEqEffect.GetEnabled(): LongBool;
begin

  Result := FSettings.Enabled;
end;


procedure TMfLowMidHighEqEffect.SetEnabled(const Value: LongBool);
begin

  if Value then
    FSettings.Enabled := True
  else
    FSettings.Enabled := False;

  ApplySettingsToMft();
end;


procedure TMfLowMidHighEqEffect.SetRampMs(const Value: Integer);
begin

  FSettings.RampMs := EnsureRange(Value,
                                  0,
                                  5000);
  ApplySettingsToMft();
end;


procedure TMfLowMidHighEqEffect.SetLowFreqHz(const Value: Single);
begin

  FSettings.LowFreqHz := EnsureRange(Value,
                                     10.0,
                                     22000.0);
  ApplySettingsToMft();
end;


procedure TMfLowMidHighEqEffect.SetLowGainDb(const Value: Single);
begin

  FSettings.LowGainDb := EnsureRange(Value,
                                     -24.0,
                                     24.0);
  ApplySettingsToMft();
end;


procedure TMfLowMidHighEqEffect.SetLowQ(const Value: Single);
begin

  FSettings.LowQ := EnsureRange(Value,
                                0.1,
                                12.0);
  ApplySettingsToMft();
end;


procedure TMfLowMidHighEqEffect.SetMidFreqHz(const Value: Single);
begin

  FSettings.MidFreqHz := EnsureRange(Value,
                                     10.0,
                                     22000.0);
  ApplySettingsToMft();
end;


procedure TMfLowMidHighEqEffect.SetMidGainDb(const Value: Single);
begin

  FSettings.MidGainDb := EnsureRange(Value,
                                     -24.0,
                                     24.0);
  ApplySettingsToMft();
end;


procedure TMfLowMidHighEqEffect.SetMidQ(const Value: Single);
begin

  FSettings.MidQ := EnsureRange(Value,
                                0.1,
                                24.0);
  ApplySettingsToMft();
end;


procedure TMfLowMidHighEqEffect.SetMidMode(const Value: TMfEqMidMode);
begin

  FSettings.MidMode := Value;
  ApplySettingsToMft();
end;


procedure TMfLowMidHighEqEffect.SetHighFreqHz(const Value: Single);
begin

  FSettings.HighFreqHz := EnsureRange(Value,
                                      10.0,
                                      22000.0);
  ApplySettingsToMft();
end;


procedure TMfLowMidHighEqEffect.SetHighGainDb(const Value: Single);
begin

  FSettings.HighGainDb := EnsureRange(Value,
                                      -24.0,
                                      24.0);
  ApplySettingsToMft();
end;


procedure TMfLowMidHighEqEffect.SetHighQ(const Value: Single);
begin

  FSettings.HighQ := EnsureRange(Value,
                                 0.1,
                                 12.0);
  ApplySettingsToMft();
end;


procedure TMfLowMidHighEqEffect.CheckForMft();
begin

  if IsDesigning() then
    Exit;

  if Assigned(FMftObj) then
    Exit;

  FMftObj := TMfLowMidHighEqMFT.Create();
  FMft := FMftObj as IMFTransform;
  ApplySettingsToMft();
end;


function TMfLowMidHighEqEffect.GetMftInstance(): IMFTransform;
begin

  Result := FMft;
end;


procedure TMfLowMidHighEqEffect.ApplySettingsToMft();
var
  Eq: IMfLowMidHighEqMft;

begin

  if (FMft = nil) then
    Exit;

  if Supports(FMft,
              IMfLowMidHighEqMft,
              Eq) then
    Eq.SetSettings(FSettings);
end;


procedure TMfLowMidHighEqEffect.ResetState();
var
  Eq: IMfLowMidHighEqMft;

begin

  if (FMft = nil) then
    Exit;

  if Supports(FMft,
              IMfLowMidHighEqMft,
              Eq) then
    Eq.ResetState;
end;

end.

