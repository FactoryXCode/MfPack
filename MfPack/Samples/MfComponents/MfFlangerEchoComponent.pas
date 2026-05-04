// FactoryX
//
// Copyright:  FactoryX. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: MfFlangerEchoComponent.pas
// Kind: Pascal Unit
// Release date: 24-01-2026
// Language: ENU
//
// Revision Version: 3.1.9
// Description: Flanger/Echo non-visual component.
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
unit MfFlangerEchoComponent;

interface

uses

  {System}
  System.Classes,
  System.SysUtils,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfTransform,
  {Application}
  MfWasApiFxIntf,
  MfWasApiFxComponentBase,
  MfFlangerEchoMFT,
  MfAudioEffectMFTBase;

type

  TMfFlangerEchoEffect = class(TMfWasApiFxComponentBase)
  private

    FMft: IMFTransform;
    FCtl: IMfFlangerEchoControl;
    FIns: IMfFlangerEchoInspect;
    FMftObj: TMfFlangerEchoMFT;

    FEnabled: Boolean;
    FBaseDelayMs: Single;
    FDepthMs: Single;
    FRateHz: Single;
    FFeedback: Single;
    FWet: Single;

    procedure SetEnabled(Value: Boolean);
    procedure SetBaseDelayMs(Value: Single);
    procedure SetDepthMs(Value: Single);
    procedure SetRateHz(Value: Single);
    procedure SetFeedback(Value: Single);
    procedure SetWet(Value: Single);

  protected

    procedure CheckForMft(); override;
    function GetMftInstance(): IMFTransform; override;

  public

    constructor Create(AOwner: TComponent); //override;
    procedure AfterConstruction(); override;
    destructor Destroy(); override;

  published

    property Enabled: Boolean read FEnabled write SetEnabled default False;
    property BaseDelayMs: Single read FBaseDelayMs write SetBaseDelayMs;
    property DepthMs: Single read FDepthMs write SetDepthMs;
    property RateHz: Single read FRateHz write SetRateHz;
    property Feedback: Single read FFeedback write SetFeedback;
    property Wet: Single read FWet write SetWet;
  end;

procedure Register;


implementation


procedure Register;
begin

  RegisterComponents('MfPack Core Audio Samples',
                     [TMfFlangerEchoEffect]);
end;


constructor TMfFlangerEchoEffect.Create(AOwner: TComponent);
begin

  inherited Create(AOwner);

  FEnabled := False;
  FBaseDelayMs := 3.0;
  FDepthMs := 2.0;
  FRateHz := 0.25;
  FFeedback := 0.2;
  FWet := 0.35;

  // Do NOT create any MFT by default, this will creates memory leaks!
  //  CheckForMft();
end;


procedure TMfFlangerEchoEffect.AfterConstruction();
begin
  inherited;

  CheckForMft();
end;


destructor TMfFlangerEchoEffect.Destroy();
begin

  // Release interface views first.
  FIns := nil;
  FCtl := nil;
  FMft := nil;

  // THEN free the actual object.
  FreeAndNil(FMftObj);

  inherited;
end;


procedure TMfFlangerEchoEffect.CheckForMft();
begin

  if (FMftObj <> nil) then
    Exit;

  if (FMft = nil) then
    begin

      FMftObj := TMfFlangerEchoMFT.Create();
      FMft := FMftObj as IMFTransform;
      FCtl := FMftObj as IMfFlangerEchoControl;
      FIns := FMftObj as IMfFlangerEchoInspect;


      // Push current values to MFT
      FCtl.EnableFX(FEnabled);
      FCtl.SetBaseDelayMs(FBaseDelayMs);
      FCtl.SetDepthMs(FDepthMs);
      FCtl.SetRateHz(FRateHz);
      FCtl.SetFeedback(FFeedback);
      FCtl.SetWet(FWet);
    end;
end;


function TMfFlangerEchoEffect.GetMftInstance(): IMFTransform;
begin

  Result := FMft;
end;


procedure TMfFlangerEchoEffect.SetEnabled(Value: Boolean);
begin

  FEnabled := Value;

  if IsDesigning() then
    Exit;

  CheckForMft();
  FCtl.EnableFX(Value);
end;


procedure TMfFlangerEchoEffect.SetBaseDelayMs(Value: Single);
begin

  FBaseDelayMs := Value;

  if IsDesigning() then
    Exit;

  CheckForMft();
  FCtl.SetBaseDelayMs(Value);
end;


procedure TMfFlangerEchoEffect.SetDepthMs(Value: Single);
begin

  FDepthMs := Value;

  if IsDesigning() then
    Exit;

  CheckForMft();
  FCtl.SetDepthMs(Value);
end;


procedure TMfFlangerEchoEffect.SetRateHz(Value: Single);
begin

  FRateHz := Value;

  if IsDesigning() then
    Exit;

  CheckForMft();
  FCtl.SetRateHz(Value);
end;


procedure TMfFlangerEchoEffect.SetFeedback(Value: Single);
begin

  FFeedback := Value;

  if IsDesigning() then
    Exit;

  CheckForMft();
  FCtl.SetFeedback(Value);
end;


procedure TMfFlangerEchoEffect.SetWet(Value: Single);
begin

  FWet := Value;

  if IsDesigning() then
    Exit;

  CheckForMft();
  FCtl.SetWet(Value);
end;

end.

