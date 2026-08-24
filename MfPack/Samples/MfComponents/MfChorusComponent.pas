// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: MfChorusComponent.pas
// Kind: Pascal Unit
// Release date: 24-01-2026
// Language: ENU
//
// Revision Version: 4.0.0
// Description: Chorus component.
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
// Remarks: Requires Windows 7 or higher.
//
// Related objects: MfParametricEqMFT
// Related projects: MfPackX400
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.28000.2705
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
unit MfChorusComponent;

interface

uses

  {System}
  System.Classes,
  System.SysUtils,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfApi,
  WinApi.MediaFoundationApi.MfObjects,
  WinApi.MediaFoundationApi.MfTransform,
  WinApi.MediaFoundationApi.MfError,
  {Application}
  MfWasApiFxComponentBase,
  MfAudioEffectMFTBase,
  MfChorusMFT;

type

  TMfChorusEffect = class(TMfWasApiFxComponentBase)
  private

    FMftObj: TMfChorusMFT;
    FMft: IMFTransform;
    FSettings: TChorusSettings;

    // Published-property setters (keep DFM streaming safe)
    procedure SetEnabled(const AValue: Boolean);
    procedure SetMix(const AValue: Single);
    procedure SetFeedback(const AValue: Single);
    procedure SetBaseDelayMs(const AValue: Single);
    procedure SetDepthMs(const AValue: Single);
    procedure SetRateMode(const AValue: TMfChorusRateMode);
    procedure SetRateHz(const AValue: Single);
    procedure SetTempoBpm(const AValue: Single);
    procedure SetNoteDiv(const AValue: TMfChorusNoteDiv);
    procedure SetWidthPct(const AValue: Single);
    procedure SetSmoothMs(const AValue: Single);

  protected

    procedure CheckForMft; override;
    function GetMftInstance: IMFTransform; override;

  public

    constructor Create(AOwner: TComponent); //override;
    procedure AfterConstruction(); override;
    destructor Destroy; override;

    procedure SetSettings(const S: TChorusSettings);
    function GetSettings: TChorusSettings;

    function GetEnabled: Boolean;
    function GetMix: Single;
    function GetFeedback: Single;
    function GetBaseDelayMs: Single;
    function GetDepthMs: Single;
    function GetRateMode: TMfChorusRateMode;
    function GetRateHz: Single;
    function GetTempoBpm: Single;
    function GetNoteDiv: TMfChorusNoteDiv;
    function GetWidthPct: Single;
    function GetSmoothMs: Single;

  public

    // Convenience access
    property Settings: TChorusSettings read GetSettings write SetSettings;

  published

    property Enabled: Boolean read GetEnabled write SetEnabled default True;
    property Mix: Single read GetMix write SetMix;
    property Feedback: Single read GetFeedback write SetFeedback;
    property BaseDelayMs: Single read GetBaseDelayMs write SetBaseDelayMs;
    property DepthMs: Single read GetDepthMs write SetDepthMs;

    property RateMode: TMfChorusRateMode read GetRateMode write SetRateMode default crmFreeHz;
    property RateHz: Single read GetRateHz write SetRateHz;
    property TempoBpm: Single read GetTempoBpm write SetTempoBpm;
    property NoteDiv: TMfChorusNoteDiv read GetNoteDiv write SetNoteDiv default cnd1_8;

    property WidthPct: Single read GetWidthPct write SetWidthPct;
    property SmoothMs: Single read GetSmoothMs write SetSmoothMs;
  end;

procedure Register;


implementation


procedure Register;
begin

  RegisterComponents('MfPack Core Audio Samples',
                     [TMfChorusEffect]);
end;

{ TMfChorusEffect }

constructor TMfChorusEffect.Create(AOwner: TComponent);
begin

  inherited;

  // Defaults (match MFT)
  FillChar(FSettings,
           SizeOf(FSettings),
           0);

  FSettings.Enabled := True;

  FSettings.Mix := 0.35;
  FSettings.Feedback := 0.10;

  FSettings.BaseDelayMs := 22.0;
  FSettings.DepthMs := 8.0;

  FSettings.RateMode := crmFreeHz;
  FSettings.RateHz := 0.35;
  FSettings.TempoBpm := 120.0;
  FSettings.NoteDiv := cnd1_8;

  FSettings.WidthPct := 70.0;
  FSettings.SmoothMs := 20.0;
end;


procedure TMfChorusEffect.AfterConstruction();
begin
  inherited;

  CheckForMft();
end;


destructor TMfChorusEffect.Destroy();
begin

  // Release COM/MFT
  FMft := nil;
  FreeAndNil(FMftObj);

  inherited;
end;


procedure TMfChorusEffect.CheckForMft();
begin

  if (FMftObj <> nil) then
    Exit;

  FMftObj := TMfChorusMFT.Create();
  FMft := FMftObj as IMFTransform;
  FMftObj.SetSettings(FSettings);
end;


function TMfChorusEffect.GetMftInstance: IMFTransform;
begin

  Result := FMft;
end;

function TMfChorusEffect.GetSettings: TChorusSettings;
begin

  Result := FSettings;
end;


function TMfChorusEffect.GetEnabled: Boolean;
begin

  Result := FSettings.Enabled;
end;


function TMfChorusEffect.GetMix: Single;
begin

  Result := FSettings.Mix;
end;


function TMfChorusEffect.GetFeedback: Single;
begin

  Result := FSettings.Feedback;
end;


function TMfChorusEffect.GetBaseDelayMs: Single;
begin

  Result := FSettings.BaseDelayMs;
end;


function TMfChorusEffect.GetDepthMs: Single;
begin

  Result := FSettings.DepthMs;
end;


function TMfChorusEffect.GetRateMode: TMfChorusRateMode;
begin

  Result := FSettings.RateMode;
end;


function TMfChorusEffect.GetRateHz: Single;
begin

  Result := FSettings.RateHz;
end;


function TMfChorusEffect.GetTempoBpm: Single;
begin
  Result := FSettings.TempoBpm;
end;


function TMfChorusEffect.GetNoteDiv: TMfChorusNoteDiv;
begin

  Result := FSettings.NoteDiv;
end;


function TMfChorusEffect.GetWidthPct: Single;
begin

  Result := FSettings.WidthPct;
end;


function TMfChorusEffect.GetSmoothMs: Single;
begin

  Result := FSettings.SmoothMs;
end;



procedure TMfChorusEffect.SetEnabled(const AValue: Boolean);
begin

  FSettings.Enabled := AValue;
  SetSettings(FSettings);
end;


procedure TMfChorusEffect.SetMix(const AValue: Single);
begin

  FSettings.Mix := AValue;
  SetSettings(FSettings);
end;


procedure TMfChorusEffect.SetFeedback(const AValue: Single);
begin
  FSettings.Feedback := AValue;
  SetSettings(FSettings);
end;


procedure TMfChorusEffect.SetBaseDelayMs(const AValue: Single);
begin

  FSettings.BaseDelayMs := AValue;
  SetSettings(FSettings);
end;


procedure TMfChorusEffect.SetDepthMs(const AValue: Single);
begin

  FSettings.DepthMs := AValue;
  SetSettings(FSettings);
end;


procedure TMfChorusEffect.SetRateMode(const AValue: TMfChorusRateMode);
begin

  FSettings.RateMode := AValue;
  SetSettings(FSettings);
end;


procedure TMfChorusEffect.SetRateHz(const AValue: Single);
begin

  FSettings.RateHz := AValue;
  SetSettings(FSettings);
end;


procedure TMfChorusEffect.SetTempoBpm(const AValue: Single);
begin

  FSettings.TempoBpm := AValue;
  SetSettings(FSettings);
end;


procedure TMfChorusEffect.SetNoteDiv(const AValue: TMfChorusNoteDiv);
begin

  FSettings.NoteDiv := AValue;
  SetSettings(FSettings);
end;


procedure TMfChorusEffect.SetWidthPct(const AValue: Single);
begin

  FSettings.WidthPct := AValue;
  SetSettings(FSettings);
end;


procedure TMfChorusEffect.SetSmoothMs(const AValue: Single);
begin

  FSettings.SmoothMs := AValue;
  SetSettings(FSettings);
end;


procedure TMfChorusEffect.SetSettings(const S: TChorusSettings);
begin

  FSettings := S;

  if not IsDesigning then
    begin

      CheckForMft();
      if (FMftObj <> nil) then
        FMftObj.SetSettings(FSettings);
    end;
end;

end.

