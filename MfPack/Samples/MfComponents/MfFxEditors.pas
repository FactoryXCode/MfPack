// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: MfFxEditors.pas
// Kind: Pascal Unit
// Release date: 24-06-2023
// Language: ENU
//
// Revision Version: 3.1.9
// Description: VCL editor to add slots to the rack.
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
unit MfFxEditors;

interface

procedure Register;

implementation

uses

  {System}
  System.SysUtils,
  System.Math,
  System.Classes,
  {Vcl}
  Vcl.Forms,
  Vcl.StdCtrls,
  Vcl.ComCtrls,
  Vcl.Controls,
  {Application}
  DesignIntf,
  DesignEditors,
  VCLEditors;

type

  // ---------------------------------------------------------------------------
  // Reusable slider dialog (unit-scope class, Delphi-compatible; no anon methods)
  // ---------------------------------------------------------------------------
  TMfSingleSliderDialog = class(TForm)
  private
    FMinV: Double;
    FMaxV: Double;
    FStepV: Double;
    FPropName: string;

    FLbl: TLabel;
    FTrack: TTrackBar;
    FBtnOK: TButton;
    FBtnCancel: TButton;

    procedure TrackChange(Sender: TObject);

  public

    constructor Create(AOwner: TComponent); override;

    function Execute(const APropName: string;
                     const ACaption: string;
                     const AMinV,
                     AMaxV,
                     AStepV,
                     ACurrent: Double;
                     out AResult: Double): Boolean;
  end;


constructor TMfSingleSliderDialog.Create(AOwner: TComponent);
begin

  // CreateNew avoids needing a DFM.
  inherited CreateNew(AOwner);

  Position := poScreenCenter;
  BorderStyle := bsDialog;
  ClientWidth := 420;
  ClientHeight := 140;

  FLbl := TLabel.Create(Self);
  FLbl.Parent := Self;
  FLbl.Left := 16;
  FLbl.Top := 16;

  FTrack := TTrackBar.Create(Self);
  FTrack.Parent := Self;
  FTrack.Left := 16;
  FTrack.Top := 40;
  FTrack.Width := 390;
  FTrack.Min := 0;
  FTrack.Frequency := 1;
  FTrack.OnChange := TrackChange;

  FBtnOK := TButton.Create(Self);
  FBtnOK.Parent := Self;
  FBtnOK.Caption := 'OK';
  FBtnOK.ModalResult := mrOk;
  FBtnOK.Left := 240;
  FBtnOK.Top := 104;
  FBtnOK.Width := 80;

  FBtnCancel := TButton.Create(Self);
  FBtnCancel.Parent := Self;
  FBtnCancel.Caption := 'Cancel';
  FBtnCancel.ModalResult := mrCancel;
  FBtnCancel.Left := 326;
  FBtnCancel.Top := 104;
  FBtnCancel.Width := 80;
end;


procedure TMfSingleSliderDialog.TrackChange(Sender: TObject);
var
  v: Double;

begin

  v := FMinV + (FTrack.Position * FStepV);
  v := EnsureRange(v,
                   FMinV,
                   FMaxV);
  FLbl.Caption := Format('%s: %g',
                         [FPropName, v]);
end;


function TMfSingleSliderDialog.Execute(const APropName: string;
                                       const ACaption: string;
                                       const AMinV,
                                       AMaxV,
                                       AStepV,
                                       ACurrent: Double;
                                       out AResult: Double): Boolean;
var
  maxPos: Integer;
  curV: Double;

begin

  FPropName := APropName;
  Caption := ACaption;

  FMinV := AMinV;
  FMaxV := AMaxV;
  FStepV := AStepV;

  if (FStepV <= 0) then
    FStepV := 0.01;

  curV := EnsureRange(ACurrent,
                      FMinV,
                      FMaxV);

  maxPos := Round((FMaxV - FMinV) / FStepV);
  if (maxPos < 1) then
    maxPos := 1;

  FTrack.Max := maxPos;
  FTrack.Frequency := Max(1,
                          maxPos div 10);

  FTrack.Position := EnsureRange(Round((curV - FMinV) / FStepV),
                                 0,
                                 maxPos);
  TrackChange(nil);

  Result := (ShowModal = mrOk);

  if Result then
    begin

      AResult := FMinV + (FTrack.Position * FStepV);
      AResult := EnsureRange(AResult,
                             FMinV,
                             FMaxV);
    end;
end;


type

  // ---------------------------------------------------------------------------
  // Property editors
  // ---------------------------------------------------------------------------

  // Generic slider dialog editor for floating properties (stored as text).
  TSingleSliderProperty = class(TFloatProperty)
  protected
    function GetMinValue(): Double; virtual;
    function GetMaxValue(): Double; virtual;
    function GetStep(): Double; virtual;    // slider step
    function GetCaption(): string; virtual; // dialog caption

  public

    function GetAttributes(): TPropertyAttributes; override;
    procedure Edit(); override;
  end;

  // Specific editors (override ranges)
  TGainDbProperty = class(TSingleSliderProperty)
  protected
    function GetMinValue(): Double; override;
    function GetMaxValue(): Double; override;
    function GetStep(): Double; override;
    function GetCaption(): string; override;
  end;

  TFreqHzProperty = class(TSingleSliderProperty)
  protected
    function GetMinValue(): Double; override;
    function GetMaxValue(): Double; override;
    function GetStep(): Double; override;
    function GetCaption(): string; override;
  end;

  TQProperty = class(TSingleSliderProperty)
  protected
    function GetMinValue: Double; override;
    function GetMaxValue: Double; override;
    function GetStep: Double; override;
    function GetCaption: string; override;
  end;

  TBwOctProperty = class(TSingleSliderProperty)
  protected
    function GetMinValue(): Double; override;
    function GetMaxValue(): Double; override;
    function GetStep(): Double; override;
    function GetCaption(): string; override;
  end;

  TWet01Property = class(TSingleSliderProperty)
  protected
    function GetMinValue(): Double; override;
    function GetMaxValue(): Double; override;
    function GetStep(): Double; override;
    function GetCaption(): string; override;
  end;

{ TSingleSliderProperty }

function TSingleSliderProperty.GetAttributes(): TPropertyAttributes;
begin

  Result := inherited GetAttributes + [paDialog];
end;


function TSingleSliderProperty.GetMinValue(): Double;
begin

  Result := 0;
end;


function TSingleSliderProperty.GetMaxValue(): Double;
begin

  Result := 1;
end;


function TSingleSliderProperty.GetStep(): Double;
begin

  Result := 0.01;
end;


function TSingleSliderProperty.GetCaption(): string;
begin
  Result := GetName();
end;


procedure TSingleSliderProperty.Edit();
var
  Dlg: TMfSingleSliderDialog;
  minV,
  maxV,
  stepV,
  curV,
  outV: Double;

begin

  minV := GetMinValue();
  maxV := GetMaxValue();
  stepV := GetStep();
  if (stepV <= 0) then
    stepV := 0.01;

  curV := StrToFloatDef(GetValue,
                        minV);

  curV := EnsureRange(curV,
                      minV,
                      maxV);

  Dlg := TMfSingleSliderDialog.Create(nil);

  try

    if Dlg.Execute(GetName(),
                   GetCaption(),
                   minV,
                   maxV,
                   stepV,
                   curV,
                   outV) then
    begin

      SetValue(FloatToStr(outV));
      Modified();
    end;
  finally

    Dlg.Free();
  end;
end;

{ Specific ranges }

function TGainDbProperty.GetMinValue(): Double;
begin

  Result := -24;
end;


function TGainDbProperty.GetMaxValue(): Double;
begin

  Result :=  24;
end;


function TGainDbProperty.GetStep(): Double;
begin

  Result := 0.1;
end;


function TGainDbProperty.GetCaption(): string;
begin

  Result := 'Gain (dB)';
end;


function TFreqHzProperty.GetMinValue(): Double;
begin

  Result := 10;
end;


function TFreqHzProperty.GetMaxValue(): Double;
begin

  Result := 22000;
end;


function TFreqHzProperty.GetStep(): Double;
begin

  Result := 10;
end;


function TFreqHzProperty.GetCaption(): string;
begin

  Result := 'Center Frequency (Hz)';
end;


function TQProperty.GetMinValue(): Double;
begin

  Result := 0.2;
end;


function TQProperty.GetMaxValue(): Double;
begin

  Result := 12.0;
end;


function TQProperty.GetStep(): Double;
begin

  Result := 0.05;
end;


function TQProperty.GetCaption(): string;
begin

  Result := 'Q';
end;


function TBwOctProperty.GetMinValue(): Double;
begin

  Result := 0.1;
end;


function TBwOctProperty.GetMaxValue(): Double;
begin

  Result := 4.0;
end;


function TBwOctProperty.GetStep(): Double;
begin

  Result := 0.05;
end;


function TBwOctProperty.GetCaption(): string;
begin

  Result := 'Bandwidth (Octaves)';
end;


function TWet01Property.GetMinValue(): Double;
begin

  Result := 0.0;
end;


function TWet01Property.GetMaxValue(): Double;
begin

  Result := 1.0;
end;


function TWet01Property.GetStep(): Double;
begin

  Result := 0.01;
end;


function TWet01Property.GetCaption(): string;
begin

  Result := 'Wet (0..1)';
end;


procedure Register;
begin

  // Property editors registered by property name.
  // If we want them restricted to only your component classes, replace `nil`
  // with the specific component class types.

  RegisterPropertyEditor(TypeInfo(Single),
                         nil,
                         'GainDb',
                         TGainDbProperty);

  RegisterPropertyEditor(TypeInfo(Single),
                         nil,
                         'CenterFreqHz',
                         TFreqHzProperty);

  RegisterPropertyEditor(TypeInfo(Single),
                         nil,
                         'Q',
                         TQProperty);

  RegisterPropertyEditor(TypeInfo(Single),
                         nil,
                         'BandwidthOctaves',
                         TBwOctProperty);

  RegisterPropertyEditor(TypeInfo(Single),
                         nil,
                         'Wet',
                         TWet01Property);
end;

end.

