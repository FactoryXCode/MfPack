// FactoryX
//
// Copyright:  FactoryX. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: MfBeatLed.pas
// Kind: Pascal Unit Component
// Release date: 24-01-2019
// Language: ENU
//
// Version: 3.2.0
// Description: Single visual component: Beat Led.
//
// Company: FactoryX
// Intiator(s): Tony (maXcomX), Carmen (carmenh).
// Contributor(s): Tony Kalf (maXcomX), Carmen (carmenh).
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 11/04/2026 All                 BauHaus release  SDK 10.0.26100.4654 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: To install the visual components, choose Install in the Project Manager.
//          Requires Windows 10 or later.
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
// Source: FactoryX.code
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
unit MfBeatLed;

interface

uses

  {Winapi}
  Winapi.Windows,
  {System}
  System.SysUtils,
  System.Classes,
  System.Math,
  {Vcl}
  Vcl.Controls,
  Vcl.Graphics;

type

  TMfBeatLedShape = (blsCircle,
                     blsEllipse,
                     blsRectangle,
                     blsRoundRect);

  TMfBeatLed = class(TGraphicControl)
  private

    FLedShape: TMfBeatLedShape;
    FLedOffColor: TColor;
    FLedOnColor: TColor;
    FBorderColor: TColor;
    FBorderWidth: Integer;
    FHoldTimeMs: Integer;
    FFadeTimeMs: Integer;
    FValue: Double; // 0.0 .. 1.0
    FPulseActive: Boolean;
    FPulseStart100ns: Int64;
    FLastPosition100ns: Int64;
    FTransparent: Boolean;

    procedure SetLedShape(const Value: TMfBeatLedShape);
    procedure SetLedOffColor(const Value: TColor);
    procedure SetLedOnColor(const Value: TColor);
    procedure SetBorderColor(const Value: TColor);
    procedure SetBorderWidth(const Value: Integer);
    procedure SetHoldTimeMs(const Value: Integer);
    procedure SetFadeTimeMs(const Value: Integer);
    procedure SetTransparent(const Value: Boolean);

    procedure SetValueInternal(const AValue: Double);
    procedure DrawLedShape(const ACanvas: TCanvas;
                           const R: TRect;
                           const ABrushColor: TColor;
                           const APenColor: TColor;
                           const APenWidth: Integer);
    function GetCurrentDisplayColor(): TColor;

  protected

    procedure Paint; override;

  public

    constructor Create(AOwner: TComponent); override;

    procedure Reset();
    procedure Pulse();
    procedure TriggerPulse(const APosition100ns: Int64);
    procedure UpdatePulse(const APosition100ns: Int64);

    property Value: Double read FValue;

  published

    property Align;
    property Anchors;
    property Constraints;
    property Enabled;
    property Visible;
    property ShowHint;
    property ParentShowHint;
    property Hint;
    property PopupMenu;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;

    property LedShape: TMfBeatLedShape read FLedShape write SetLedShape default blsCircle;
    property LedOffColor: TColor read FLedOffColor write SetLedOffColor default $00594744;
    property LedOnColor: TColor read FLedOnColor write SetLedOnColor default clYellow;
    property BorderColor: TColor read FBorderColor write SetBorderColor default clBlack;
    property BorderWidth: Integer read FBorderWidth write SetBorderWidth default 1;
    property HoldTimeMs: Integer read FHoldTimeMs write SetHoldTimeMs default 60;
    property FadeTimeMs: Integer read FFadeTimeMs write SetFadeTimeMs default 220;
    property Transparent: Boolean read FTransparent write SetTransparent default True;

    property Height default 20;
    property Width default 20;
  end;

procedure Register;


implementation


procedure Register;
begin

  RegisterComponents('RDJ Controls',
                     [TMfBeatLed]);
end;


function ClampByte(const AValue: Integer): Byte; inline;
begin

  if (AValue < 0) then
    Result := 0
  else
    if (AValue > 255) then
      Result := 255
    else
      Result := Byte(AValue);
end;


function ClampDouble(const AValue,
                           AMin,
                           AMax: Double): Double; inline;
begin

  if (AValue < AMin) then
    Result := AMin
  else
    if (AValue > AMax) then
      Result := AMax
    else
      Result := AValue;
end;


function BlendColor(const AColor1,
                          AColor2: TColor;
                    const AT: Double): TColor;
var
  C1: TColor;
  C2: TColor;
  R1: Integer;
  G1: Integer;
  B1: Integer;
  R2: Integer;
  G2: Integer;
  B2: Integer;
  TVal: Double;
  R: Integer;
  G: Integer;
  B: Integer;

begin

  C1 := ColorToRGB(AColor1);
  C2 := ColorToRGB(AColor2);

  R1 := GetRValue(C1);
  G1 := GetGValue(C1);
  B1 := GetBValue(C1);

  R2 := GetRValue(C2);
  G2 := GetGValue(C2);
  B2 := GetBValue(C2);

  TVal := ClampDouble(AT,
                      0.0,
                      1.0);

  R := Round(R1 + ((R2 - R1) * TVal));
  G := Round(G1 + ((G2 - G1) * TVal));
  B := Round(B1 + ((B2 - B1) * TVal));

  Result := RGB(ClampByte(R),
                ClampByte(G),
                ClampByte(B));
end;


function EaseOutQuad(const T: Double): Double; inline;
begin
  Result := 1.0 - ((1.0 - T) * (1.0 - T));
end;


function EaseOutCubic(const T: Double): Double; inline;
var
  X: Double;

begin

  X := 1.0 - T;
  Result := 1.0 - (X * X * X);
end;


function EaseOutSine(const T: Double): Double; inline;
begin

  Result := Sin((T * Pi) / 2.0);
end;

{ TMfBeatLed }

constructor TMfBeatLed.Create(AOwner: TComponent);
begin

  inherited Create(AOwner);

  Width := 20;
  Height := 20;

  FLedShape := blsCircle;
  FLedOffColor := $00594744;
  FLedOnColor := clYellow;
  FBorderColor := clBlack;
  FBorderWidth := 1;
  FHoldTimeMs := 60;
  FFadeTimeMs := 220;
  FTransparent := True;

  FValue := 0.0;
  FPulseActive := False;
  FPulseStart100ns := 0;
  FLastPosition100ns := 0;
end;


procedure TMfBeatLed.SetLedShape(const Value: TMfBeatLedShape);
begin

  if (FLedShape = Value) then
    Exit;

  FLedShape := Value;
  Invalidate;
end;


procedure TMfBeatLed.SetLedOffColor(const Value: TColor);
begin

  if (FLedOffColor = Value) then
    Exit;

  FLedOffColor := Value;
  Invalidate;
end;


procedure TMfBeatLed.SetLedOnColor(const Value: TColor);
begin
  if (FLedOnColor = Value) then
    Exit;

  FLedOnColor := Value;
  Invalidate;
end;


procedure TMfBeatLed.SetBorderColor(const Value: TColor);
begin
  if (FBorderColor = Value) then
    Exit;

  FBorderColor := Value;
  Invalidate;
end;


procedure TMfBeatLed.SetBorderWidth(const Value: Integer);
var
  NewValue: Integer;

begin

  NewValue := Value;

  if (NewValue < 0) then
    NewValue := 0;

  if (FBorderWidth = NewValue) then
    Exit;

  FBorderWidth := NewValue;
  Invalidate;
end;


procedure TMfBeatLed.SetHoldTimeMs(const Value: Integer);
var
  NewValue: Integer;

begin

  NewValue := Value;
  if (NewValue < 0) then
    NewValue := 0;

  if (FHoldTimeMs = NewValue) then
    Exit;

  FHoldTimeMs := NewValue;
end;


procedure TMfBeatLed.SetFadeTimeMs(const Value: Integer);
var
  NewValue: Integer;

begin

  NewValue := Value;

  if (NewValue < 1) then
    NewValue := 1;

  if (FFadeTimeMs = NewValue) then
    Exit;

  FFadeTimeMs := NewValue;
end;


procedure TMfBeatLed.SetTransparent(const Value: Boolean);
begin

  if (FTransparent = Value) then
    Exit;

  FTransparent := Value;
  Invalidate;
end;


procedure TMfBeatLed.SetValueInternal(const AValue: Double);
var
  NewValue: Double;

begin

  NewValue := ClampDouble(AValue,
                          0.0,
                          1.0);

  if SameValue(FValue,
               NewValue,
               0.0001) then
    Exit;

  FValue := NewValue;
  Invalidate;
end;


procedure TMfBeatLed.Reset();
begin

  FPulseActive := False;
  FPulseStart100ns := 0;
  FLastPosition100ns := 0;
  SetValueInternal(0.0);
end;


procedure TMfBeatLed.Pulse();
begin

  TriggerPulse(0);
end;


procedure TMfBeatLed.TriggerPulse(const APosition100ns: Int64);
begin

  FPulseActive := True;
  FPulseStart100ns := APosition100ns;
  FLastPosition100ns := APosition100ns;
  SetValueInternal(1.0);
end;


procedure TMfBeatLed.UpdatePulse(const APosition100ns: Int64);
var
  Hold100ns: Int64;
  Fade100ns: Int64;
  Elapsed100ns: Int64;
  TVal: Double;

begin
  FLastPosition100ns := APosition100ns;

  if not FPulseActive then
    Exit;

  Hold100ns := Int64(FHoldTimeMs) * 10000;
  Fade100ns := Int64(FFadeTimeMs) * 10000;

  Elapsed100ns := APosition100ns - FPulseStart100ns;
  if (Elapsed100ns < 0) then
    Elapsed100ns := 0;

  if (Elapsed100ns <= Hold100ns) then
    begin
      SetValueInternal(1.0);
      Exit;
    end;

  Elapsed100ns := Elapsed100ns - Hold100ns;

  if (Elapsed100ns >= Fade100ns) then
    begin
      FPulseActive := False;
      SetValueInternal(0.0);
      Exit;
    end;

  TVal := Elapsed100ns / Fade100ns;
  TVal := ClampDouble(TVal,
                      0.0,
                      1.0);

  // softer, more heartbeat-like decay
  TVal := EaseOutCubic(TVal);

  SetValueInternal(1.0 - TVal);
end;


function TMfBeatLed.GetCurrentDisplayColor(): TColor;
begin
  Result := BlendColor(FLedOffColor,
                       FLedOnColor,
                       FValue);
end;


procedure TMfBeatLed.DrawLedShape(const ACanvas: TCanvas;
                                  const R: TRect;
                                  const ABrushColor: TColor;
                                  const APenColor: TColor;
                                  const APenWidth: Integer);
var
  W: Integer;
  H: Integer;
  S: Integer;
  DrawRect: TRect;

begin

  ACanvas.Brush.Style := bsSolid;
  ACanvas.Brush.Color := ABrushColor;
  ACanvas.Pen.Color := APenColor;
  ACanvas.Pen.Width := APenWidth;

  case FLedShape of
    blsCircle:
      begin
        W := R.Right - R.Left;
        H := R.Bottom - R.Top;
        S := Min(W,
                 H);

        DrawRect.Left := R.Left + ((W - S) div 2);
        DrawRect.Top := R.Top + ((H - S) div 2);
        DrawRect.Right := DrawRect.Left + S;
        DrawRect.Bottom := DrawRect.Top + S;

        ACanvas.Ellipse(DrawRect);
      end;

    blsEllipse:
      ACanvas.Ellipse(R);

    blsRectangle:
      ACanvas.Rectangle(R);

    blsRoundRect:
      ACanvas.RoundRect(R.Left,
                        R.Top,
                        R.Right,
                        R.Bottom,
                        6,
                        6);
  end;
end;


procedure TMfBeatLed.Paint;
var
  R: TRect;
  FillColor: TColor;

begin

  R := ClientRect;

  if not FTransparent then
    begin
      Canvas.Brush.Style := bsSolid;
      Canvas.Brush.Color := Color;
      Canvas.FillRect(R);
    end;

  InflateRect(R,
              -1,
              -1);

  FillColor := GetCurrentDisplayColor();

  DrawLedShape(Canvas,
               R,
               FillColor,
               FBorderColor,
               FBorderWidth);
end;

end.
