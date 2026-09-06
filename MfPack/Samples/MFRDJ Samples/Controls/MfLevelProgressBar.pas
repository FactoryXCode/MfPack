// FactoryX
//
// Copyright:  FactoryX. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: MfLevelProgressBar.pas
// Kind: Pascal Unit Component
// Release date: 24-01-2019
// Language: ENU
//
// Version: 4.0.0
// Description: Single visual component: Replacement for themed TProgressBar.
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
// Remarks: To install the visual components, choose Install in the Project Manager.
//          Requires Windows 10 or later.
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
unit MfLevelProgressBar;

interface

uses

  {System}
  System.SysUtils,
  System.Classes,
  System.Types,
  System.Math,
  {Vcl}
  Vcl.Controls,
  Vcl.Graphics,
  Vcl.ExtCtrls;

type
  TMfProgressOrientation = (poHorizontal, poVertical);
  TMfPictureLayout = (plStretch, plTile, plCenter);

  TMfLevelProgressBar = class(TCustomControl)
  private

    FMin: Integer;
    FMax: Integer;
    FPosition: Integer;
    FOrientation: TMfProgressOrientation;
    FShowBorder: Boolean;
    FBorderColor: TColor;
    FBackgroundColor: TColor;
    FForegroundColor: TColor;
    FForegroundPicture: TPicture;
    FForegroundPictureEnabled: Boolean;
    FForegroundPictureLayout: TMfPictureLayout;
    FPadding: Integer;
    FRoundRadius: Integer;
    FOnChange: TNotifyEvent;

    procedure SetMin(const Value: Integer);
    procedure SetMax(const Value: Integer);
    procedure SetPosition(const Value: Integer);
    procedure SetOrientation(const Value: TMfProgressOrientation);
    procedure SetShowBorder(const Value: Boolean);
    procedure SetBorderColor(const Value: TColor);
    procedure SetBackgroundColor(const Value: TColor);
    procedure SetForegroundColor(const Value: TColor);
    procedure SetForegroundPicture(const Value: TPicture);
    procedure SetForegroundPictureEnabled(const Value: Boolean);
    procedure SetForegroundPictureLayout(const Value: TMfPictureLayout);
    procedure SetPadding(const Value: Integer);
    procedure SetRoundRadius(const Value: Integer);
    procedure ForegroundPictureChanged(Sender: TObject);
    function GetNormalizedProgress: Single;
    function GetBarRect(): TRect;
    function GetFillRect(const ABarRect: TRect): TRect;
    procedure DoChange();

  protected

    procedure Paint(); override;
    procedure Resize(); override;
    procedure DrawRoundedFrame(const R: TRect;
                               AColor: TColor);
    procedure DrawRoundedFill(const R: TRect;
                              AColor: TColor);
    procedure DrawPictureFill(const AFillRect: TRect);

  public

    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;

  published

    property Align;
    property Anchors;
    property Color default clBtnFace;
    property Constraints;
    property DoubleBuffered default True;
    property Enabled;
    property Font;
    property ParentColor;
    property ParentFont;
    property PopupMenu;
    property ShowHint;
    property Visible;

    property Minimum: Integer read FMin write SetMin default 0;
    property Maximum: Integer read FMax write SetMax default 100;
    property Position: Integer read FPosition write SetPosition default 0;
    property Orientation: TMfProgressOrientation read FOrientation write SetOrientation default poHorizontal;

    property ShowBorder: Boolean read FShowBorder write SetShowBorder default True;
    property BorderColor: TColor read FBorderColor write SetBorderColor default clBtnShadow;
    property BackgroundColor: TColor read FBackgroundColor write SetBackgroundColor default clBlack;
    property ForegroundColor: TColor read FForegroundColor write SetForegroundColor default clLime;

    property ForegroundPicture: TPicture read FForegroundPicture write SetForegroundPicture;
    property ForegroundPictureEnabled: Boolean read FForegroundPictureEnabled write SetForegroundPictureEnabled default False;
    property ForegroundPictureLayout: TMfPictureLayout read FForegroundPictureLayout write SetForegroundPictureLayout default plStretch;

    property Padding: Integer read FPadding write SetPadding default 1;
    property RoundRadius: Integer read FRoundRadius write SetRoundRadius default 4;

    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

procedure Register;


implementation


uses
  Winapi.Windows,
  Vcl.Imaging.pngimage;


procedure Register;
begin
  RegisterComponents('RDJ Controls', [TMfLevelProgressBar]);
end;


{ TMfLevelProgressBar }

constructor TMfLevelProgressBar.Create(AOwner: TComponent);
begin

  inherited Create(AOwner);

  ControlStyle := ControlStyle + [csOpaque, csReplicatable];
  Width := 160;
  Height := 18;
  Color := clBtnFace;
  ParentColor := False;
  DoubleBuffered := True;

  FMin := 0;
  FMax := 100;
  FPosition := 0;
  FOrientation := poHorizontal;
  FShowBorder := True;
  FBorderColor := clBtnShadow;
  FBackgroundColor := clBlack;
  FForegroundColor := clLime;
  FForegroundPictureEnabled := False;
  FForegroundPictureLayout := plStretch;
  FPadding := 1;
  FRoundRadius := 4;

  FForegroundPicture := TPicture.Create;
  FForegroundPicture.OnChange := ForegroundPictureChanged;
end;


destructor TMfLevelProgressBar.Destroy;
begin

  FreeAndNil(FForegroundPicture);

  inherited Destroy;
end;


procedure TMfLevelProgressBar.DoChange;
begin

  if Assigned(FOnChange) then
    FOnChange(Self);
end;


procedure TMfLevelProgressBar.DrawPictureFill(const AFillRect: TRect);
var
  SaveIndex: Integer;
  Graphic: TGraphic;
  X: Integer;
  Y: Integer;
  W: Integer;
  H: Integer;
  SrcRect: TRect;
  DstRect: TRect;

begin

  if IsRectEmpty(AFillRect) then
    Exit;

  Graphic := FForegroundPicture.Graphic;
  if (Graphic = nil) or Graphic.Empty then
    begin

      DrawRoundedFill(AFillRect,
                      FForegroundColor);
      Exit;
    end;

  SaveIndex := SaveDC(Canvas.Handle);
  try

    IntersectClipRect(Canvas.Handle,
                      AFillRect.Left,
                      AFillRect.Top,
                      AFillRect.Right,
                      AFillRect.Bottom);

    case FForegroundPictureLayout of
      plStretch:
        begin

          Canvas.StretchDraw(AFillRect,
                             Graphic);
        end;

      plTile:
        begin

          W := Graphic.Width;
          H := Graphic.Height;

          if (W <= 0) or (H <= 0) then
          begin

            DrawRoundedFill(AFillRect,
                            FForegroundColor);
            Exit;
          end;

          Y := AFillRect.Top;
          while (Y < AFillRect.Bottom) do
            begin

              X := AFillRect.Left;
              while (X < AFillRect.Right) do
                begin

                  DstRect := Rect(X,
                                  Y,
                                  X + W,
                                  Y + H);
                  Canvas.StretchDraw(DstRect,
                                     Graphic);
                  Inc(X,
                      W);
                end;
              Inc(Y,
                  H);
            end;
        end;

      plCenter:
        begin

          W := Graphic.Width;
          H := Graphic.Height;

          if (W <= 0) or (H <= 0) then
            begin

              DrawRoundedFill(AFillRect,
                              FForegroundColor);
              Exit;
            end;

          if (W > (AFillRect.Right - AFillRect.Left)) or
             (H > (AFillRect.Bottom - AFillRect.Top)) then
            begin

              SrcRect := Rect(0,
                              0,
                              Graphic.Width,
                              Graphic.Height);
              Canvas.StretchDraw(AFillRect,
                                 Graphic);
            end
          else
            begin

              DstRect := Rect(AFillRect.Left + ((AFillRect.Right - AFillRect.Left - W) div 2),
                              AFillRect.Top + ((AFillRect.Bottom - AFillRect.Top - H) div 2),
                              AFillRect.Left + ((AFillRect.Right - AFillRect.Left - W) div 2) + W,
                              AFillRect.Top + ((AFillRect.Bottom - AFillRect.Top - H) div 2) + H);
              Canvas.StretchDraw(DstRect,
                                 Graphic);
            end;
        end;
    end;
  finally

    RestoreDC(Canvas.Handle,
              SaveIndex);
  end;
end;


procedure TMfLevelProgressBar.DrawRoundedFill(const R: TRect;
                                              AColor: TColor);
begin

  if IsRectEmpty(R) then
    Exit;

  Canvas.Brush.Style := bsSolid;
  Canvas.Brush.Color := AColor;
  Canvas.Pen.Style := psClear;

  if FRoundRadius > 0 then
    Canvas.RoundRect(R.Left,
                     R.Top,
                     R.Right,
                     R.Bottom,
                     FRoundRadius * 2,
                     FRoundRadius * 2)
  else

    Canvas.FillRect(R);
end;


procedure TMfLevelProgressBar.DrawRoundedFrame(const R: TRect;
                                               AColor: TColor);
begin

  if not FShowBorder then
    Exit;

  Canvas.Brush.Style := bsClear;
  Canvas.Pen.Style := psSolid;
  Canvas.Pen.Color := AColor;

  if (FRoundRadius > 0) then
    Canvas.RoundRect(R.Left,
                     R.Top,
                     R.Right,
                     R.Bottom,
                     FRoundRadius * 2,
                     FRoundRadius * 2)
  else
    Canvas.Rectangle(R);
end;


procedure TMfLevelProgressBar.ForegroundPictureChanged(Sender: TObject);
begin

  Invalidate;
end;


function TMfLevelProgressBar.GetBarRect(): TRect;
var
  LPad: Integer;

begin

  LPad := Max(FPadding,
              0);
  Result := Rect(LPad,
                 LPad,
                 Width - LPad,
                 Height - LPad);

  if (Result.Right < Result.Left) then
    Result.Right := Result.Left;

  if (Result.Bottom < Result.Top) then
    Result.Bottom := Result.Top;
end;


function TMfLevelProgressBar.GetFillRect(const ABarRect: TRect): TRect;
var
  Pct: Single;
  FillSize: Integer;

begin

  Result := ABarRect;
  Pct := GetNormalizedProgress();

  if (FOrientation = poHorizontal) then
    begin

      FillSize := Round((ABarRect.Right - ABarRect.Left) * Pct);
      Result.Right := Result.Left + FillSize;
    end
  else
    begin

      FillSize := Round((ABarRect.Bottom - ABarRect.Top) * Pct);
      Result.Top := Result.Bottom - FillSize;
    end;

  if (Result.Right < Result.Left) then
    Result.Right := Result.Left;
  if (Result.Bottom < Result.Top) then
    Result.Bottom := Result.Top;
end;


function TMfLevelProgressBar.GetNormalizedProgress(): Single;
var
  Range: Integer;
  PosValue: Integer;

begin

  Range := FMax - FMin;
  if (Range <= 0) then
    Exit(0.0);

  PosValue := EnsureRange(FPosition,
                          FMin, FMax) - FMin;
  Result := PosValue / Range;
  Result := EnsureRange(Result,
                        0.0,
                        1.0);
end;


procedure TMfLevelProgressBar.Paint;
var
  BarRect: TRect;
  FillRect: TRect;

begin
  inherited;

  Canvas.Brush.Style := bsSolid;
  Canvas.Brush.Color := Color;
  Canvas.FillRect(ClientRect);

  BarRect := GetBarRect();
  if IsRectEmpty(BarRect) then
    Exit;

  DrawRoundedFill(BarRect,
                  FBackgroundColor);

  FillRect := GetFillRect(BarRect);

  if not IsRectEmpty(FillRect) then
    begin

      if FForegroundPictureEnabled and
         Assigned(FForegroundPicture) and
         Assigned(FForegroundPicture.Graphic) and
         not FForegroundPicture.Graphic.Empty then
        DrawPictureFill(FillRect)
    else
      DrawRoundedFill(FillRect,
                      FForegroundColor);
  end;

  DrawRoundedFrame(BarRect,
                   FBorderColor);
end;


procedure TMfLevelProgressBar.Resize;
begin

  inherited Resize;

  Invalidate;
end;


procedure TMfLevelProgressBar.SetBackgroundColor(const Value: TColor);
begin

  if (FBackgroundColor = Value) then
    Exit;
  FBackgroundColor := Value;
  Invalidate;
end;


procedure TMfLevelProgressBar.SetBorderColor(const Value: TColor);
begin

  if (FBorderColor = Value) then
    Exit;
  FBorderColor := Value;
  Invalidate;
end;


procedure TMfLevelProgressBar.SetForegroundColor(const Value: TColor);
begin

  if (FForegroundColor = Value) then
    Exit;
  FForegroundColor := Value;
  Invalidate;
end;


procedure TMfLevelProgressBar.SetForegroundPicture(const Value: TPicture);
begin

  FForegroundPicture.Assign(Value);
  Invalidate;
end;


procedure TMfLevelProgressBar.SetForegroundPictureEnabled(const Value: Boolean);
begin

  if (FForegroundPictureEnabled = Value) then
    Exit;
  FForegroundPictureEnabled := Value;
  Invalidate;
end;


procedure TMfLevelProgressBar.SetForegroundPictureLayout(const Value: TMfPictureLayout);
begin

  if (FForegroundPictureLayout = Value) then
    Exit;
  FForegroundPictureLayout := Value;
  Invalidate;
end;


procedure TMfLevelProgressBar.SetMax(const Value: Integer);
var
  NewMax: Integer;

begin

  NewMax := Value;
  if (NewMax < FMin) then
    NewMax := FMin;

  if (FMax = NewMax) then
    Exit;

  FMax := NewMax;
  if (FPosition > FMax) then
    FPosition := FMax;
  Invalidate;
  DoChange;
end;


procedure TMfLevelProgressBar.SetMin(const Value: Integer);
var
  NewMin: Integer;

begin

  NewMin := Value;
  if (NewMin > FMax) then
    NewMin := FMax;

  if (FMin = NewMin) then
    Exit;

  FMin := NewMin;
  if (FPosition < FMin) then
    FPosition := FMin;
  Invalidate;
  DoChange;
end;


procedure TMfLevelProgressBar.SetOrientation(const Value: TMfProgressOrientation);
begin

  if (FOrientation = Value) then
    Exit;
  FOrientation := Value;
  Invalidate;
end;


procedure TMfLevelProgressBar.SetPadding(const Value: Integer);
var
  NewPadding: Integer;

begin

  NewPadding := Max(Value,
                    0);
  if (FPadding = NewPadding) then
    Exit;
  FPadding := NewPadding;
  Invalidate;
end;


procedure TMfLevelProgressBar.SetPosition(const Value: Integer);
var
  NewPos: Integer;

begin

  NewPos := EnsureRange(Value,
                        FMin, FMax);
  if (FPosition = NewPos) then
    Exit;

  FPosition := NewPos;
  Invalidate;
  DoChange;
end;


procedure TMfLevelProgressBar.SetRoundRadius(const Value: Integer);
var
  NewRadius: Integer;

begin

  NewRadius := Max(Value,
                   0);
  if (FRoundRadius = NewRadius) then
    Exit;
  FRoundRadius := NewRadius;
  Invalidate;
end;


procedure TMfLevelProgressBar.SetShowBorder(const Value: Boolean);
begin

  if (FShowBorder = Value) then
    Exit;
  FShowBorder := Value;
  Invalidate;
end;

end.
