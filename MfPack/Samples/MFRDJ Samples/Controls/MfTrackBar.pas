// FactoryX
//
// Copyright:  FactoryX. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: MfTrackBar.pas
// Kind: Pascal Unit Component
// Release date: 24-01-2019
// Language: ENU
//
// Version: 4.0.0
// Description: Single visual component: Trackbar.
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
unit MfTrackBar;

interface

uses

  {WinApi}
  WinApi.Windows,
  WinApi.Messages,
  {System}
  System.Classes,
  System.Types,
  System.Math,
  System.SysUtils,
  {Vcl}
  Vcl.Controls,
  Vcl.Graphics,
  Vcl.Imaging.pngimage;

type

  TMfTrackBarOrientation = (soHorizontal,
                            soVertical);

  TMfThumbDrawMode = (tdmNative,
                      tdmStretch);

  TMfIncreaseToward = (itRight,
                       itLeft,
                       itUp,
                       itDown);

  TMfTickLabelMode = (tlNone,
                      tlLinear,
                      tlDb);

  TMfTickPlacementH = (tphBottom,
                       tphTop,
                       tphBoth);

  TMfTickPlacementV = (tpvLeft,
                       tpvRight,
                       tpvBoth);

  TMfTrackBar = class(TCustomControl)
  private

    FMin: Integer;
    FMax: Integer;
    FPosition: Integer;

    FOrientation: TMfTrackBarOrientation;
    FIncreaseToward: TMfIncreaseToward;
    FReversed: Boolean;

    FThumbDrawMode: TMfThumbDrawMode;
    FThumbWidth: Integer;
    FThumbHeight: Integer;

    FTransparentThumb: Boolean;
    FTransparentColor: TColor;

    FAutoDefaultThumb: Boolean;

    FSmallChange: Integer;
    FLargeChange: Integer;
    FClickToJump: Boolean;

    FDetentEnabled: Boolean;
    FDetentValue: Integer;
    FDetentThreshold: Integer;

    FShowTicks: Boolean;
    FTickCount: Integer;
    FTickLabelMode: TMfTickLabelMode;
    FTickLabelDecimals: Integer;
    FDbFloor: Single;
    FDbCeil: Single;

    FTickColor: TColor;
    FTickTextColor: TColor;
    FTickLabelBackColor: TColor;
    FTickLabelsVisible: Boolean;
    FTickPlacementH: TMfTickPlacementH;
    FTickPlacementV: TMfTickPlacementV;

    FDragging: Boolean;
    FDragOffset: Integer;
    FOnChange: TNotifyEvent;

    FBackgroundPicture: TPicture;
    FThumbPictureHorz: TPicture;
    FThumbPictureVert: TPicture;
    FBackBuffer: TBitmap;
    FBackBufferDirty: Boolean;
    FStretchBackground: Boolean;

    procedure SetMinimum(Value: Integer);
    procedure SetMaximum(Value: Integer);
    procedure SetPosition(Value: Integer);
    procedure SetOrientation(Value: TMfTrackBarOrientation);
    procedure SetIncreaseToward(Value: TMfIncreaseToward);
    procedure SetReversed(Value: Boolean);
    procedure SetThumbDrawMode(Value: TMfThumbDrawMode);
    procedure SetThumbWidth(Value: Integer);
    procedure SetThumbHeight(Value: Integer);
    procedure SetTransparentThumb(Value: Boolean);
    procedure SetTransparentColor(Value: TColor);
    procedure SetAutoDefaultThumb(Value: Boolean);
    procedure SetSmallChange(Value: Integer);
    procedure SetLargeChange(Value: Integer);
    procedure SetClickToJump(Value: Boolean);
    procedure SetDetentEnabled(Value: Boolean);
    procedure SetDetentValue(Value: Integer);
    procedure SetDetentThreshold(Value: Integer);
    procedure SetShowTicks(Value: Boolean);
    procedure SetTickCount(Value: Integer);
    procedure SetTickLabelMode(Value: TMfTickLabelMode);
    procedure SetTickLabelDecimals(Value: Integer);
    procedure SetDbFloor(Value: Single);
    procedure SetDbCeil(Value: Single);
    procedure SetTickColor(Value: TColor);
    procedure SetTickTextColor(Value: TColor);
    procedure SetTickLabelBackColor(Value: TColor);
    procedure SetTickLabelsVisible(Value: Boolean);
    procedure SetTickPlacementH(Value: TMfTickPlacementH);
    procedure SetTickPlacementV(Value: TMfTickPlacementV);
    procedure SetStretchBackground(Value: Boolean);
    procedure SetBackgroundPicture(const Value: TPicture);
    procedure SetThumbPictureHorz(const Value: TPicture);
    procedure SetThumbPictureVert(const Value: TPicture);

    procedure PictureChanged(Sender: TObject);
    procedure MarkBackgroundDirty();

    function TrackRect(): TRect;
    function ThumbSize(out W, H: Integer): Boolean;
    function ThumbRectForPos(APos: Integer): TRect;
    function PosFromPoint(const P: TPoint): Integer;
    function ClampPos(APos: Integer): Integer;
    function ApplyDetent(const APos: Integer): Integer;
    function ShouldInvert: Boolean;
    function PosToFrac(const APos: Integer): Double;
    function GetActiveThumbGraphic: TGraphic;
    function GetTrackPadding: Integer;

    procedure StepBy(Delta: Integer);
    procedure DrawTicksToCanvas(ACanvas: TCanvas;
                                const TrackR: TRect);
    procedure BuildDefaultBackground(ACanvas: TCanvas;
                                     const R: TRect);
    procedure BuildDefaultThumb(ABitmap: TBitmap;
                                const AHorizontal: Boolean);
    procedure EnsureBackBuffer();
    procedure DrawThumbToCanvas(ACanvas: TCanvas;
                                const R: TRect);
    procedure InvalidateThumbDelta(const AOldRect,
                                   ANewRect: TRect);

  protected

    procedure Loaded(); override;
    procedure Paint(); override;
    procedure Resize(); override;
    procedure MouseDown(Button: TMouseButton;
                        Shift: TShiftState;
                        X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState;
                        X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton;
                      Shift: TShiftState; X, Y: Integer); override;
    procedure KeyDown(var Key: Word;
                      Shift: TShiftState); override;
    function DoMouseWheel(Shift: TShiftState;
                          WheelDelta: Integer;
                          MousePos: TPoint): Boolean; override;
    procedure DoChange(); virtual;

    procedure CMColorChanged(var Msg: TMessage); message CM_COLORCHANGED;
    procedure CMParentColorChanged(var Msg: TMessage); message CM_PARENTCOLORCHANGED;

  public

    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;

    procedure AnimateTrackBarToPosition(const ATargetPos: Integer;
                                        const ASleepMs: Cardinal);
  published

    property Align;
    property Anchors;
    property Enabled;
    property TabStop default True;
    property TabOrder;
    property Color;
    property ParentColor;
    property Font;

    property Minimum: Integer read FMin write SetMinimum default 0;
    property Maximum: Integer read FMax write SetMaximum default 100;
    property Position: Integer read FPosition write SetPosition default 0;
    property Orientation: TMfTrackBarOrientation read FOrientation write SetOrientation default soHorizontal;
    property IncreaseToward: TMfIncreaseToward read FIncreaseToward write SetIncreaseToward default itRight;
    property Reversed: Boolean read FReversed write SetReversed default False;

    property ThumbDrawMode: TMfThumbDrawMode read FThumbDrawMode write SetThumbDrawMode default tdmStretch;
    property ThumbWidth: Integer read FThumbWidth write SetThumbWidth default 19;
    property ThumbHeight: Integer read FThumbHeight write SetThumbHeight default 13;
    property TransparentThumb: Boolean read FTransparentThumb write SetTransparentThumb default True;
    property TransparentColor: TColor read FTransparentColor write SetTransparentColor default clFuchsia;
    property AutoDefaultThumb: Boolean read FAutoDefaultThumb write SetAutoDefaultThumb default True;

    property BackgroundPicture: TPicture read FBackgroundPicture write SetBackgroundPicture;
    property ThumbPictureHorz: TPicture read FThumbPictureHorz write SetThumbPictureHorz;
    property ThumbPictureVert: TPicture read FThumbPictureVert write SetThumbPictureVert;
    property StretchBackground: Boolean read FStretchBackground write SetStretchBackground default True;

    property ShowTicks: Boolean read FShowTicks write SetShowTicks default False;
    property TickCount: Integer read FTickCount write SetTickCount default 11;
    property TickLabelMode: TMfTickLabelMode read FTickLabelMode write SetTickLabelMode default tlNone;
    property TickLabelDecimals: Integer read FTickLabelDecimals write SetTickLabelDecimals default 0;
    property DbFloor: Single read FDbFloor write SetDbFloor;
    property DbCeil: Single read FDbCeil write SetDbCeil;
    property TickColor: TColor read FTickColor write SetTickColor default clGrayText;
    property TickTextColor: TColor read FTickTextColor write SetTickTextColor default clGrayText;
    property TickLabelBackColor: TColor read FTickLabelBackColor write SetTickLabelBackColor default clNone;
    property TickLabelsVisible: Boolean read FTickLabelsVisible write SetTickLabelsVisible default True;
    property TickPlacementH: TMfTickPlacementH read FTickPlacementH write SetTickPlacementH default tphBottom;
    property TickPlacementV: TMfTickPlacementV read FTickPlacementV write SetTickPlacementV default tpvLeft;

    property SmallChange: Integer read FSmallChange write SetSmallChange default 1;
    property LargeChange: Integer read FLargeChange write SetLargeChange default 10;
    property ClickToJump: Boolean read FClickToJump write SetClickToJump default True;

    property DetentEnabled: Boolean read FDetentEnabled write SetDetentEnabled default False;
    property DetentValue: Integer read FDetentValue write SetDetentValue default 0;
    property DetentThreshold: Integer read FDetentThreshold write SetDetentThreshold default 1;

    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnDblClick;
  end;

procedure Register;


implementation


procedure Register;
begin

  RegisterComponents('RDJ Controls',
                     [TMfTrackBar]);
end;


{ TMfTrackBar }

constructor TMfTrackBar.Create(AOwner: TComponent);
begin

  inherited;

  ControlStyle := ControlStyle + [csOpaque, csDoubleClicks];
  Width := 204;
  Height := 45;
  TabStop := True;
  DoubleBuffered := True;
  Color := clBtnFace;

  FMin := 0;
  FMax := 100;
  FPosition := 0;

  FOrientation := soHorizontal;
  FIncreaseToward := itRight;
  FReversed := False;

  FThumbDrawMode := tdmStretch;
  FThumbWidth := 19;
  FThumbHeight := 13;
  FTransparentThumb := True;
  FTransparentColor := clFuchsia;
  FAutoDefaultThumb := True;

  FSmallChange := 1;
  FLargeChange := 10;
  FClickToJump := True;

  FDetentEnabled := False;
  FDetentValue := 0;
  FDetentThreshold := 1;

  FShowTicks := False;
  FTickCount := 11;
  FTickLabelMode := tlNone;
  FTickLabelDecimals := 0;
  FDbFloor := -50.0;
  FDbCeil := 6.0;
  FTickColor := clGrayText;
  FTickTextColor := clGrayText;
  FTickLabelBackColor := clNone;
  FTickLabelsVisible := True;
  FTickPlacementH := tphBottom;
  FTickPlacementV := tpvLeft;

  FBackgroundPicture := TPicture.Create;
  FBackgroundPicture.OnChange := PictureChanged;
  FThumbPictureHorz := TPicture.Create;
  FThumbPictureHorz.OnChange := PictureChanged;
  FThumbPictureVert := TPicture.Create;
  FThumbPictureVert.OnChange := PictureChanged;
  FBackBuffer := TBitmap.Create;
  FBackBuffer.PixelFormat := pf32bit;
  FBackBufferDirty := True;
  FStretchBackground := True;
end;


destructor TMfTrackBar.Destroy;
begin

  FBackBuffer.Free;
  FThumbPictureVert.Free;
  FThumbPictureHorz.Free;
  FBackgroundPicture.Free;

  inherited;
end;


procedure TMfTrackBar.Loaded();
begin

  inherited;

  MarkBackgroundDirty();
end;


procedure TMfTrackBar.Resize();
begin

  inherited;

  MarkBackgroundDirty();
end;


procedure TMfTrackBar.PictureChanged(Sender: TObject);
begin

  if (Sender = FBackgroundPicture) then
    MarkBackgroundDirty()
  else
    Invalidate;
end;


procedure TMfTrackBar.MarkBackgroundDirty();
begin

  FBackBufferDirty := True;

  Invalidate;
end;


function TMfTrackBar.ClampPos(APos: Integer): Integer;
begin

  Result := EnsureRange(APos,
                        FMin,
                        FMax);
end;


function TMfTrackBar.ApplyDetent(const APos: Integer): Integer;
begin

  Result := APos;

  if FDetentEnabled and (Abs(APos - FDetentValue) <= FDetentThreshold) then
    Result := FDetentValue;
end;


function TMfTrackBar.ShouldInvert(): Boolean;
begin

  if FOrientation = soHorizontal then
    Result := (FIncreaseToward = itLeft)
  else
    Result := (FIncreaseToward = itDown);

  if FReversed then
    Result := not Result;
end;


function TMfTrackBar.PosToFrac(const APos: Integer): Double;
begin

  if (FMax = FMin) then
    Result := 0.0
  else
    Result := (APos - FMin) / (FMax - FMin);

  if ShouldInvert then
    Result := 1.0 - Result;

  Result := EnsureRange(Result,
                        0.0,
                        1.0);
end;


function TMfTrackBar.GetTrackPadding(): Integer;
begin

  if FOrientation = soHorizontal then
    Result := Max(ThumbWidth div 2,
                  8)
  else
    Result := Max(ThumbHeight div 2,
                  8);
end;


function TMfTrackBar.TrackRect(): TRect;
var
  Pad: Integer;

begin

  Result := ClientRect;
  Pad := GetTrackPadding;
  InflateRect(Result,
              -Pad,
              -Pad);

  if (FOrientation = soHorizontal) then
    begin

      Result.Top := (ClientHeight div 2) - 3;
      Result.Bottom := Result.Top + 6;
    end
  else
    begin

      Result.Left := (ClientWidth div 2) - 3;
      Result.Right := Result.Left + 6;
    end;
end;


function TMfTrackBar.GetActiveThumbGraphic(): TGraphic;
begin

 if (FOrientation = soHorizontal) then
    Result := FThumbPictureHorz.Graphic
  else
    Result := FThumbPictureVert.Graphic;
end;


function TMfTrackBar.ThumbSize(out W,
                               H: Integer): Boolean;
var
  G: TGraphic;
  B: TBitmap;

begin

  G := GetActiveThumbGraphic;
  Result := Assigned(G) and not G.Empty;

  if Result and (FThumbDrawMode = tdmNative) then
    begin

      W := G.Width;
      H := G.Height;
      Exit;
    end;

  if Result then
    begin

      W := FThumbWidth;
      H := FThumbHeight;
      Exit;
    end;

  if FAutoDefaultThumb then
    begin

      B := TBitmap.Create;

      try

        BuildDefaultThumb(B,
                          FOrientation = soHorizontal);
        W := B.Width;
        H := B.Height;

        if (FThumbDrawMode = tdmStretch) then
          begin

            W := FThumbWidth;
            H := FThumbHeight;
          end;
        Result := True;
      finally

        B.Free;
      end;
    end
  else
    begin

      W := FThumbWidth;
      H := FThumbHeight;
      Result := False;
    end;
end;


function TMfTrackBar.ThumbRectForPos(APos: Integer): TRect;
var
  W, H: Integer;
  Frac: Double;
  Span: Integer;
  X, Y: Integer;
  Tr: TRect;

begin

  ThumbSize(W,
            H);

  Tr := TrackRect;
  Frac := PosToFrac(APos);

  if (FOrientation = soHorizontal) then
    begin

      Span := Max(1,
                  Tr.Width);
      X := Tr.Left + Round(frac * Span) - (W div 2);
      Y := (ClientHeight div 2) - (H div 2);
    end
  else
    begin

      Span := Max(1,
                  Tr.Height);
      Y := Tr.Bottom - Round(frac * Span) - (H div 2);
      X := (ClientWidth div 2) - (W div 2);
    end;

  Result := Rect(X,
                 Y,
                 X + W,
                 Y + H);
end;


function TMfTrackBar.PosFromPoint(const P: TPoint): Integer;
var
  Tr: TRect;
  Frac: Double;
  Work: Integer;

begin

  Tr := TrackRect;

  if (FOrientation = soHorizontal) then
    begin

      Work := EnsureRange(P.X - FDragOffset,
      Tr.Left - (ThumbWidth div 2), Tr.Right - (ThumbWidth div 2));
      Frac := (Work + (ThumbWidth div 2) - Tr.Left) / Max(1,
      Tr.Width);
    end
  else
    begin

      Work := EnsureRange(P.Y - FDragOffset,
                          Tr.Top - (ThumbHeight div 2),
                          Tr.Bottom - (ThumbHeight div 2));
      Frac := (Tr.Bottom - (Work + (ThumbHeight div 2))) / Max(1,
                                                               Tr.Height);
    end;

  if ShouldInvert then
    Frac := 1.0 - Frac;

  Result := FMin + Round(EnsureRange(Frac,
                                     0.0,
                                     1.0) * (FMax - FMin));

  Result := ApplyDetent(ClampPos(Result));
end;


procedure TMfTrackBar.BuildDefaultThumb(ABitmap: TBitmap;
                                        const AHorizontal: Boolean);
var
  R: TRect;

begin

  ABitmap.PixelFormat := pf32bit;

  if AHorizontal then
    ABitmap.SetSize(19,
                    13)
  else
    ABitmap.SetSize(13,
                    19);

  R := Rect(0,
            0,
            ABitmap.Width,
            ABitmap.Height);

  ABitmap.Canvas.Brush.Color := clFuchsia;
  ABitmap.Canvas.FillRect(R);
  ABitmap.Transparent := True;
  ABitmap.TransparentColor := clFuchsia;

  ABitmap.Canvas.Brush.Color := $00C4C4C4;
  ABitmap.Canvas.Pen.Color := $00808080;

  RoundRect(ABitmap.Canvas.Handle,
            0,
            0,
            ABitmap.Width,
            ABitmap.Height,
            6,
            6);

  ABitmap.Canvas.Pen.Color := clWhite;

  ABitmap.Canvas.MoveTo(2,
                        2);

  ABitmap.Canvas.LineTo(ABitmap.Width - 2,
                        2);
end;


procedure TMfTrackBar.BuildDefaultBackground(ACanvas: TCanvas;
                                             const R: TRect);
var
  Tr: TRect;
  MidY,
  MidX: Integer;

begin

  ACanvas.Brush.Color := Color;
  ACanvas.FillRect(R);

  Tr := TrackRect;
  ACanvas.Brush.Color := clBtnShadow;
  ACanvas.FillRect(Tr);

  if (FOrientation = soHorizontal) then
    begin

      MidY := (Tr.Top + Tr.Bottom) div 2;
      ACanvas.Pen.Color := clBlack;

      ACanvas.MoveTo(Tr.Left,
                     MidY + 1);

      ACanvas.LineTo(Tr.Right,
                     MidY + 1);

      ACanvas.Pen.Color := clSilver;

      ACanvas.MoveTo(Tr.Left,
                     MidY);

      ACanvas.LineTo(Tr.Right,
                     MidY);
    end
  else
    begin

      MidX := (Tr.Left + Tr.Right) div 2;

      ACanvas.Pen.Color := clBlack;
      ACanvas.MoveTo(MidX + 1,
                     Tr.Top);

      ACanvas.LineTo(MidX + 1,
                     Tr.Bottom);

      ACanvas.Pen.Color := clSilver;

      ACanvas.MoveTo(MidX,
                     Tr.Top);

      ACanvas.LineTo(MidX,
                     Tr.Bottom);
    end;

  DrawTicksToCanvas(ACanvas,
                    Tr);
end;


procedure TMfTrackBar.DrawTicksToCanvas(ACanvas: TCanvas;
                                        const TrackR: TRect);
var
  I: Integer;
  Frac: Double;
  PosVal: Integer;
  X,
  Y: Integer;
  Txt: string;
  Norm,
  Db: Double;
  Fmt: string;
  TickLen,
  TextOff,
  TextW,
  TextH: Integer;

const
  EPS: Double = 1.0e-12;

begin

  if (not FShowTicks) or (FTickCount < 2) then
    Exit;

  ACanvas.Font.Assign(Font);
  ACanvas.Pen.Color := FTickColor;
  ACanvas.Font.Color := FTickTextColor;
  ACanvas.Brush.Style := bsSolid;

  TickLen := 6;
  TextOff := 2;

  for I := 0 to FTickCount - 1 do
  begin

    Frac := I / (FTickCount - 1);
    PosVal := FMin + Round(Frac * (FMax - FMin));
    Frac := PosToFrac(PosVal);

    if FOrientation = soHorizontal then
      begin

        X := TrackR.Left + Round(Frac * TrackR.Width);

        if (FTickPlacementH = tphBottom) or (FTickPlacementH = tphBoth) then
          begin

            ACanvas.MoveTo(X,
                           TrackR.Bottom + 1);

            ACanvas.LineTo(X,
                           TrackR.Bottom + 1 + TickLen);
          end;

        if (FTickPlacementH = tphTop) or (FTickPlacementH = tphBoth) then
          begin

            ACanvas.MoveTo(X,
                           TrackR.Top - 1);

            ACanvas.LineTo(X,
                           TrackR.Top - 1 - TickLen);
          end;

        if (FTickLabelMode <> tlNone) and FTickLabelsVisible then
          begin

            if (FTickLabelMode = tlLinear) then
              begin

               Fmt := '0';
               if (FTickLabelDecimals > 0) then
                 Fmt := Fmt + '.' + StringOfChar('0',
                                                 FTickLabelDecimals);
               Txt := FormatFloat(Fmt,
                                  PosVal);
              end
            else
              begin

                Norm := (PosVal - FMin) / Max(1.0, (FMax - FMin));
                Norm := EnsureRange(Norm, 0.0, 1.0);
                Db := 20.0 * Log10(Max(EPS, Norm));
                Db := EnsureRange(Db, FDbFloor, FDbCeil);
                Fmt := '0';

                if (FTickLabelDecimals > 0) then
                  Fmt := Fmt + '.' + StringOfChar('0', FTickLabelDecimals);

                Txt := FormatFloat(Fmt,
                                   Db) + ' dB';
              end;

            TextW := ACanvas.TextWidth(Txt);
            TextH := ACanvas.TextHeight(Txt);

            if (FTickPlacementH = tphBottom) or (FTickPlacementH = tphBoth) then
              begin

                if (FTickLabelBackColor <> clNone) then
                  begin

                    ACanvas.Brush.Color := FTickLabelBackColor;
                    ACanvas.FillRect(Rect(X - (TextW div 2) - 2,
                                          TrackR.Bottom + 1 + TickLen + TextOff - 1,
                                          X + (TextW div 2) + 2,
                                          TrackR.Bottom + 1 + TickLen + TextOff - 1 + TextH + 2));
                  end;
                ACanvas.TextOut(X - (TextW div 2),
                                TrackR.Bottom + 1 + TickLen + TextOff,
                                Txt);
              end;

            if (FTickPlacementH = tphTop) or (FTickPlacementH = tphBoth) then
              begin

                if (FTickLabelBackColor <> clNone) then
                  begin

                    ACanvas.Brush.Color := FTickLabelBackColor;
                    ACanvas.FillRect(Rect(X - (TextW div 2) - 2,
                                     TrackR.Top - 1 - TickLen - TextOff - TextH - 1,
                                     X + (TextW div 2) + 2,
                                     TrackR.Top - 1 - TickLen - TextOff - TextH - 1 + TextH + 2));
                  end;
                ACanvas.TextOut(X - (TextW div 2),
                                TrackR.Top - 1 - TickLen - TextOff - TextH,
                                Txt);
              end;
          end;
      end
    else
      begin

        Y := TrackR.Top + Round((1.0 - Frac) * TrackR.Height);

        if (FTickPlacementV = tpvLeft) or (FTickPlacementV = tpvBoth) then
          begin

            ACanvas.MoveTo(TrackR.Left - 1,
                           Y);
            ACanvas.LineTo(TrackR.Left - 1 - TickLen,
                           Y);
          end;

        if (FTickPlacementV = tpvRight) or (FTickPlacementV = tpvBoth) then
          begin

            ACanvas.MoveTo(TrackR.Right + 1,
                           Y);
            ACanvas.LineTo(TrackR.Right + 1 + TickLen,
                           Y);
          end;

        if (FTickLabelMode <> tlNone) and FTickLabelsVisible then
          begin

            if (FTickLabelMode = tlLinear) then
              begin

                Fmt := '0';
                if (FTickLabelDecimals > 0) then
                  Fmt := Fmt + '.' + StringOfChar('0', FTickLabelDecimals);

                Txt := FormatFloat(Fmt, PosVal);
              end
            else
              begin

                Norm := (PosVal - FMin) / Max(1.0,
                                              (FMax - FMin));

                Norm := EnsureRange(Norm,
                                    0.0,
                                    1.0);

                Db := 20.0 * Log10(Max(EPS,
                                   Norm));
                Db := EnsureRange(Db,
                                  FDbFloor,
                                  FDbCeil);
               Fmt := '0';

               if (FTickLabelDecimals > 0) then
                 Fmt := Fmt + '.' + StringOfChar('0',
                                                 FTickLabelDecimals);
               Txt := FormatFloat(Fmt, Db) + ' dB';
              end;

            TextW := ACanvas.TextWidth(Txt);
            TextH := ACanvas.TextHeight(Txt);

            if (FTickPlacementV = tpvLeft) or (FTickPlacementV = tpvBoth) then
              begin

                if (FTickLabelBackColor <> clNone) then
                  begin

                    ACanvas.Brush.Color := FTickLabelBackColor;
                    ACanvas.FillRect(Rect(TrackR.Left - 1 - TickLen - TextOff - TextW - 2,
                                          Y - (TextH div 2) - 1,
                                          TrackR.Left - 1 - TickLen - TextOff + 2,
                                          Y - (TextH div 2) - 1 + TextH + 2));
                  end;

                ACanvas.TextOut(TrackR.Left - 1 - TickLen - TextOff - TextW, Y - (TextH div 2), Txt);
              end;

            if (FTickPlacementV = tpvRight) or (FTickPlacementV = tpvBoth) then
              begin

                if (FTickLabelBackColor <> clNone) then
                  begin

                    ACanvas.Brush.Color := FTickLabelBackColor;
                    ACanvas.FillRect(Rect(TrackR.Right + 1 + TickLen + TextOff - 2,
                                          Y - (TextH div 2) - 1,
                                          TrackR.Right + 1 + TickLen + TextOff - 2 + TextW + 4,
                                           Y - (TextH div 2) - 1 + TextH + 2));
                  end;

                ACanvas.TextOut(TrackR.Right + 1 + TickLen + TextOff,
                                Y - (TextH div 2),
                                Txt);
              end;
          end;
      end;
  end;
end;


procedure TMfTrackBar.EnsureBackBuffer();
var
  R: TRect;

begin

  if (Width <= 0) or (Height <= 0) then
    Exit;

  if (FBackBuffer.Width <> Width) or (FBackBuffer.Height <> Height) then
    FBackBufferDirty := True;

  if not FBackBufferDirty then
    Exit;

  FBackBuffer.SetSize(Width, Height);
  R := Rect(0, 0, Width, Height);

  if Assigned(FBackgroundPicture) and
     Assigned(FBackgroundPicture.Graphic) and
     not FBackgroundPicture.Graphic.Empty then
    begin

      FBackBuffer.Canvas.Brush.Color := Color;
      FBackBuffer.Canvas.FillRect(R);

      if FStretchBackground then
        FBackBuffer.Canvas.StretchDraw(R,
                                       FBackgroundPicture.Graphic)
      else
        FBackBuffer.Canvas.Draw(0,
                                0,
                                FBackgroundPicture.Graphic);

        if FShowTicks then
          DrawTicksToCanvas(FBackBuffer.Canvas, TrackRect);
    end
  else
    BuildDefaultBackground(FBackBuffer.Canvas,
                           R);

  FBackBufferDirty := False;
end;


procedure TMfTrackBar.DrawThumbToCanvas(ACanvas: TCanvas;
                                        const R: TRect);
var
  G: TGraphic;
  Tmp: TBitmap;

begin

  G := GetActiveThumbGraphic();

  if Assigned(G) and not G.Empty then
    begin

      if (G is TBitmap) and FTransparentThumb then
        begin

          Tmp := TBitmap.Create;

          try

            Tmp.Assign(G);
            Tmp.Transparent := True;
            Tmp.TransparentColor := ColorToRGB(FTransparentColor);

            if (FThumbDrawMode = tdmStretch) then
              ACanvas.StretchDraw(R,
                                  Tmp)
            else
              ACanvas.Draw(R.Left,
                           R.Top,
                           Tmp);
          finally

            Tmp.Free;
          end;
        end
      else
        begin

          if (FThumbDrawMode = tdmStretch) then
            ACanvas.StretchDraw(R,
                                G)
          else
            ACanvas.Draw(R.Left,
                         R.Top,
                         G);
        end;
      Exit;
    end;

  if FAutoDefaultThumb then
  begin

    Tmp := TBitmap.Create;

    try

      BuildDefaultThumb(Tmp,
                        FOrientation = soHorizontal);
      Tmp.Transparent := True;
      Tmp.TransparentColor := clFuchsia;

      if (FThumbDrawMode = tdmStretch) then
        ACanvas.StretchDraw(R,
                            Tmp)
      else
        ACanvas.Draw(R.Left,
                     R.Top,
                     Tmp);
    finally

      Tmp.Free;
    end;
  end;
end;


procedure TMfTrackBar.InvalidateThumbDelta(const AOldRect,
                                           ANewRect: TRect);
var
  R: TRect;

begin

  if not HandleAllocated then
    begin

      Invalidate();
      Exit();
    end;

  UnionRect(R,
            AOldRect,
            ANewRect);

  InflateRect(R,
              2,
              2);

  InvalidateRect(Handle,
                 @R,
                 False);
end;


procedure TMfTrackBar.Paint;
var
  Th: TRect;

begin

  EnsureBackBuffer();

  if (FBackBuffer.Width > 0) and (FBackBuffer.Height > 0) then
    Canvas.Draw(0,
                0,
                FBackBuffer)
  else
    begin

      Canvas.Brush.Color := Color;
      Canvas.FillRect(ClientRect);
    end;

  Th := ThumbRectForPos(FPosition);
  DrawThumbToCanvas(Canvas,
                    Th);
end;


procedure TMfTrackBar.MouseDown(Button: TMouseButton;
                                Shift: TShiftState; X, Y: Integer);
var
  Th: TRect;

begin

  inherited;

  if (Button <> mbLeft) then
    Exit;

  SetFocus;
  Th := ThumbRectForPos(FPosition);

  if PtInRect(Th,
              Point(X,
              Y)) then
    begin

      FDragging := True;

      if (FOrientation = soHorizontal) then
        FDragOffset := X - Th.Left
      else
        FDragOffset := Y - Th.Top;
    end
  else
    if FClickToJump then
      begin

        FDragging := True;
        if (FOrientation = soHorizontal) then
          FDragOffset := Th.Width div 2
        else
          FDragOffset := Th.Height div 2;

     Position := PosFromPoint(Point(X,
                                    Y));
      end;
end;


procedure TMfTrackBar.MouseMove(Shift: TShiftState;
                                X,
                                Y: Integer);
begin

  inherited;

  if FDragging then
    Position := PosFromPoint(Point(X,
                                   Y));
end;


procedure TMfTrackBar.MouseUp(Button: TMouseButton;
                              Shift: TShiftState;
                              X,
                              Y: Integer);
begin

  inherited;

  if (Button = mbLeft) then
    FDragging := False;
end;


procedure TMfTrackBar.StepBy(Delta: Integer);
begin

  Position := ClampPos(FPosition + Delta);
end;


procedure TMfTrackBar.KeyDown(var Key: Word;
                              Shift: TShiftState);
begin

  inherited;

  case Key of
    VK_LEFT,
    VK_DOWN:  StepBy(-FSmallChange);

    VK_RIGHT,
    VK_UP:   StepBy(+FSmallChange);

    VK_PRIOR: StepBy(+FLargeChange);
    VK_NEXT: StepBy(-FLargeChange);
    VK_HOME: Position := FMin;
    VK_END: Position := FMax;
  end;
end;


function TMfTrackBar.DoMouseWheel(Shift: TShiftState;
                                  WheelDelta: Integer;
                                  MousePos: TPoint): Boolean;
begin

  Result := inherited DoMouseWheel(Shift,
                                   WheelDelta,
                                   MousePos);

  if not Result then
    begin

      if WheelDelta > 0 then
        StepBy(FSmallChange)
      else
        if (WheelDelta < 0) then
          StepBy(-FSmallChange);
      Result := True;
    end;
end;


procedure TMfTrackBar.DoChange();
begin

  if Assigned(FOnChange) then
    FOnChange(Self);
end;


procedure TMfTrackBar.CMColorChanged(var Msg: TMessage);
begin

  inherited;

  MarkBackgroundDirty();
end;


procedure TMfTrackBar.CMParentColorChanged(var Msg: TMessage);
begin

  inherited;

  MarkBackgroundDirty();
end;


procedure TMfTrackBar.SetMinimum(Value: Integer);
begin

  if (FMin <> Value) then
    begin

      FMin := Value;
      if (FMax < FMin) then
        FMax := FMin;
      Position := ClampPos(FPosition);
      MarkBackgroundDirty();
    end;
end;

procedure TMfTrackBar.SetMaximum(Value: Integer);
begin

  if (FMax <> Value) then
    begin

      FMax := Value;
      if (FMin > FMax) then
        FMin := FMax;
      Position := ClampPos(FPosition);
      MarkBackgroundDirty();
    end;
end;


procedure TMfTrackBar.SetPosition(Value: Integer);
var
  NewPos: Integer;
  OldRect,
  NewRect: TRect;

begin

  NewPos := ApplyDetent(ClampPos(Value));

  if (FPosition <> NewPos) then
    begin

      OldRect := ThumbRectForPos(FPosition);
      FPosition := NewPos;
      NewRect := ThumbRectForPos(FPosition);

      InvalidateThumbDelta(OldRect,
                           NewRect);
      DoChange();
    end;
end;


procedure TMfTrackBar.SetOrientation(Value: TMfTrackBarOrientation);
begin

  if (FOrientation <> Value) then
    begin

      FOrientation := Value;
      if (FOrientation = soHorizontal) then
        begin

          FIncreaseToward := itRight;
          if FAutoDefaultThumb then
            begin

              FThumbWidth := 19;
              FThumbHeight := 13;
            end;
        end
      else
        begin

          FIncreaseToward := itUp;
          if FAutoDefaultThumb then
          begin

            FThumbWidth := 13;
            FThumbHeight := 19;
          end;
        end;
      MarkBackgroundDirty();
    end;
end;


procedure TMfTrackBar.SetIncreaseToward(Value: TMfIncreaseToward);
begin

  if (FIncreaseToward <> Value) then
    begin

      FIncreaseToward := Value;
      MarkBackgroundDirty();
    end;
end;


procedure TMfTrackBar.SetReversed(Value: Boolean);
begin

  if (FReversed <> Value) then
  begin

    FReversed := Value;
    MarkBackgroundDirty();
  end;
end;


procedure TMfTrackBar.SetThumbDrawMode(Value: TMfThumbDrawMode);
begin

  if (FThumbDrawMode <> Value) then
    begin

      FThumbDrawMode := Value;
      Invalidate();
    end;
end;


procedure TMfTrackBar.SetThumbWidth(Value: Integer);
begin

  Value := EnsureRange(Value,
                       4,
                       300);

  if (FThumbWidth <> Value) then
  begin

    FThumbWidth := Value;
    MarkBackgroundDirty;
  end;
end;


procedure TMfTrackBar.SetThumbHeight(Value: Integer);
begin

  Value := EnsureRange(Value,
                       4,
                       300);

  if (FThumbHeight <> Value) then
    begin

      FThumbHeight := Value;
      MarkBackgroundDirty();
    end;
end;


procedure TMfTrackBar.SetTransparentThumb(Value: Boolean);
begin

  if (FTransparentThumb <> Value) then
    begin

      FTransparentThumb := Value;
      Invalidate();
    end;
end;


procedure TMfTrackBar.SetTransparentColor(Value: TColor);
begin

  if (FTransparentColor <> Value) then
    begin

      FTransparentColor := Value;
      Invalidate();
    end;
end;


procedure TMfTrackBar.SetAutoDefaultThumb(Value: Boolean);
begin

  if (FAutoDefaultThumb <> Value) then
    begin

      FAutoDefaultThumb := Value;
      Invalidate();
    end;
end;


procedure TMfTrackBar.SetSmallChange(Value: Integer);
begin

  FSmallChange := Max(1,
                      Value);
end;


procedure TMfTrackBar.SetLargeChange(Value: Integer);
begin

  FLargeChange := Max(1,
                      Value);
end;


procedure TMfTrackBar.SetClickToJump(Value: Boolean);
begin

  FClickToJump := Value;
end;


procedure TMfTrackBar.SetDetentEnabled(Value: Boolean);
begin

  if (FDetentEnabled <> Value) then
    begin

      FDetentEnabled := Value;
      Position := ApplyDetent(FPosition);
    end;
end;


procedure TMfTrackBar.SetDetentValue(Value: Integer);
begin

  if (FDetentValue <> Value) then
    begin

      FDetentValue := Value;
      Position := ApplyDetent(FPosition);
    end;
end;


procedure TMfTrackBar.SetDetentThreshold(Value: Integer);
begin

  FDetentThreshold := Max(0,
                          Value);
end;


procedure TMfTrackBar.SetShowTicks(Value: Boolean);
begin

  if (FShowTicks <> Value) then
    begin

      FShowTicks := Value;
      MarkBackgroundDirty();
    end;
end;


procedure TMfTrackBar.SetTickCount(Value: Integer);
begin

  Value := Max(2,
               Value);

  if (FTickCount <> Value) then
    begin

      FTickCount := Value;
      MarkBackgroundDirty();
    end;
end;


procedure TMfTrackBar.SetTickLabelMode(Value: TMfTickLabelMode);
begin

  if (FTickLabelMode <> Value) then
    begin

      FTickLabelMode := Value;
      MarkBackgroundDirty();
    end;
end;


procedure TMfTrackBar.SetTickLabelDecimals(Value: Integer);
begin

  Value := EnsureRange(Value, 0, 6);

  if (FTickLabelDecimals <> Value) then
    begin

      FTickLabelDecimals := Value;
      MarkBackgroundDirty();
    end;
end;


procedure TMfTrackBar.SetDbFloor(Value: Single);
begin

  if not SameValue(FDbFloor,
                   Value,
                   1E-6) then
  begin

    FDbFloor := Value;
    MarkBackgroundDirty;
  end;
end;


procedure TMfTrackBar.SetDbCeil(Value: Single);
begin

  if not SameValue(FDbCeil,
                   Value,
                   1E-6) then
    begin

      FDbCeil := Value;
      MarkBackgroundDirty();
    end;
end;


procedure TMfTrackBar.SetTickColor(Value: TColor);
begin

  if (FTickColor <> Value) then
    begin

      FTickColor := Value;
      MarkBackgroundDirty();
    end;
end;


procedure TMfTrackBar.SetTickTextColor(Value: TColor);
begin

  if (FTickTextColor <> Value) then
    begin

      FTickTextColor := Value;
      MarkBackgroundDirty();
    end;
end;


procedure TMfTrackBar.SetTickLabelBackColor(Value: TColor);
begin

  if (FTickLabelBackColor <> Value) then
    begin

      FTickLabelBackColor := Value;
      MarkBackgroundDirty();
    end;
end;


procedure TMfTrackBar.SetTickLabelsVisible(Value: Boolean);
begin

  if (FTickLabelsVisible <> Value) then
    begin

      FTickLabelsVisible := Value;
      MarkBackgroundDirty();
    end;
end;


procedure TMfTrackBar.SetTickPlacementH(Value: TMfTickPlacementH);
begin

  if (FTickPlacementH <> Value) then
    begin

      FTickPlacementH := Value;
      MarkBackgroundDirty();
    end;
end;


procedure TMfTrackBar.SetTickPlacementV(Value: TMfTickPlacementV);
begin

  if (FTickPlacementV <> Value) then
    begin

      FTickPlacementV := Value;
      MarkBackgroundDirty();
    end;
end;


procedure TMfTrackBar.SetBackgroundPicture(const Value: TPicture);
begin

  FBackgroundPicture.Assign(Value);
  MarkBackgroundDirty();
end;


procedure TMfTrackBar.SetThumbPictureHorz(const Value: TPicture);
begin

  FThumbPictureHorz.Assign(Value);
  Invalidate();
end;


procedure TMfTrackBar.SetThumbPictureVert(const Value: TPicture);
begin

  FThumbPictureVert.Assign(Value);
  Invalidate();
end;


procedure TMfTrackBar.SetStretchBackground(Value: Boolean);
begin

  if (FStretchBackground <> Value) then
    begin

      FStretchBackground := Value;
      MarkBackgroundDirty;
    end;
end;


procedure TMfTrackBar.AnimateTrackBarToPosition(const ATargetPos: Integer;
                                                const ASleepMs: Cardinal);
begin

  TThread.CreateAnonymousThread(procedure
                                var
                                CurPos: Integer;
                                Step: Integer;

                                begin

                                  while True do
                                    begin

                                      TThread.Queue(nil,
                                                    procedure
                                                    begin

                                                      CurPos := Self.Position;
                                                    end);

                                      Sleep(ASleepMs);

                                      if (CurPos = ATargetPos) then
                                        Break;

                                      if (CurPos < ATargetPos) then
                                        Step := 1
                                      else
                                        Step := -1;

                                      TThread.Queue(nil,
                                                    procedure
                                                    begin

                                                      if (Self.Position <> ATargetPos) then
                                                        Self.Position := Self.Position + Step;
                                                    end);

                                      Sleep(ASleepMs);
                                    end;
                                end).Start;
end;

end.

