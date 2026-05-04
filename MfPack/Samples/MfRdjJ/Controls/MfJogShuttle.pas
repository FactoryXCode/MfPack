// FactoryX
//
// Copyright:  FactoryX. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: MfJogShuttle.pas
// Kind: Pascal Unit Component
// Release date: 24-01-2019
// Language: ENU
//
// Version: 3.2.0
// Description: Single visual component: Jogshuttle {TODO: need some polish}.
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
unit MfJogShuttle;

interface

uses

  {Winapi}
  Winapi.Windows,
  Winapi.Messages,
  {System}
  System.SysUtils,
  System.Classes,
  System.Types,
  System.Math,
  {Vcl}
  Vcl.Controls,
  Vcl.Graphics,
  Vcl.ComCtrls;

const

  MaxPixelCount = 32768;

type

  pRGBArray = ^TRGBArray;
  TRGBArray = array[0..MaxPixelCount - 1] of TRGBTriple;

  TJogMode = (jmAbsolute,
              jmJogRelative);

  TJogChangeEvent = procedure(Sender: TObject;
                              const NewPosition: Integer) of object;
  TJogDeltaEvent  = procedure(Sender: TObject;
                              const Delta: Integer) of object;

  { TJogShuttleProgress

    A jog/shuttle wheel that exposes the same core properties as TProgressBar
    (Min/Max/Position/Step) so it can be wired like a scrubber.

    - jmAbsolute: dragging sets Position directly.
    - jmJogRelative: dragging produces Delta steps (relative), Position changes by Delta.

    Optional linking:
      ProgressBar: if assigned, Position is mirrored to ProgressBar.Position.
  }

  TJogShuttleProgress = class(TCustomControl)
  private

    FPicture: TPicture;
    FTransparent: Boolean;

    FBitmapOriginal: TBitmap;
    FBitmapRotated: TBitmap;
    FPictureLoaded: Boolean;

    FDragging: Boolean;
    FDragStartAngle: Double;
    FDragStartPos: Integer;

    FMode: TJogMode;

    FMin: Integer;
    FMax: Integer;
    FPosition: Integer;
    FStep: Integer;

    FJogStep: Integer;

    FProgressBar: TProgressBar;

    FOnChange: TJogChangeEvent;
    FOnJogDelta: TJogDeltaEvent;

    procedure PictureChanged(Sender: TObject);
    procedure SetPicture(const Value: TPicture);
    procedure SetTransparent(const Value: Boolean);

    procedure EnsureBitmaps;
    procedure LoadOriginalFromPicture;

    procedure SetMin(const Value: Integer);
    procedure SetMax(const Value: Integer);
    procedure SetPosition(const Value: Integer);
    procedure SetStep(const Value: Integer);
    procedure SetJogStep(const Value: Integer);

    procedure SetProgressBar(const Value: TProgressBar);

    function ConstrainI(const AMin,
                              AMax,
                              AValue: Integer): Integer; inline;

    function CenterPt: TPoint;
    function AngleFromPoint(const X,
                                  Y: Integer): Double; // radians, -pi..+pi
    function AngleToDegreesCW(const Theta: Double): Double; // 0..360 (clockwise)

    function PositionToAngleDeg: Double;
    function AngleDegToPosition(const Deg: Double): Integer;

    procedure RotateToAngle(const DegCW: Double);

    procedure SyncProgressBar;
    procedure DoChange;
    procedure DoJogDelta(const Delta: Integer);

  protected
    procedure Paint; override;
    procedure Resize; override;

    procedure MouseDown(Button: TMouseButton;
                        Shift: TShiftState;
                        X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState;
                        X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton;
                      Shift: TShiftState;
                      X, Y: Integer); override;
    function DoMouseWheel(Shift: TShiftState;
                          WheelDelta: Integer;
                          MousePos: TPoint): Boolean; override;

  public

    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;

    procedure StepIt();

  published

    { ProgressBar-compatible core }
    property Min: Integer read FMin write SetMin default 0;
    property Max: Integer read FMax write SetMax default 100;
    property Position: Integer read FPosition write SetPosition default 0;
    property Step: Integer read FStep write SetStep default 10;

    { Jog/shuttle extras }
    property Mode: TJogMode read FMode write FMode default jmAbsolute;
    property JogStep: Integer read FJogStep write SetJogStep default 1;

    { Optional image skin }
    property Picture: TPicture read FPicture write SetPicture;
    property Transparent: Boolean read FTransparent write SetTransparent default False;

    { Optional linking }
    property ProgressBar: TProgressBar read FProgressBar write SetProgressBar;

    { Events }
    property OnChange: TJogChangeEvent read FOnChange write FOnChange;
    property OnJogDelta: TJogDeltaEvent read FOnJogDelta write FOnJogDelta;

    { Usual control properties }
    property Align;
    property Anchors;
    property Constraints;
    property Enabled;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;

procedure Register;


implementation


procedure Register;
begin

  RegisterComponents('RDJ Controls',
                    [TJogShuttleProgress]);
end;

{ TJogShuttleProgress }

constructor TJogShuttleProgress.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  ControlStyle := ControlStyle + [csReplicatable,
                                  csOpaque,
                                  csClickEvents,
                                  csDoubleClicks];
  DoubleBuffered := True;

  Width := 110;
  Height := 110;

  FMin := 0;
  FMax := 100;
  FPosition := 0;
  FStep := 10;

  FMode := jmAbsolute;
  FJogStep := 1;

  FPicture := TPicture.Create;
  FPicture.OnChange := PictureChanged;

  FBitmapOriginal := TBitmap.Create;
  FBitmapRotated := TBitmap.Create;
  FPictureLoaded := False;
end;


destructor TJogShuttleProgress.Destroy;
begin

  FPicture.Free;
  FBitmapOriginal.Free;
  FBitmapRotated.Free;

  inherited Destroy;
end;


procedure TJogShuttleProgress.EnsureBitmaps;
begin

  if (FBitmapOriginal = nil) then
    FBitmapOriginal := TBitmap.Create;

  if (FBitmapRotated = nil) then
    FBitmapRotated := TBitmap.Create;
end;


procedure TJogShuttleProgress.PictureChanged(Sender: TObject);
begin

  FPictureLoaded := False;

  if not (csDesigning in ComponentState) then
    Invalidate
  else
    Repaint; // safe in designer
end;


procedure TJogShuttleProgress.SetPicture(const Value: TPicture);
begin

  FPicture.Assign(Value);
  PictureChanged(Self);
end;


procedure TJogShuttleProgress.SetTransparent(const Value: Boolean);
begin

  if (FTransparent <> Value) then
    begin

      FTransparent := Value;
      PictureChanged(Self);
    end;
end;


function TJogShuttleProgress.ConstrainI(const AMin,
                                        AMax,
                                        AValue: Integer): Integer;
begin

  Result := AValue;

  if (Result < AMin) then
    Result := AMin;
  if (Result > AMax) then
    Result := AMax;
end;


procedure TJogShuttleProgress.SetMin(const Value: Integer);
begin

  if FMin <> Value then
    begin

      FMin := Value;
      if (FMax < FMin) then
        FMax := FMin;
      SetPosition(FPosition);
    end;
end;


procedure TJogShuttleProgress.SetMax(const Value: Integer);
begin

  if (FMax <> Value) then
    begin

      FMax := Value;
      if (FMax < FMin) then
        FMin := FMax;
      SetPosition(FPosition);
    end;
end;


procedure TJogShuttleProgress.SetStep(const Value: Integer);
begin

  if Value <= 0 then
    FStep := 1
  else
    FStep := Value;
end;


procedure TJogShuttleProgress.SetJogStep(const Value: Integer);
begin

  if (Value <= 0) then
    FJogStep := 1
  else
    FJogStep := Value;
end;


procedure TJogShuttleProgress.SetProgressBar(const Value: TProgressBar);
begin

  if (FProgressBar <> Value) then
    begin

      FProgressBar := Value;
      SyncProgressBar;
    end;
end;


procedure TJogShuttleProgress.SyncProgressBar;
begin

  if (FProgressBar <> nil) then
    begin
      // ProgressBar ranges are Integer too; mirror best-effort.
      try

        if (FProgressBar.Min <> FMin) then
          FProgressBar.Min := FMin;
        if (FProgressBar.Max <> FMax) then
          FProgressBar.Max := FMax;
        if (FProgressBar.Position <> FPosition) then
          FProgressBar.Position := FPosition;
      except
      // ignore any design-time / invalid handle issues
      end;
    end;
end;


procedure TJogShuttleProgress.DoChange;
begin

  if Assigned(FOnChange) then
    FOnChange(Self,
              FPosition);
end;


procedure TJogShuttleProgress.DoJogDelta(const Delta: Integer);
begin

  if Assigned(FOnJogDelta) then
    FOnJogDelta(Self,
                Delta);
end;


procedure TJogShuttleProgress.SetPosition(const Value: Integer);
var
  v: Integer;

begin

  v := ConstrainI(FMin,
                  FMax,
                  Value);

  if (v <> FPosition) then
    begin

      FPosition := v;
      SyncProgressBar();

      if not (csDesigning in ComponentState) then
        Invalidate
      else
        Repaint;

      DoChange;
    end
  else
    begin

      // Still keep bar in sync.
      SyncProgressBar();
    end;
end;


procedure TJogShuttleProgress.StepIt;
begin

  SetPosition(FPosition + FStep);
end;


function TJogShuttleProgress.CenterPt: TPoint;
begin

  Result := Point(ClientWidth div 2,
                  ClientHeight div 2);
end;


function TJogShuttleProgress.AngleFromPoint(const X, Y: Integer): Double;
var
  c: TPoint;
  dx, dy: Double;

begin

  c := CenterPt;
  dx := X - c.X;
  dy := Y - c.Y;
  // Atan2 returns radians, CCW from +X axis
  Result := ArcTan2(dy,
                    dx);
end;


function TJogShuttleProgress.AngleToDegreesCW(const Theta: Double): Double;
var
  degCCW: Double;
  degCW: Double;

begin

  degCCW := Theta * 180.0 / PI; // -180..180

  // convert CCW to CW and normalize to 0..360
  degCW := -degCCW;
  while (degCW < 0) do
    degCW := degCW + 360.0;

  while (degCW >= 360.0) do
    degCW := degCW - 360.0;
  Result := degCW;
end;


function TJogShuttleProgress.PositionToAngleDeg(): Double;
var
  span: Integer;
  t: Double;

begin

  span := FMax - FMin;
  if span <= 0 then
    Exit(0);
  t := (FPosition - FMin) / span;
  Result := t * 360.0;
end;


function TJogShuttleProgress.AngleDegToPosition(const Deg: Double): Integer;
var
  span: Integer;
  t: Double;

begin

  span := FMax - FMin;
  if (span <= 0) then
    Exit(FMin);
  t := Deg / 360.0;
  Result := FMin + Round(t * span);
  Result := ConstrainI(FMin,
                       FMax,
                       Result);
end;


procedure TJogShuttleProgress.LoadOriginalFromPicture;
begin

  EnsureBitmaps();

  if (FPicture.Graphic <> nil) then
    begin

      FBitmapOriginal.Assign(FPicture.Graphic);
      if (FBitmapOriginal.PixelFormat <> pf24bit) then
        FBitmapOriginal.PixelFormat := pf24bit;
      FPictureLoaded := (FBitmapOriginal.Width > 0) and (FBitmapOriginal.Height > 0);
    end
  else
    begin

      FBitmapOriginal.SetSize(0, 0);
      FPictureLoaded := False;
    end;
end;


procedure TJogShuttleProgress.RotateToAngle(const DegCW: Double);
const
  Black: RGBTriple = (rgbtBlue: 0;
                      rgbtGreen: 0;
                      rgbtRed: 0);

var
  i,
  j: Word;
  BW,
  BH: Word;
  iAxis,
  jAxis: Integer;
  iOriginal,
  jOriginal: Integer;
  iPrime,
  jPrime: Integer;
  iPrimeRot,
  jPrimeRot: Integer;
  RowOriginal: pRGBArray;
  RowRotated: pRGBArray;
  sinT,
  cosT: Double;
  theta: Double;
  scanBytes: Integer;
  pOrigStart: Pointer;
  pOrig: Pointer;
  iRot,
  jRot: Integer;
  jPrimeSinT,
  jPrimeCosT: Double;
  rad: Double;

  // Helper
  function DegToRad(const D: Double): Double; inline;
  begin

    Result := D * PI / 180.0;
  end;

begin

  if not FPictureLoaded then
    Exit;

  // Copy original size
  FBitmapRotated.SetSize(FBitmapOriginal.Width,
                         FBitmapOriginal.Height);
  FBitmapRotated.PixelFormat := FBitmapOriginal.PixelFormat;

  iAxis := FBitmapOriginal.Width div 2;
  jAxis := FBitmapOriginal.Height div 2;

  // Clockwise angle; our rotation routine expects clockwise as negative (same as original unit)
  rad := DegToRad(DegCW);
  theta := -rad;
  sinT := Sin(theta);
  cosT := Cos(theta);

  if (FBitmapOriginal.Height < 2) then
    Exit;

  scanBytes := Integer(FBitmapOriginal.ScanLine[1]) - Integer(FBitmapOriginal.ScanLine[0]);

  BW := FBitmapOriginal.Width - 1;
  BH := FBitmapOriginal.Height - 1;
  iRot := (2 * iAxis) + 1;
  jRot := (2 * jAxis) + 1;

  RowRotated := FBitmapRotated.ScanLine[BH];
  pOrigStart := FBitmapOriginal.ScanLine[0];

  for j := BH downto 0 do
    begin

      jPrime := (2 * j) - jRot;
      jPrimeSinT := jPrime * sinT;
      jPrimeCosT := jPrime * cosT;
      pOrig := pOrigStart;

      for i := BW downto 0 do
        begin

          iPrime := (2 * i) - iRot;
          iPrimeRot := Round(iPrime * cosT - jPrimeSinT);
          iOriginal := (iPrimeRot - 1) div 2 + iAxis;

          if (iOriginal >= 0) and (iOriginal <= BW) then
            begin

              jPrimeRot := Round(iPrime * sinT + jPrimeCosT);
              jOriginal := (jPrimeRot - 1) div 2 + jAxis;

              if (jOriginal >= 0) and (jOriginal <= BH) then
                begin

                  RowOriginal := Pointer(Integer(pOrig) + (jOriginal * scanBytes));
                  RowRotated[i] := RowOriginal[iOriginal];
                end
              else
                RowRotated[i] := Black;
            end
          else
            RowRotated[i] := Black;
        end;

      Dec(Integer(RowRotated),
          scanBytes);
    end;
end;


procedure TJogShuttleProgress.Paint;
var
  r: TRect;
  deg: Double;
  bmpToDraw: TBitmap;

begin

  inherited;

  Canvas.Brush.Style := bsSolid;
  Canvas.Brush.Color := Color;
  Canvas.FillRect(ClientRect);

  if not FPictureLoaded then
    LoadOriginalFromPicture;

  if FPictureLoaded then
    begin

      deg := PositionToAngleDeg;
      RotateToAngle(deg);
      bmpToDraw := FBitmapRotated;

      r := Rect(0,
                0, ClientWidth,
                ClientHeight);

      // Keep circle aspect.
      if (ClientWidth <> ClientHeight) then
        begin

          if (ClientWidth > ClientHeight) then
            begin

              r.Left := (ClientWidth - ClientHeight) div 2;
              r.Right := r.Left + ClientHeight;
            end
          else
            begin

              r.Top := (ClientHeight - ClientWidth) div 2;
              r.Bottom := r.Top + ClientWidth;
            end;
        end;

      Canvas.StretchDraw(r,
                         bmpToDraw);
    end
  else
    begin

      // Simple fallback.
      Canvas.Pen.Style := psSolid;
      Canvas.Pen.Color := clGray;
      Canvas.Brush.Style := bsClear;
      Canvas.Ellipse(1,
                     1,
                     ClientWidth - 1,
                     ClientHeight - 1);

      Canvas.MoveTo(CenterPt.X,
                    CenterPt.Y);
      Canvas.LineTo(CenterPt.X,
                    4);
    end;
end;


procedure TJogShuttleProgress.Resize;
begin

  inherited;

  if not (csDesigning in ComponentState) then
    Invalidate
  else
    Repaint;
end;


procedure TJogShuttleProgress.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin

  inherited;

  if (Button = mbLeft) and Enabled then
    begin

      FDragging := True;
      FDragStartAngle := AngleToDegreesCW(AngleFromPoint(X,
                                                         Y));
      FDragStartPos := FPosition;
      MouseCapture := True;
    end;
end;


procedure TJogShuttleProgress.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  curAngle: Double;
  deltaDeg: Double;
  deltaPos: Integer;
  newPos: Integer;
  span: Integer;

begin

  inherited;

  if FDragging and Enabled then
    begin

      curAngle := AngleToDegreesCW(AngleFromPoint(X,
                                                  Y));

      deltaDeg := curAngle - FDragStartAngle;
      // Normalize shortest path (-180..+180)
      while (deltaDeg > 180.0) do
        deltaDeg := deltaDeg - 360.0;

      while (deltaDeg < -180.0) do
        deltaDeg := deltaDeg + 360.0;

      span := FMax - FMin;

      if (span <= 0) then
        Exit;

      if (FMode = jmAbsolute) then
        begin

          // Absolute maps cursor angle to position.
          newPos := AngleDegToPosition(curAngle);
          SetPosition(newPos);
        end
      else
        begin

          // relative jog: degrees -> steps
          deltaPos := Round((deltaDeg / 360.0) * span);
          // quantize
          if (FJogStep > 1) then
            deltaPos := (deltaPos div FJogStep) * FJogStep;

         if (deltaPos <> 0) then
           begin

             newPos := ConstrainI(FMin,
                                  FMax,
                                  FDragStartPos + deltaPos);
            SetPosition(newPos);
            DoJogDelta(deltaPos);
           end;
        end;
    end;
end;


procedure TJogShuttleProgress.MouseUp(Button: TMouseButton;
                                      Shift: TShiftState;
                                      X, Y: Integer);
begin

  inherited;

  if (Button = mbLeft) then
    begin

      FDragging := False;
      MouseCapture := False;
    end;
end;


function TJogShuttleProgress.DoMouseWheel(Shift: TShiftState;
                                          WheelDelta: Integer;
                                          MousePos: TPoint): Boolean;
var
  dir: Integer;
  d: Integer;

begin

  // VCL expects this virtual to return True when the wheel message was handled.
  Result := inherited DoMouseWheel(Shift,
                                   WheelDelta,
                                   MousePos);
  if Result then
    Exit;

  if not Enabled then
    Exit(False);

  // Standard wheel: 120 per notch
  if (WheelDelta > 0) then
    dir := 1
  else
    if (WheelDelta < 0) then
      dir := -1
    else
      dir := 0;

  if (dir <> 0) then
    begin

      d := dir * FJogStep;
      SetPosition(FPosition + d);
      DoJogDelta(d);
      Result := True;
    end
  else
    Result := False;
end;

end.
