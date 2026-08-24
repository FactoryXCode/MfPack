// FactoryX
//
// Copyright: FactoryX. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: RDJ.RdjPro.Compositor.pas
// Kind: Pascal Unit
// Release date: 24-01-2026
// Language: ENU
//
// Revision Version: 4.0.0
// Description: Native scene compositor for RDJ Pro.
//     Purpose:
//       - Own a video-sized backbuffer.
//       - Draw the camera frame with correct aspect ratio.
//       - Draw transparent text overlays.
//       - Expose the final composed frame for preview now and encoding later.
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
// Remarks: Requires Windows 10 or higher.
//          This unit deliberately knows nothing about Media Foundation capture devices.
//          Media Foundation provides frames; the compositor builds the RdjPro scene.
//          Please, read documentation carefully!
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
unit RDJ.RdjPro.Compositor;

{
  RDJ.RdjPro.Compositor

  First native scene compositor for CarmenPro.

  Purpose:
    - Own a video-sized backbuffer.
    - Draw the camera frame with correct aspect ratio.
    - Draw transparent text overlays.
    - Expose the final composed frame for preview now and encoding later.

  This unit deliberately knows nothing about Media Foundation capture devices.
  Media Foundation provides frames; the compositor builds the RdjPro scene.

  Phase 1:
    Camera frame + overlay text.

  Later phases:
    - visualizer bitmap layer
    - logo / cover art layer
    - lower thirds
    - ON AIR lamp
    - output frame handoff to H264 encoder
}

interface

uses

  Winapi.Windows,
  System.SysUtils,
  System.Types,
  Vcl.Graphics;

type

  TRdjProSceneLayout = (cpslFit,
                        cpslFillCrop);

  TRdjProLayerAnchor = (cplaTopLeft,
                        cplaTopRight,
                        cplaBottomLeft,
                        cplaBottomRight,
                        cplaCenter);

  TRdjProCompositor = class
  private

    FWidth: Integer;
    FHeight: Integer;
    FBackBuffer: TBitmap;
    FOverlayText: string;
    FLayout: TRdjProSceneLayout;
    FBackgroundColor: TColor;
    FVisualizerBitmap: TBitmap;
    FVisualizerVisible: Boolean;
    FVisualizerWidth: Integer;
    FVisualizerHeight: Integer;
    FVisualizerMargin: Integer;
    FVisualizerAnchor: TRdjProLayerAnchor;

    procedure EnsureBackBuffer();
    function CalcFitRect(const ASourceW,
                               ASourceH: Integer): TRect;
    function CalcFillCropRect(const ASourceW,
                                    ASourceH: Integer): TRect;
    procedure DrawOverlayText(ACanvas: TCanvas);
    function CalcAnchoredRect(const ALayerW,
                                    ALayerH,
                                    AMargin: Integer;
                              const AAnchor: TRdjProLayerAnchor): TRect;
    procedure DrawVisualizerLayer(ACanvas: TCanvas);

  public

    constructor Create();
    destructor Destroy(); override;

    procedure SetSize(const AWidth,
                            AHeight: Integer);

    procedure Clear();
    procedure ComposeCameraFrame(ACameraFrame: TBitmap);

    procedure SetVisualizerFrame(AFrame: TBitmap);
    procedure ClearVisualizerFrame();

    function BackBuffer: TBitmap;

    property Width: Integer read FWidth;
    property Height: Integer read FHeight;

    // Fit keeps the whole camera frame visible with letterbox/pillarbox space.
    // FillCrop fills the scene and crops the overflow, useful for broadcast layouts.
    property Layout: TRdjProSceneLayout read FLayout write FLayout;

    property BackgroundColor: TColor read FBackgroundColor write FBackgroundColor;
    property OverlayText: string read FOverlayText write FOverlayText;

    property VisualizerVisible: Boolean read FVisualizerVisible write FVisualizerVisible;
    property VisualizerWidth: Integer read FVisualizerWidth write FVisualizerWidth;
    property VisualizerHeight: Integer read FVisualizerHeight write FVisualizerHeight;
    property VisualizerMargin: Integer read FVisualizerMargin write FVisualizerMargin;
    property VisualizerAnchor: TRdjProLayerAnchor read FVisualizerAnchor write FVisualizerAnchor;
  end;


implementation

{ TRdjProCompositor }

constructor TRdjProCompositor.Create();
begin

  inherited Create();

  FBackBuffer := TBitmap.Create();
  FBackBuffer.PixelFormat := pf32bit;

  FVisualizerBitmap := TBitmap.Create();
  FVisualizerBitmap.PixelFormat := pf32bit;

  FWidth := 1280;
  FHeight := 720;
  FLayout := cpslFit;
  FBackgroundColor := clBlack;
  FOverlayText := 'CarmenPro Native';
  FVisualizerVisible := False;
  FVisualizerWidth := 360;
  FVisualizerHeight := 120;
  FVisualizerMargin := 24;
  FVisualizerAnchor := cplaBottomRight;

  EnsureBackBuffer();
end;

destructor TRdjProCompositor.Destroy();
begin

  FVisualizerBitmap.Free();
  FBackBuffer.Free();

  inherited Destroy();
end;

procedure TRdjProCompositor.EnsureBackBuffer();
begin

  if (FWidth < 1) then
    FWidth := 1;

  if (FHeight < 1) then
    FHeight := 1;

  if (FBackBuffer.Width <> FWidth) or
     (FBackBuffer.Height <> FHeight) then
    begin

      FBackBuffer.PixelFormat := pf32bit;
      FBackBuffer.SetSize(FWidth,
                          FHeight);
    end;
end;


procedure TRdjProCompositor.SetSize(const AWidth,
                                       AHeight: Integer);
begin

  FWidth := AWidth;
  FHeight := AHeight;
  EnsureBackBuffer();
end;


procedure TRdjProCompositor.Clear();
begin

  EnsureBackBuffer();

  FBackBuffer.Canvas.Brush.Style := bsSolid;
  FBackBuffer.Canvas.Brush.Color := FBackgroundColor;
  FBackBuffer.Canvas.FillRect(Rect(0,
                                   0,
                                   FWidth,
                                   FHeight));
end;

function TRdjProCompositor.CalcFitRect(const ASourceW,
                                          ASourceH: Integer): TRect;
var
  ScaleX: Double;
  ScaleY: Double;
  Scale: Double;
  DrawW: Integer;
  DrawH: Integer;

begin

  Result := Rect(0,
                 0,
                 0,
                 0);

  if (ASourceW <= 0) or (ASourceH <= 0) or
     (FWidth <= 0) or (FHeight <= 0) then
    Exit;

  ScaleX := FWidth / ASourceW;
  ScaleY := FHeight / ASourceH;

  if (ScaleX < ScaleY) then
    Scale := ScaleX
  else
    Scale := ScaleY;

  DrawW := Round(ASourceW * Scale);
  DrawH := Round(ASourceH * Scale);

  Result.Left := (FWidth - DrawW) div 2;
  Result.Top := (FHeight - DrawH) div 2;
  Result.Right := Result.Left + DrawW;
  Result.Bottom := Result.Top + DrawH;
end;


function TRdjProCompositor.CalcFillCropRect(const ASourceW,
                                               ASourceH: Integer): TRect;
var
  ScaleX: Double;
  ScaleY: Double;
  Scale: Double;
  DrawW: Integer;
  DrawH: Integer;

begin

  Result := Rect(0,
                 0,
                 0,
                 0);

  if (ASourceW <= 0) or (ASourceH <= 0) or
     (FWidth <= 0) or (FHeight <= 0) then
    Exit;

  ScaleX := FWidth / ASourceW;
  ScaleY := FHeight / ASourceH;

  // FillCrop uses the larger scale factor, filling the whole scene.
  // The excess image area is cropped outside the destination bitmap bounds.
  if (ScaleX > ScaleY) then
    Scale := ScaleX
  else
    Scale := ScaleY;

  DrawW := Round(ASourceW * Scale);
  DrawH := Round(ASourceH * Scale);

  Result.Left := (FWidth - DrawW) div 2;
  Result.Top := (FHeight - DrawH) div 2;
  Result.Right := Result.Left + DrawW;
  Result.Bottom := Result.Top + DrawH;
end;


function TRdjProCompositor.CalcAnchoredRect(const ALayerW,
                                               ALayerH,
                                               AMargin: Integer;
                                            const AAnchor: TRdjProLayerAnchor): TRect;
var
  W: Integer;
  H: Integer;

begin

  W := ALayerW;
  H := ALayerH;

  if (W < 1) then
    W := 1;

  if (H < 1) then
    H := 1;

  case AAnchor of

    cplaTopLeft:
      begin

        Result.Left := AMargin;
        Result.Top := AMargin;
      end;

    cplaTopRight:
      begin

        Result.Left := FWidth - W - AMargin;
        Result.Top := AMargin;
      end;

    cplaBottomLeft:
      begin

        Result.Left := AMargin;
        Result.Top := FHeight - H - AMargin;
      end;

    cplaBottomRight:
      begin

        Result.Left := FWidth - W - AMargin;
        Result.Top := FHeight - H - AMargin;
      end;

    cplaCenter:
      begin

        Result.Left := (FWidth - W) div 2;
        Result.Top := (FHeight - H) div 2;
      end;
  else
    begin

      Result.Left := AMargin;
      Result.Top := AMargin;
    end;
  end;

  Result.Right := Result.Left + W;
  Result.Bottom := Result.Top + H;
end;


procedure TRdjProCompositor.DrawVisualizerLayer(ACanvas: TCanvas);
var
  R: TRect;

begin

  if not FVisualizerVisible then
    Exit;

  if (FVisualizerBitmap.Width <= 0) or
     (FVisualizerBitmap.Height <= 0) then
    Exit;

  R := CalcAnchoredRect(FVisualizerWidth,
                        FVisualizerHeight,
                        FVisualizerMargin,
                        FVisualizerAnchor);

  // Simple first visualizer layer: draw the supplied bitmap over the camera scene.
  // Later we can add alpha blending, rounded panels or Direct2D rendering.
  ACanvas.StretchDraw(R,
                      FVisualizerBitmap);
end;


procedure TRdjProCompositor.DrawOverlayText(ACanvas: TCanvas);
var
  X: Integer;
  Y: Integer;

begin

  if (FOverlayText = '') then
    Exit;

  X := 24;
  Y := 22;

  // Transparent overlay. Shadow first, then readable foreground text.
  ACanvas.Brush.Style := bsClear;
  ACanvas.Font.Size := 18;
  ACanvas.Font.Style := [fsBold];

  ACanvas.Font.Color := clBlack;
  ACanvas.TextOut(X + 2,
                  Y + 2,
                  FOverlayText);

  ACanvas.Font.Color := clLime;
  ACanvas.TextOut(X,
                  Y,
                  FOverlayText);

  ACanvas.Brush.Style := bsSolid;
end;


procedure TRdjProCompositor.ComposeCameraFrame(ACameraFrame: TBitmap);
var
  DrawR: TRect;

begin

  Clear();

  if Assigned(ACameraFrame) and
     (ACameraFrame.Width > 0) and
     (ACameraFrame.Height > 0) then
    begin

      case FLayout of
        cpslFit:
          DrawR := CalcFitRect(ACameraFrame.Width,
                               ACameraFrame.Height);

        cpslFillCrop:
          DrawR := CalcFillCropRect(ACameraFrame.Width,
                                    ACameraFrame.Height);
      else
        DrawR := CalcFitRect(ACameraFrame.Width,
                             ACameraFrame.Height);
      end;

      FBackBuffer.Canvas.StretchDraw(DrawR,
                                     ACameraFrame);
    end;

  DrawVisualizerLayer(FBackBuffer.Canvas);
  DrawOverlayText(FBackBuffer.Canvas);
end;


procedure TRdjProCompositor.SetVisualizerFrame(AFrame: TBitmap);
begin

  if not Assigned(AFrame) or
     (AFrame.Width <= 0) or
     (AFrame.Height <= 0) then
    begin

      ClearVisualizerFrame();
      Exit;
    end;

  FVisualizerBitmap.Assign(AFrame);
  FVisualizerVisible := True;
end;


procedure TRdjProCompositor.ClearVisualizerFrame();
begin

  FVisualizerBitmap.SetSize(0,
                            0);
  FVisualizerVisible := False;
end;


function TRdjProCompositor.BackBuffer: TBitmap;
begin

  EnsureBackBuffer();
  Result := FBackBuffer;
end;

end.

