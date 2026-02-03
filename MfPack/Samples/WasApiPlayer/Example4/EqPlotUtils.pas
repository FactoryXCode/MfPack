// FactoryX
//
// Copyright:  FactoryX. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: EqPlotUtils.pas
// Kind: Pascal Unit
// Release date: 24-01-2026
// Language: ENU
//
// Revision Version: 3.1.9
// Description: Math operations/calculations for the 3 band EQ.
//
// Notes:
//  - Designed for real-time use in the WASAPI render thread.
// Company: FactoryX
// Intiator(s): Tony (maXcomX).
// Contributor(s): Tony Kalf (maXcomX)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 01/13/2026 All                 Sineead O'Connor release  SDK 10.0.26100.4654 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 (2H20) or later.
//
// Related objects: -
// Related projects: MfPackX319/Samples/WasApiPlayer/Example4
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
unit EqPlotUtils;

interface

uses
  WinApi.Windows,
  Vcl.Graphics,
  MfAudioHighMidLowTypes;

type

  TComplex = record

    var
      Re, Im: Double;

    class function Make(const ARe,
                        AIm: Double): TComplex; static;

    class operator Add(const A,
                       B: TComplex): TComplex;

    class operator Subtract(const A,
                            B: TComplex): TComplex;

    class operator Multiply(const A,
                            B: TComplex): TComplex;

    class operator Divide(const A,
                          B: TComplex): TComplex;
    function Abs_(): Double;
  end;

  function BiquadResponse(const Bq: TBiquadCoeffs;
                          const Fs,
                          FreqHz: Double): TComplex;

  procedure PlotEqResponse(Canvas: TCanvas;
                           const R: TRect;
                           const Fs: Double;
                           const LowBq,
                           MidBq,
                           HighBq: TBiquadCoeffs);


implementation

uses
  System.Math;


class function TComplex.Make(const ARe,
                             AIm: Double): TComplex;
begin

  Result.Re := ARe;
  Result.Im := AIm;
end;


class operator TComplex.Add(const A,
                            B: TComplex): TComplex;
begin

  Result := TComplex.Make(A.Re + B.Re, A.Im + B.Im);
end;


class operator TComplex.Subtract(const A,
                                 B: TComplex): TComplex;
begin

  Result := TComplex.Make(A.Re - B.Re, A.Im - B.Im);
end;

class operator TComplex.Multiply(const A, B: TComplex): TComplex;
begin

  Result := TComplex.Make(A.Re*B.Re - A.Im*B.Im, A.Re*B.Im + A.Im*B.Re);
end;


class operator TComplex.Divide(const A,
                               B: TComplex): TComplex;
var
  denominator: Double;

begin

  denominator := B.Re * B.Re + B.Im * B.Im;

  if (denominator = 0) then
    Exit(TComplex.Make(0,
                       0));

  Result := TComplex.Make((A.Re * B.Re + A.Im * B.Im) / denominator,
                          (A.Im * B.Re - A.Re * B.Im) / denominator);
end;


function TComplex.Abs_(): Double;
begin

  Result := Sqrt(Re * Re + Im*Im);
end;


function BiquadResponse(const Bq: TBiquadCoeffs;
                        const Fs,
                        FreqHz: Double): TComplex;
var
  w: Double;
  e1,
  e2: TComplex;  // e^{-jω}, e^{-j2ω}
  c,
  s: Double;
  numerator,
  denominator: TComplex;

begin

  if (Fs <= 0) or (FreqHz <= 0) then
    Exit(TComplex.Make(1,
                       0));

  w := 2 * Pi * FreqHz / Fs;

  // e^{-jω} = cos(w) - j sin(w)
  c := Cos(w);
  s := Sin(w);
  e1 := TComplex.Make(c,
                      -s);

  // e^{-j2ω}
  c := Cos(2 * w);
  s := Sin(2 * w);
  e2 := TComplex.Make(c,
                      -s);

  numerator := TComplex.Make(Bq.a0,
                             0) +
               TComplex.Make(Bq.a1,
                             0) * e1 +
               TComplex.Make(Bq.a2,
                             0) * e2;

  denominator := TComplex.Make(1.0,
                               0) +
                 TComplex.Make(Bq.b1,
                               0) * e1 +
                 TComplex.Make(Bq.b2,
                               0) * e2;

  Result := (numerator / denominator);
end;


procedure PlotEqResponse(Canvas: TCanvas;
                         const R: TRect;
                         const Fs: Double;
                         const LowBq, MidBq, HighBq: TBiquadCoeffs);
const
  Fmin   = 10.0;
  Fmax   = 22000.0;
  Points = 400;
  DbMin  = -24.0;
  DbMax  =  24.0;

var
  i: Integer;
  t,
  f: Double;
  H,
  Hl,
  Hm,
  Hh: TComplex;
  mag,
  db: Double;
  x,
  y: Integer;

begin

  // Background
  Canvas.Brush.Style := bsSolid;
  Canvas.Brush.Color := $00001A00;
  Canvas.FillRect(R);

  // Curve styling.
  Canvas.Pen.Style := psSolid;
  Canvas.Pen.Width := 1;
  Canvas.Pen.Color := clLime;   // or clWhite

  for i := 0 to Points - 1 do
    begin

      t := i / (Points - 1);
      f := Fmin * Power(Fmax / Fmin,
                        t);

      Hl := BiquadResponse(LowBq,
                           Fs,
                           f);

      Hm := BiquadResponse(MidBq,
                           Fs,
                           f);

      Hh := BiquadResponse(HighBq,
                           Fs,
                           f);

      H := Hl * Hm * Hh;

      mag := H.Abs_;

      if (mag < 1e-12) then
        mag := 1e-12;

      db := 20 * Log10(mag);
      db := EnsureRange(db,
                        DbMin,
                        DbMax);

      x := R.Left + Round(t * (R.Width - 1));
      y := R.Bottom - Round((db - DbMin) / (DbMax - DbMin) * (R.Height - 1));

      if (i = 0) then
        Canvas.MoveTo(x, y)
      else
        Canvas.LineTo(x, y);
  end;
end;


end.
