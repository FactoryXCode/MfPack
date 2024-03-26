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
//  Copyright © 2003-2023 Renate Schaaf
//==============================================================================


unit Common;

interface

{$WARN SYMBOL_PLATFORM OFF}

uses
  {System}
  System.Types,
  System.UITypes,
  System.Threading,
  System.SysUtils,
  System.Classes,
  System.Math,
  System.SyncObjs;

{$IFOPT O-}
{$DEFINE O_MINUS}
{$O+}
{$ENDIF}
{$IFOPT Q+}
{$DEFINE Q_PLUS}
{$Q-}
{$ENDIF}



type
  // Filter types
  TFilter = (cfBox,
             cfBilinear,
             cfBicubic,
             cfMine,
             cfLanczos,
             cfBSpline);

  TPrecision = (prLow,
                prHigh);
const
  // Default radii for the filters, can be made a tad smaller for performance
  DefaultRadius: array [TFilter] of Single = (0.5,
                                              1,
                                              2,
                                              2,
                                              3,
                                              2);

type
  // happens right now, if you use a custom thread pool which has not been initialized
  eParallelException = class(Exception);

  // amIndependent: all channels are resampled independently, pixels with alpha=0 can contribute
  // to the RGB-part of the result.
  //
  // amPreMultiply: RBG-channels are pre-multiplied by alpha-channel before resampling,
  // after that the resampled alpha-channel is divided out again, unless=0. This means that pixels
  // with alpha=0 have no contribution to the RGB-part of the result.
  //
  // amIgnore: Resampling ignores the alpha-channel and only stores RGB into target.
  // VCL-version: Target-alpha is unchanged. Useful if the alpha-channel
  // is not needed or the target already contains a custom alpha-channel which should not be changed
  // FMX-version: Target-alpha is set to 255. Otherwise the FMX-controls would not display the target.
  //
  // amTransparentColor: The source is resampled while preserving transparent parts as indicatated by TransparentColor.
  // The target can use the same color for transparency. Uses the alpha-channel only internally.
  // Currently not supported for FMX.
  TAlphaCombineMode = (amIndependent,
                       amPreMultiply,
                       amIgnore,
                       amTransparentColor);

type
  TResamplingThread = class(TThread)
  private
    fResamplingThreadProc: TProc;

  protected
    procedure Execute; override;

  public
    Wakeup: TEvent;
    Done: TEvent;
    Ready: TEvent;

    constructor Create();
    destructor Destroy(); override;

    procedure RunAnonProc(aProc: TProc);
  end;

  TThreadArray = array of TResamplingThread;

  // A record defining a simple thread pool. A pointer to such a record can be
  // passed to the ZoomResampleParallelThreads procedure to indicate that this thread
  // pool should be used. This way the procedure can be used in concurrent threads.
  TResamplingThreadPool = record
  private
    fInitialized: Boolean;
    fThreadCount: Integer;
    fResamplingThreads: TThreadArray;

  public

    /// <summary> Creates the threads. Call before you use it in parallel procedures. If already initialized, it will finalize first, don't call it unnecessarily. </summary>
    procedure Initialize(aMaxThreadCount: Integer;
                         aPriority: TThreadpriority);

    /// <summary> Frees the threads. Call when your code exits the part where you use parallel resampling to free up memory and CPU-time. If you don't finalize a custom threadpool, you will have a memory leak. </summary>
    procedure Finalize();

    property ResamplingThreads: TThreadArray read fResamplingThreads;
    property Initialized: boolean read fInitialized;
    property ThreadCount: Integer read fThreadCount;
  end;

  PResamplingThreadPool = ^TResamplingThreadPool;

  TBGRA = record
    b: Byte;
    g: Byte;
    r: Byte;
    a: Byte;
  end;

  PBGRA = ^TBGRA;

  TBGRAInt = record
    b: Integer;
    g: Integer;
    r: Integer;
    a: Integer;
  end;

  PBGRAInt = ^TBGRAInt;

  TBGRAIntArray = array of TBGRAInt;

  TCacheMatrix = array of TBGRAIntArray;

  TIntArray = array of Integer;

  TContributor = record
    Min: Integer;
    High: Integer;
    // Min: start source pixel.
    // High+1: number of source pixels to contribute to the result.
    Weights: array of Integer; // floats scaled by $100 or $800.
  end;

  TContribArray = array of TContributor;

  TResamplingThreadSetup = record
    Tbps,
    Sbps: Integer; // Pitch (bytes per scanline) in Target/Source. Negative for VCL-TBitmap .
    // Contributors horizontal/vertical.
    ContribsX: TContribArray;
    ContribsY: TContribArray;
    // Scanline for row 0.
    rStart: PByte;
    rTStart: PByte;

    xmin: Integer;
    xmax: Integer;
    ThreadCount: Integer;
    xminSource: Integer;
    xmaxSource: Integer;

    // ymin,ymax for each thread.
    ymin: TIntArray;
    ymax: TIntArray;

    CacheMatrix: TCacheMatrix; // Cache for result of vertical pass, 1 array for each thread.

    procedure PrepareResamplingThreads(NewWidth: Integer;
                                       NewHeight: Integer;
                                       OldWidth: Integer;
                                       OldHeight: Integer;
                                       Radius: Single;
                                       Filter: TFilter;
                                       SourceRect: TRectF;
                                       AlphaCombineMode: TAlphaCombineMode;
                                       aMaxThreadCount: Integer;
                                       SourcePitch: Integer;
                                       TargetPitch: Integer;
                                       SourceStart: PByte;
                                       TargetStart: PByte);
  end;

  //PResamplingThreadSetup = ^TResamplingThreadSetup;

var
  _DefaultThreadPool: TResamplingThreadPool;
  _IsFMX: Boolean; // Value is set in initialization of uScale and uScaleFMX.

const
  // Constants used to divide the work for threading.
  _ChunkHeight: Integer = 8;
  _MaxThreadCount: Integer = 64;



  // Inline methods.
  function Box(x: Double): Double; inline;
  function Linear(x: Double): Double; inline;
  function BSpline(x: Double): Double; inline;
  function Bicubic(x: Double): Double; inline;
  function Lanczos(x: Double): Double; inline;

  procedure Combine(const ps: PBGRA;
                    const Weight: Integer;
                    const Cache: PBGRAInt;
                    const acm: TAlphaCombineMode); inline;

  procedure Increase(const ps: PBGRA;
                     const Weight: Integer;
                     const Cache: PBGRAInt;
                     const acm: TAlphaCombineMode); inline;

  procedure InitTotal(const Cache: PBGRAInt;
                      const Weight: Integer;
                      var Total: TBGRAInt;
                      const acm: TAlphaCombineMode); inline;

  procedure IncreaseTotal(const Cache: PBGRAInt;
                          const Weight: Integer;
                          var Total: TBGRAInt;
                          const acm: TAlphaCombineMode); inline;

  procedure ClampIndependent(const Total: TBGRAInt;
                             const pT: PBGRA); inline;

  procedure ClampIgnore(const Total: TBGRAInt;
                        const pT: PBGRA); inline;

  procedure ClampPreMult(const Total: TBGRAInt;
                         const pT: PBGRA); inline;

  procedure ProcessRow(y: Integer;
                       CacheStart: PBGRAInt;
                       const RTS: TResamplingThreadSetup;
                       AlphaCombineMode: TAlphaCombineMode); inline;


  function Mine(x: Double): Double;

  procedure MakeContributors(r: Single;
                             SourceSize: Integer;
                             TargetSize: Integer;
                             SourceStart: Double;
                             SourceFloatwidth: Double;
                             Filter: TFilter;
                             precision: TPrecision;
                             var Contribs: TContribArray);

  /// <summary> Initializes the default resampling thread pool. If already initialized, it does nothing. If not called, the default pool is initialized at the first use of a parallel procedure, causing a delay. </summary>
  procedure InitDefaultResamplingThreads();

  /// <summary> Frees the default resampling threads. If they are initialized and not finalized the Finalization of uScale(FMX) will do it. </summary>
  procedure FinalizeDefaultResamplingThreads();


implementation

type
  TFilterFunction = function(x: Double): Double;

const
  FilterFunctions: array [TFilter] of TFilterFunction = (Box,
                                                         Linear,
                                                         Bicubic,
                                                         Mine,
                                                         Lanczos,
                                                         BSpline);

  PrecisionFacts: array [TPrecision] of Integer = ($100,
                                                   $800);
  PreMultPrecision = 1 shl 2;

  PointCount = 18; // 6 would be Simpson's rule, but I like emphasis on midpoint
  PointCountMinus2 = PointCount - 2;
  PointCountInv = 1 / PointCount;

  // Follow the filter functions.
  // They actually never get inlined, because
  // MakeContributors uses a procedural variable,
  // but their use is not in a time-critical spot.
function Box(x: Double): double; inline;
begin
  x := abs(x);
  if x > 1 then
    Result := 0
  else
    Result := 0.5;
end;


function Linear(x: Double): Double; inline;
begin
  x := abs(x);
  if x < 1 then
    Result := 1 - x
  else
    Result := 0;
end;


function BSpline(x: Double): Double; inline;
begin
  x := abs(x);
  if x < 0.5 then
    Result := 8 * x * x * (x - 1) + 4 / 3
  else if x < 1 then
    Result := 8 / 3 * sqr(1 - x) * (1 - x)
  else
    Result := 0;
end;


const
  beta = 0.52;
  beta2 = beta * beta;
  alpha = 105 / (16 - 112 * beta2);
  aa = 1 / 7 * alpha;
  bb = -1 / 5 * alpha * (2 + beta2);
  cc = 1 / 3 * alpha * (1 + 2 * beta2);
  dd = -alpha * beta2;


function Mine(x: Double): Double;
begin
  x := Abs(x);
  if (x > 1) then
    Result := 0
  else
    Result := 7 * aa * x * x * x * x * x * x + 5 * bb * x * x * x * x + 3 * cc * Sqr(x) + dd;
end;


const
  ac = -2;


function Bicubic(x: Double): Double; inline;
begin
  x := abs(x);
  if x < 1 / 2 then
    Result := 4 * (ac + 8) * x * x * x - 2 * (ac + 12) * x * x + 2
  else if x < 1 then
    Result := 2 * ac * (2 * x * x * x - 5 * x * x + 4 * x - 1)
  else
    Result := 0;
end;


function Lanczos(x: double): double; inline;
var
  y, yinv: double;
begin
  x := abs(x);
  if x = 0 then
    Result := 3
  else if x < 1 then
  begin
    y := Pi * x;
    yinv := 1 / y;
    Result := sin(3 * y) * sin(y) * yinv * yinv;
  end
  else
    Result := 0;
end;


procedure MakeContributors(r: Single;
                           SourceSize: Integer;
                           TargetSize: Integer;
                           SourceStart: Double;
                           SourceFloatwidth: Double;
                           Filter: TFilter;
                           precision: TPrecision;
                           var Contribs: TContribArray);

var
  xCenter, scale, rr: double;
  x, j: Integer;
  x1, x2, x0, x3, delta, dw: double;
  TrueMin, TrueMax, Mx, prec: Integer;
  sum, ds: Integer;
  FT: TFilterFunction;

begin

  if SourceFloatwidth = 0 then
    SourceFloatwidth := SourceSize;

  scale := SourceFloatwidth / TargetSize;
  prec := PrecisionFacts[precision];

  SetLength(Contribs,
            TargetSize);

  FT := FilterFunctions[Filter];

  if scale > 1 then
    // downsampling
    rr := r * scale
  else
    // upsampling
    rr := r;
  delta := 1 / rr;

  for x := 0 to TargetSize - 1 do
  begin
    xCenter := (x + 0.5) * scale;
    TrueMin := Ceil(xCenter - rr + SourceStart - 1);
    TrueMax := Floor(xCenter + rr + SourceStart);
    Contribs[x].Min := Min(max(TrueMin, 0), SourceSize - 1);
    // make sure not to read in negative pixel locations
    Mx := max(Min(TrueMax, SourceSize - 1), 0);
    // make sure not to read past w1-1 in the source
    Contribs[x].High := Mx - Contribs[x].Min;
    Assert(Contribs[x].High >= 0);
    // High=Number of contributing pixels minus 1
    SetLength(Contribs[x].Weights, Contribs[x].High + 1);
    sum := 0;
    with Contribs[x] do
    begin
      x0 := delta * (Min - SourceStart - xCenter + 0.5);
      for j := 0 to High do
      begin
        x1 := x0 - 0.5 * delta;
        x2 := x0 + 0.5 * delta;
        // intersect interval [x1, x2] with the support of the filter
        x1 := max(x1, -1);
        x2 := System.Math.Min(x2, 1);
        // x3 is the new center
        x3 := 0.5 * (x1 + x2);
        // Evaluate integral_x1^x2 FT(x) dx using a mixture of
        // the midpoint rule and the trapezoidal rule.
        // The midpoint parts seems to preserve details
        // while the trapezoidal part and the intersection
        // with the support of the filter prevents artefacts.
        // PointCount=6 would be Simpson's rule.
        dw := PointCountInv * (x2 - x1) *
          (FT(x1) + FT(x2) + PointCountMinus2 * FT(x3));
        // scale float to Integer, Integer=prec corresponds to float=1
        Weights[j] := Trunc(prec * dw);
        x0 := x0 + delta;
        sum := sum + Weights[j];
      end;
      for j := TrueMin - Min to -1 do
      begin
        // assume the first pixel to be repeated
        x0 := delta * (Min + j - SourceStart - xCenter + 0.5);
        x1 := x0 - 0.5 * delta;
        x2 := x0 + 0.5 * delta;
        x1 := max(x1, -1);
        x2 := System.Math.Min(x2, 1);
        x3 := 0.5 * (x1 + x2);
        dw := PointCountInv * (x2 - x1) *
          (FT(x1) + FT(x2) + PointCountMinus2 * FT(x3));
        ds := Trunc(prec * dw);
        Weights[0] := Weights[0] + ds;
        sum := sum + ds;
      end;
      for j := High + 1 to TrueMax - Min do
      begin
        // assume the last pixel to be repeated
        x0 := delta * (Min + j - SourceStart - xCenter + 0.5);
        x1 := x0 - 0.5 * delta;
        x2 := x0 + 0.5 * delta;
        x1 := max(x1, -1);
        x2 := System.Math.Min(x2, 1);
        x3 := 0.5 * (x1 + x2);
        dw := PointCountInv * (x2 - x1) *
          (FT(x1) + FT(x2) + PointCountMinus2 * FT(x3));
        ds := Trunc(prec * dw);
        Weights[High] := Weights[High] + ds;
        sum := sum + ds;
      end;
      // make sure weights sum up to prec
      Weights[High div 2] := Weights[High div 2] + prec - sum;
    end;
    { with Contribs[x] }
  end; { for x }
end;



procedure Combine(const ps: PBGRA;
                  const Weight: Integer;
                  const Cache: PBGRAInt;
                  const acm: TAlphaCombineMode); inline;
var
  alpha: Integer;

begin
  if acm in [amIndependent, amIgnore] then
  begin
    Cache.b := Weight * ps.b;
    Cache.g := Weight * ps.g;
    Cache.r := Weight * ps.r;
    if acm in [amIndependent] then
      Cache.a := Weight * ps.a;
  end
  else
  begin
    if ps.a > 0 then
    begin
      alpha := Weight * ps.a;
      Cache.b := ps.b * alpha div PreMultPrecision;
      Cache.g := ps.g * alpha div PreMultPrecision;
      Cache.r := ps.r * alpha div PreMultPrecision;
      Cache.a := alpha;
    end
    else
      Cache^ := Default (TBGRAInt);
  end;
end;


procedure Increase(const ps: PBGRA;
                   const Weight: Integer;
                   const Cache: PBGRAInt;
                   const acm: TAlphaCombineMode); inline;
var
  alpha: Integer;

begin
  if (acm in [amIndependent,
             amIgnore]) then
    begin
      Inc(Cache.b,
          Weight * ps.b);

      Inc(Cache.g,
          Weight * ps.g);

      Inc(Cache.r,
          Weight * ps.r);

      if (acm = amIndependent) then
        Inc(Cache.a,
            Weight * ps.a);
    end
  else if (ps.a > 0) then
    begin
      alpha := Weight * ps.a;
      Inc(Cache.b, ps.b * alpha div PreMultPrecision);
      Inc(Cache.g, ps.g * alpha div PreMultPrecision);
      Inc(Cache.r, ps.r * alpha div PreMultPrecision);
      Inc(Cache.a, alpha);
    end;
end;


procedure InitTotal(const Cache: PBGRAInt;
                    const Weight: Integer;
                    var Total: TBGRAInt;
                    const acm: TAlphaCombineMode); inline;
begin
  if (acm in [amIndependent,
              amIgnore]) then
    begin
      Total.b := Weight * Cache.b;
      Total.g := Weight * Cache.g;
      Total.r := Weight * Cache.r;
      if (acm = amIndependent) then
        Total.a := Weight * Cache.a;
    end
  else if Cache.a <> 0 then
    begin
      Total.b := Weight * Cache.b;
      Total.g := Weight * Cache.g;
      Total.r := Weight * Cache.r;
      Total.a := Weight * Cache.a;
    end
  else
    Total := Default (TBGRAInt);
end;


procedure IncreaseTotal(const Cache: PBGRAInt;
                        const Weight: Integer;
                        var Total: TBGRAInt;
                        const acm: TAlphaCombineMode); inline;
begin
  if (acm in [amIndependent,
              amIgnore]) then
    begin
      Inc(Total.b,
          Weight * Cache.b);
      Inc(Total.g,
          Weight * Cache.g);
      Inc(Total.r,
          Weight * Cache.r);
    if (acm = amIndependent) then
      Inc(Total.a,
          Weight * Cache.a);
    end
  else if (Cache.a <> 0) then
    begin
    Inc(Total.b, Weight * Cache.b);
    Inc(Total.g, Weight * Cache.g);
    Inc(Total.r, Weight * Cache.r);
    Inc(Total.a, Weight * Cache.a);
  end;
end;


procedure ClampIndependent(const Total: TBGRAInt;
                           const pT: PBGRA); inline;
begin
  pT.b := Min((max(Total.b,
              0) + $1FFFFF) shr 22, 255);
  pT.g := Min((max(Total.g,
              0) + $1FFFFF) shr 22, 255);
  pT.r := Min((max(Total.r,
              0) + $1FFFFF) shr 22, 255);
  pT.a := Min((max(Total.a,
              0) + $1FFFFF) shr 22, 255);
end;


procedure ClampIgnore(const Total: TBGRAInt;
                      const pT: PBGRA); inline;
begin
  pT.b := Min((Max(Total.b,
              0) + $1FFFFF) shr 22, 255);
  pT.g := Min((Max(Total.g,
              0) + $1FFFFF) shr 22, 255);
  pT.r := Min((Max(Total.r,
              0) + $1FFFFF) shr 22, 255);
  if _IsFMX then
    pT.a := 255;
end;


procedure ClampPreMult(const Total: TBGRAInt;
                       const pT: PBGRA); inline;
var
  alpha: byte;

begin
  alpha := Min((Max(Total.a,
                    0) + $7FFF) shr 16, 255);
  if (alpha > 0) then
    begin
      pT.b := Min((Max(Total.b div alpha,
                       0) + $1FFF) shr 14, 255);
      pT.g := Min((Max(Total.g div alpha,
                       0) + $1FFF) shr 14, 255);
      pT.r := Min((Max(Total.r div alpha,
                       0) + $1FFF) shr 14, 255);
      pT.a := alpha;
    end
  else
    pT^ := Default(TBGRA);
end;


procedure ProcessRow(y: Integer;
                     CacheStart: PBGRAInt;
                     const RTS: TResamplingThreadSetup;
                     AlphaCombineMode: TAlphaCombineMode); inline;
var
  ps: PBGRA;
  pT: PBGRA;
  rs: PByte;
  rT: PByte;
  x: Integer;
  i: Integer;
  j: Integer;
  highx: Integer;
  highy: Integer;
  minx: Integer;
  miny: Integer;
  Weightx: PInteger;
  Weighty: PInteger;
  Weight: Integer;
  Total: TBGRAInt;
  run: PBGRAInt;
  jump: Integer;

begin
  miny := RTS.ContribsY[y].Min;
  highy := RTS.ContribsY[y].High;
  rs := RTS.rStart;
  rT := RTS.rTStart;

  Inc(rs,
      RTS.Sbps * miny);

  Inc(rT,
      RTS.Tbps * y);

  Inc(rs,
      4 * RTS.xminSource);

  Weighty := @RTS.ContribsY[y].Weights[0];

  ps := PBGRA(rs);
  run := CacheStart;
  Weight := Weighty^;
  //resample vertically into Cache-Array. run points to Cache-Array-Entry.
  //ps is a source-pixel
  for x := RTS.xminSource to RTS.xmaxSource do
    begin

      Combine(ps,
              Weight,
              run,
              AlphaCombineMode);

      Inc(ps);
      Inc(run);
    end; // for x

  Inc(Weighty);
  Inc(rs,
      RTS.Sbps);

  for j := 1 to highy do
    begin
      ps := PBGRA(rs);
      run := CacheStart;
      Weight := Weighty^;

      for x := RTS.xminSource to RTS.xmaxSource do
        begin

          Increase(ps,
                   Weight,
                   run,
                   AlphaCombineMode);

          Inc(ps);
          Inc(run);
        end; // for x

      Inc(Weighty);
      Inc(rs, RTS.Sbps);
    end; // for j

  pT := PBGRA(rT);
  Inc(pT,
      RTS.xmin);
  run := CacheStart;
  jump := RTS.xminSource;

  // Resample Cache-array horizontally into target row.
  // Total is the result for one pixel as TBGRAInt.
  for x := RTS.xmin to RTS.xmax do
    begin
      minx := RTS.ContribsX[x].Min;
      highx := RTS.ContribsX[x].High;
      Weightx := @RTS.ContribsX[x].Weights[0];
      Inc(run,
          minx - jump);

      InitTotal(run,
                Weightx^,
                Total,
                AlphaCombineMode);

      Inc(Weightx);
      Inc(run);

      for i := 1 to highx do
        begin

          IncreaseTotal(run,
                        Weightx^,
                        Total,
                        AlphaCombineMode);

          Inc(Weightx);
          Inc(run);
        end;

    jump := highx + 1 + minx;

    case AlphaCombineMode of
      amIndependent:      ClampIndependent(Total,
                                           pT);
      amPreMultiply:      ClampPreMult(Total,
                                       pT);
      amIgnore:           ClampIgnore(Total,
                                      pT);
      amTransparentColor: ClampPreMult(Total,
                                       pT);
    end;

    if AlphaCombineMode = amTransparentColor then
      if (pT.a > 128) then
        pT.a := 255
      else
        pT.a := 0;

    Inc(pT);
  end; // for x
end;


const
  Precisions: array [TAlphaCombineMode] of TPrecision = (prHigh,
                                                         prLow,
                                                         prHigh,
                                                         prLow);

procedure TResamplingThreadSetup.PrepareResamplingThreads(NewWidth: Integer;
                                                          NewHeight: Integer;
                                                          OldWidth: Integer;
                                                          OldHeight: Integer;
                                                          Radius: Single;
                                                          Filter: TFilter;
                                                          SourceRect: TRectF;
                                                          AlphaCombineMode: TAlphaCombineMode;
                                                          aMaxThreadCount: Integer;
                                                          SourcePitch: Integer;
                                                          TargetPitch: Integer;
                                                          SourceStart: PByte;
                                                          TargetStart: PByte);
var
  yChunkCount: Integer;
  yChunk: Integer;
  j: Integer;
  Index: Integer;

begin

  Tbps := TargetPitch;
  Sbps := SourcePitch;

  MakeContributors(Radius,
                   OldWidth,
                   NewWidth,
                   SourceRect.Left,
                   SourceRect.Right - SourceRect.Left,
                   Filter,
                   Precisions[AlphaCombineMode],
                   ContribsX);

  MakeContributors(Radius,
                   OldHeight,
                   NewHeight,
                   SourceRect.Top,
                   SourceRect.Bottom - SourceRect.Top,
                   Filter,
                   Precisions[AlphaCombineMode],
                   ContribsY);

  rStart := SourceStart; // Source.Scanline[0];
  rTStart := TargetStart; // Target.Scanline[0];

  yChunkCount := Max(Min(NewHeight div _ChunkHeight + 1,
                         aMaxThreadCount),
                         1);

  ThreadCount := yChunkCount;

  SetLength(ymin,
            ThreadCount);

  SetLength(ymax,
            ThreadCount);

  yChunk := NewHeight div yChunkCount;

  xmin := 0;
  xmax := NewWidth - 1;

  xminSource := ContribsX[0].Min;
  xmaxSource := ContribsX[xmax].Min + ContribsX[xmax].High;

 for j := 0 to yChunkCount - 1 do
   begin
     ymin[j] := j * yChunk;
     if (j < yChunkCount - 1) then
       ymax[j] := (j + 1) * yChunk - 1
     else
       ymax[j] := NewHeight - 1;
   end;

  SetLength(CacheMatrix,
            ThreadCount);

  for Index := 0 to ThreadCount - 1 do
    SetLength(CacheMatrix[Index],
              xmaxSource - xminSource + 1);
end;


{ TResamplingThread }

constructor TResamplingThread.Create();
begin
  inherited Create(False);
  FreeOnTerminate := False;
  Wakeup := TEvent.Create;
  Done := TEvent.Create;
  Ready := TEvent.Create;
end;


destructor TResamplingThread.Destroy();
begin
  Wakeup.Free;
  Done.Free;
  Ready.Free;
  inherited;
end;


procedure TResamplingThread.Execute;
begin
  while not terminated do
    begin
      Ready.SetEvent;
      Wakeup.Waitfor(INFINITE);
      if not terminated then
        begin
          Wakeup.ResetEvent;
          fResamplingThreadProc;
          Done.SetEvent;
        end;
  end;
end;


procedure TResamplingThread.RunAnonProc(aProc: TProc);
begin
  Ready.Waitfor(INFINITE);
  Ready.ResetEvent;
  Done.ResetEvent;
  fResamplingThreadProc := aProc;
  Wakeup.SetEvent;
end;

{ TResamplingThreadPool }

procedure TResamplingThreadPool.Finalize();
var
  i: Integer;

begin
  if not Initialized then
    Exit;
  for i := 0 to Length(fResamplingThreads) - 1 do
    begin
      fResamplingThreads[i].Terminate;
      fResamplingThreads[i].Wakeup.SetEvent;
      fResamplingThreads[i].Free;
      fResamplingThreads[i] := nil;
    end;

  SetLength(fResamplingThreads,
            0);
  fThreadCount := 0;
  fInitialized := False;
end;


procedure TResamplingThreadPool.Initialize(aMaxThreadCount: Integer;
                                           aPriority: TThreadpriority);
var
  i: Integer;

begin
  if Initialized then
    Finalize;

  // We need at least 2 threads.
  fThreadCount := Max(aMaxThreadCount,
                      2);
  SetLength(fResamplingThreads,
            fThreadCount);

  for i := 0 to Length(ResamplingThreads) - 1 do
    begin
      fResamplingThreads[i] := TResamplingThread.Create;
      fResamplingThreads[i].Priority := aPriority;
      fResamplingThreads[i].Ready.Waitfor(INFINITE);
    end;
  fInitialized := True;
end;


procedure InitDefaultResamplingThreads;
begin
  if _DefaultThreadPool.fInitialized then
    exit;
  // Creating more threads than processors present does not seem to
  // speed up anything.
  _DefaultThreadPool.Initialize(Min(_MaxThreadCount,
                                    TThread.ProcessorCount),
                                    tpHigher);
end;


procedure FinalizeDefaultResamplingThreads();
begin
  if not _DefaultThreadPool.fInitialized then
    Exit;
  _DefaultThreadPool.Finalize;
end;


initialization

finalization

  FinalizeDefaultResamplingThreads();

{$IFDEF O_MINUS}
{$O-}
{$UNDEF O_MINUS}
{$ENDIF}
{$IFDEF Q_PLUS}
{$Q+}
{$UNDEF Q_PLUS}
{$ENDIF}

end.
