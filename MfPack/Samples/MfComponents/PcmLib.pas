// FactoryX
//
// Copyright:  FactoryX. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: PcmLib.pas
// Kind: Pascal Unit
// Release date: 24-01-2026
// Language: ENU
//
// Revision Version: 3.1.9
// Description: PCM converter unit.
//
// Notes:
//  - Designed for real-time use in the WASAPI render thread.
// Company: FactoryX
// Intiator(s): Tony (maXcomX), Carmen (carmenh).
// Contributor(s): Tony Kalf (maXcomX)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 05/05/2026 All                 Bauhaus release  SDK 10.0.26100.4654 (Windows 11)
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
unit PcmLib;

interface

uses

  {WinApi}
  WinApi.Windows,
  WinApi.WinApiTypes,
  WinApi.ActiveX,

  System.SysUtils,

  WinApi.WinMM.MMReg,
  WinApi.WinMM.MMeApi;

type

  TSingleArray = array[0..0] of Single;
  PSingleArray = ^TSingleArray;

  TSmallIntArray = array[0..0] of SmallInt;
  PSmallIntArray = ^TSmallIntArray;

  TIntegerArray = array[0..0] of Integer;
  PIntegerArray = ^TIntegerArray;

  TByteArray = array[0..0] of Byte;
  PByteArray = ^TByteArray;


  function PtrSingleOffset(p: PSingle;
                           Index: Integer): PSingle; inline;

  procedure Int16ToFloat(const InBytes: PByte;
                         OutF: PSingle;
                         Samples: Integer); //inline;

  procedure Int32ToFloat(const InBytes: PByte;
                         OutF: PSingle;
                         Samples: Integer); inline;

  procedure Int24ToFloat(const InBytes: PByte;
                         OutF: PSingle;
                         Samples: Integer); inline;

  procedure FloatToInt16(const InF: PSingle;
                         OutBytes: PByte;
                         Samples: Integer); inline;

  procedure FloatToInt32(const InF: PSingle;
                         OutBytes: PByte;
                         Samples: Integer); inline;

  procedure FloatToInt24(const InF: PSingle;
                         OutBytes: PByte;
                         Samples: Integer); inline;

  procedure EnsureDynFloatBuf(var Buf: PSingle;
                              var CapacitySamples: Integer;
                              NeededSamples: Integer); inline;

  function GetWfxBitsAndFloat(const pwfx: PWAVEFORMATEX; out Bits: Integer; out IsFloat: Boolean): Boolean; inline;

  // ---------------------------------------------------------------------------
  // Float32 safety helpers: NaN/Inf detection, sanitizing, and
  // Catmull-Rom interpolation for true-peak / oversampling.
  // ---------------------------------------------------------------------------

  function MfIsFiniteS(const v: Single): Boolean; inline;

  function MfSanitizeSampleS(v: Single;
                             const ClampAbs: Single = 16.0): Single; inline;

  procedure MfSanitizeInterleavedFloat32(p: PSingle;
                                         Samples: Integer;
                                         const ClampAbs: Single = 16.0); inline;

  function MfCatmullRomS(const y0,
                         y1,
                         y2,
                         y3,
                         t: Single;
                         const ClampAbs: Single = 16.0): Single; inline;

  // Parameters:
  //  RampMs = desired smoothing time in milliseconds
  //  SampleRate = e.g. 44100
  //  Frames = block size being processed
  //
  // It returns a coefficient between 0..1.
  // near 0 = very slow change
  // near 1 = immediate change
  //
  // Example:
  //  If:
  //  RampMs = 50
  //  SampleRate = 44100
  //  Frames = 512
  //  then:
  //  ramp samples ≈ 2205
  //  coeff ≈ 512 / 2205 ≈ 0.232
  //  So each block moves about 23% toward the target.
  function RampCoeff(const RampMs: Integer;
                     const SampleRate: Cardinal;
                     const Frames: Integer): Double; inline;

  // Decibel to linear.
  function DbToLin(const dB: Single): Single; inline;

  // DEBUG:
  procedure DebugWfx(AMethod: string;
                     pwfx: PWAVEFORMATEX);


implementation

uses
  System.Math;


function PtrSingleOffset(p: PSingle;
                         Index: Integer): PSingle; inline;
begin

  Result := PSingle(NativeUInt(p) + NativeUInt(Index * SizeOf(Single)));
end;


function GetWfxBitsAndFloat(const pwfx: PWAVEFORMATEX;
                            out Bits: Integer;
                            out IsFloat: Boolean): Boolean; inline;
var
  pEx: PWAVEFORMATEXTENSIBLE;

begin

  Bits := 0;
  IsFloat := False;
  Result := False;

  if pwfx = nil then
    Exit;

  case pwfx^.wFormatTag of
    WAVE_FORMAT_IEEE_FLOAT:
      begin
        Bits := pwfx^.wBitsPerSample;
        IsFloat := (Bits = 32);
        Result := True;
        Exit;
      end;

    WAVE_FORMAT_PCM:
      begin
        Bits := pwfx^.wBitsPerSample;
        IsFloat := False;
        Result := True;
        Exit;
      end;

    WAVE_FORMAT_EXTENSIBLE:
      begin
        pEx := PWAVEFORMATEXTENSIBLE(pwfx);
        Bits := pEx^.Format.wBitsPerSample;

        // SubFormat decides if this is IEEE float or integer PCM.
        if IsEqualGUID(pEx^.SubFormat, KSDATAFORMAT_SUBTYPE_IEEE_FLOAT) then
          IsFloat := (Bits = 32)
        else
          IsFloat := False;

        Result := True;
        Exit;
      end;
  else
    // Unknown/unsupported format tag.
    Result := False;
  end;
end;


procedure Int16ToFloat(const InBytes: PByte;
                       OutF: PSingle;
                       Samples: Integer); inline;
var
  i: Integer;
  pIn: PSmallInt;
  pOut: PSingle;

begin

  pIn := PSmallInt(InBytes);
  pOut := OutF;

  for i := 0 to Samples - 1 do
    begin

      pOut^ := pIn^ * (1.0 / 32768.0);
      Inc(pIn);
      Inc(pOut);
    end;
end;


procedure Int32ToFloat(const InBytes: PByte;
                       OutF: PSingle;
                       Samples: Integer); inline;
var
  i: Integer;
  pIn: PInteger;
  pOut: PSingle;

begin

  pIn := PInteger(InBytes);
  pOut := OutF;

  for i := 0 to Samples - 1 do
    begin

      pOut^ := pIn^ * (1.0 / 2147483648.0);
      Inc(pIn);
      Inc(pOut);
    end;
end;


procedure Int24ToFloat(const InBytes: PByte;
                       OutF: PSingle;
                       Samples: Integer); inline;
var
  i: Integer;
  b0, b1, b2: Integer;
  v: Integer;
  p: PByte;
  pOut: PSingle;

begin

  p := InBytes;
  pOut := OutF;

  for i := 0 to Samples - 1 do
  begin
    b0 := p^; Inc(p);
    b1 := p^; Inc(p);
    b2 := p^; Inc(p);

    v := (b0) or (b1 shl 8) or (b2 shl 16);
    if (v and $00800000) <> 0 then
      v := v or Integer($FF000000);

    pOut^ := v * (1.0 / 8388608.0);
    Inc(pOut);
  end;
end;


procedure FloatToInt16(const InF: PSingle;
                       OutBytes: PByte;
                       Samples: Integer); inline;
var
  i: Integer;
  pIn: PSingle;
  pOut: PSmallInt;
  x: Single;
  v: Integer;

begin

  pIn := InF;
  pOut := PSmallInt(OutBytes);

  for i := 0 to Samples - 1 do
    begin

      x := pIn^;
      if (x < -1.0) then
        x := -1.0
      else
        if (x > 0.9999695) then
          x := 0.9999695;

      v := Round(x * 32768.0);
      if (v < -32768) then
        v := -32768
      else
        if (v > 32767) then
          v := 32767;

      pOut^ := SmallInt(v);

      Inc(pIn);
      Inc(pOut);
    end;
end;


procedure FloatToInt32(const InF: PSingle;
                       OutBytes: PByte;
                       Samples: Integer); inline;
var
  i: Integer;
  pIn: PSingle;
  pOut: PInteger;
  x: Single;
  v64: Int64;

begin

  pIn := InF;
  pOut := PInteger(OutBytes);

  for i := 0 to Samples - 1 do
    begin

      x := pIn^;

      if (x < -1.0) then
        x := -1.0
      else
        if (x > 0.99999994) then
          x := 0.99999994;

      v64 := Round(x * 2147483648.0);
      if (v64 < Int64(-2147483648)) then
        v64 := Int64(-2147483648)
      else
        if (v64 > Int64(2147483647)) then
          v64 := Int64(2147483647);

      pOut^ := Integer(v64);

      Inc(pIn);
      Inc(pOut);
    end;
end;


procedure FloatToInt24(const InF: PSingle;
                       OutBytes: PByte;
                       Samples: Integer); inline;
var
  i: Integer;
  pIn: PSingle;
  x: Single;
  v: Integer;
  p: PByte;

begin

  pIn := InF;
  p := OutBytes;

  for i := 0 to Samples - 1 do
    begin

      x := pIn^;

      if (x < -1.0) then
        x := -1.0
      else
        if (x > 0.99999994) then
          x := 0.99999994;

      v := Round(x * 8388608.0);
      if (v < -8388608) then
        v := -8388608
      else
        if (v > 8388607) then
          v := 8388607;

      p^ := Byte(v and $FF);
      Inc(p);

      p^ := Byte((v shr 8) and $FF);
      Inc(p);

      p^ := Byte((v shr 16) and $FF);
      Inc(p);

      Inc(pIn);
  end;
end;


procedure EnsureDynFloatBuf(var Buf: PSingle;
                            var CapacitySamples: Integer;
                            NeededSamples: Integer); inline;
var
  NewBytes: NativeUInt;

begin

  if (NeededSamples <= 0) then
    Exit;

  if (CapacitySamples < NeededSamples) then
    begin

      CapacitySamples := NeededSamples;
      NewBytes := NativeUInt(CapacitySamples) * NativeUInt(SizeOf(Single));

      if (Buf = nil) then
        GetMem(Buf,
               NewBytes)
      else
        ReallocMem(Buf,
                   NewBytes);
    end;
end;


function MfIsFiniteS(const v: Single): Boolean; inline;
var
  u: Cardinal absolute v;

begin

  // IEEE754: exponent all ones => Inf/NaN.
  Result := ((u and $7F800000) <> $7F800000);
end;


function MfSanitizeSampleS(v: Single;
                           const ClampAbs: Single = 16.0): Single; inline;
begin

  // Kill NaN/Inf (and keep signal NaNs from raising XE7 exceptions).
  if not MfIsFiniteS(v) then
    Exit(0.0);

  // Clamp to a sane range to prevent polynomial overflow in oversampling.
  if (v > ClampAbs) then
    Exit(ClampAbs)
  else
    if (v < -ClampAbs) then
      Exit(-ClampAbs);

  Result := v;
end;


procedure MfSanitizeInterleavedFloat32(p: PSingle;
                                       Samples: Integer;
                                       const ClampAbs: Single = 16.0); inline;
var
  i: Integer;

begin

  if (p = nil) or (Samples <= 0) then
    Exit;

  for i := 0 to Samples - 1 do
    begin

      p^ := MfSanitizeSampleS(p^,
                              ClampAbs);
      Inc(p);
    end;
end;


function MfCatmullRomS(const y0,
                       y1,
                       y2,
                       y3,
                       t: Single;
                       const ClampAbs: Single = 16.0): Single; inline;
var
  v0,
  v1,
  v2,
  v3: Double;
  td,
  t2,
  t3: Double;

begin

  // Sanitize inputs first; this prevents XE7 FP exceptions and white-noise
  // bursts if a single NaN/Inf sneaks into the DSP chain.
  v0 := MfSanitizeSampleS(y0,
                          ClampAbs);
  v1 := MfSanitizeSampleS(y1,
                          ClampAbs);
  v2 := MfSanitizeSampleS(y2,
                          ClampAbs);
  v3 := MfSanitizeSampleS(y3,
                          ClampAbs);

  td := (t * 1.0);
  t2 := td * td;
  t3 := t2 * td;

  // Standard Catmull-Rom spline (computed in Double for robustness).
  Result := (0.5 * ((2*v1) +
            (-v0 + v2) * td +
            (2*v0 - 5*v1 + 4*v2 - v3) * t2 +
            (-v0 + 3*v1 - 3*v2 + v3) * t3)) * 1.0;
end;


function RampCoeff(const RampMs: Integer;
                   const SampleRate: Cardinal;
                   const Frames: Integer): Double; inline;
var
  RampSamples: Double;

begin

  if (RampMs <= 0) or
     (SampleRate = 0) or
     (Frames <= 0) then
    Exit(1.0);

  RampSamples := (RampMs * 0.001) * SampleRate;
  if (RampSamples <= 0.0) then
    Exit(1.0);

  Result := Frames / RampSamples;

  if (Result < 0.0) then
    Result := 0.0
  else
  if (Result > 1.0) then
    Result := 1.0;
end;


function DbToLin(const dB: Single): Single; inline;
begin

  Result := Power(10.0,
                  dB / 20.0);
end;


// DEBUG:

procedure DebugWfx(AMethod: string;
                   pwfx: PWAVEFORMATEX);
var
  pex: PWAVEFORMATEXTENSIBLE;
  g: TGUID;
  sGuid: string;

begin

  if (pwfx = nil) then
    Exit;

  OutputDebugString(PChar(Format(
    'In method: %s  WFX: tag=%d ch=%d sr=%d bits=%d align=%d avg=%d cbSize=%d',
    [AMethod, pwfx^.wFormatTag, pwfx^.nChannels, pwfx^.nSamplesPerSec,
     pwfx^.wBitsPerSample, pwfx^.nBlockAlign, pwfx^.nAvgBytesPerSec,
     pwfx^.cbSize])));

  if (pwfx^.wFormatTag = WAVE_FORMAT_EXTENSIBLE) then
    begin

      pex := PWAVEFORMATEXTENSIBLE(pwfx);
      g := pex^.SubFormat;
      sGuid := GUIDToString(g);
      OutputDebugString(PChar('  EXT: SubFormat=' + sGuid +
                              Format('  Mask=$%x', [pex^.dwChannelMask])));
    end;
end;


end.
