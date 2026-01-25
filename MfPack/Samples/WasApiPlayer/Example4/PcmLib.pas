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
unit PcmLib;

interface

uses

  {WinApi}
  WinApi.Windows,
  WinApi.WinApiTypes;

type

  TSingleArray = array[0..0] of Single;
  PSingleArray = ^TSingleArray;

  TSmallIntArray = array[0..0] of SmallInt;
  PSmallIntArray = ^TSmallIntArray;

  TIntegerArray = array[0..0] of Integer;
  PIntegerArray = ^TIntegerArray;

  TByteArray = array[0..0] of Byte;
  PByteArray = ^TByteArray;


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


implementation


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

end.
