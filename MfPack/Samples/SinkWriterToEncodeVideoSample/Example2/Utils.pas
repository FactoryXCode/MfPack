// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - MediaFoundation
// Project location: https://sourceforge.net/projects/MfPack
//                   https://github.com/FactoryXCode/MfPack
// Module: Utils.pas
// Kind: Pascal / Delphi unit
// Release date: 11-12-2012
// Language: ENU
//
// Revision Version: 3.1.3
// Description: Contains bitmap helpers.
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX), Charles Hacker
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 28/08/2022 All                 PiL release  SDK 10.0.22621.0 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 or later.
//
// Related objects: -
// Related projects: MfPackX313
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.22621.0
//
// Todo: -
//
//==============================================================================
// Source: https://learn.microsoft.com/en-us/windows/win32/medfound/tutorial--using-the-sink-writer-to-encode-video
//         https://blog.dummzeuch.de/2019/12/12/accessing-bitmap-pixels-with-less-scanline-calls-in-delphi/
//
// Copyright (c) Microsoft Corporation. All rights reserved.
//==============================================================================
//
// LICENSE
//
//  The contents of this file are subject to the
//  GNU General Public License v3.0 (the "License");
//  you may not use this file except in
//  compliance with the License. You may obtain a copy of the License at
//  https://www.gnu.org/licenses/gpl-3.0.html
//
// Software distributed under the License is distributed on an "AS IS"
// basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
// License for the specific language governing rights and limitations
// under the License.
//
// Explanatory memorandum:
// Non commercial users may distribute this sourcecode provided that this
// header is included in full at the top of the file.
// Commercial users are not allowed to distribute this sourcecode as part of
// their product.
//
//==============================================================================
unit Utils;

interface

uses
  {WinApi}
  WinApi.Windows,
  WinApi.WinApiTypes,
  {System}
  System.Types,
  {Vcl}
  Vcl.Graphics,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfUtils;


{$IF SizeOf(Pointer) = 4}
type NativeInt = Integer;   // Correction for NativeInt on Delphi <= 2007 (8 bytes to 4 bytes).
{$IFEND}

type

  pRGBArray = ^TRGBArray;
  TRGBArray = array[0..32767] of TRGBTriple;

  // Sets the pointer to the correct pixel offset.
  // See for details: https://blog.dummzeuch.de/2019/12/12/accessing-bitmap-pixels-with-less-scanline-calls-in-delphi/
  function SetPointer(const aPointer: Pointer;
                      aOffset: NativeInt): Pointer; inline;

  // A more precise method to resize a bitmap.
  // Source:
  // Charles Hacker
  // Lecturer in Electronics and Computing
  // School of Engineering
  // Griffith University - Gold Coast
  // Australia
  procedure ResizeBitmap(aBitmap: TBitmap;
                         toWidth: Integer;
                         toHeight: Integer);

  // Calculates nanoseconds to milliseconds.
  function NsecToMsec(nSec: Int64): Int64; inline;

  // Calculates frame duration in 100 nanoseconds units.
  function CalcFrameDuration(aLatency: UINT32;
                             aFrameRate: Double): HNSTIME;

  // Performance or latency calculation
  function PerformanceCounterMilliseconds(AFrequency: Int64): Int64;


threadvar
  TimerFrequency: Int64;



implementation



function SetPointer(const aPointer: Pointer;
                    aOffset: NativeInt): Pointer; inline;
begin
  Result := Pointer(NativeInt(aPointer) + aOffset);
end;


procedure ResizeBitmap(aBitmap: TBitmap;
                       toWidth: Integer;
                       toHeight: Integer);
var
  xScale, yScale: Single;
  sFrom_y, sFrom_x: Single;
  iFrom_y, iFrom_x: Integer;
  to_y, to_x: Integer;
  weight_x, weight_y: array[0..1] of Single;
  weight: Single;
  new_red, new_green: Integer;
  new_blue: Integer;
  total_red, total_green: Single;
  total_blue: Single;
  ix, iy: Integer;
  bTmp: TBitmap;
  sli, slo: pRGBArray;

begin

  if (aBitmap.PixelFormat <> pf24bit) then
    aBitmap.PixelFormat := pf24bit;

  // Create a temporary bitmap
  bTmp := TBitmap.Create;
  bTmp.PixelFormat := pf24bit;
  bTmp.Width := toWidth;
  bTmp.Height := toHeight;

  xScale := bTmp.Width / (aBitmap.Width - 1);
  yScale := bTmp.Height / (aBitmap.Height - 1);

  for to_y := 0 to bTmp.Height - 1 do
    begin
      sFrom_y := to_y / yScale;
      iFrom_y := Trunc(sFrom_y);
      weight_y[1] := sFrom_y - iFrom_y;
      weight_y[0] := 1 - weight_y[1];

      for to_x := 0 to bTmp.Width - 1 do
        begin
          sFrom_x := to_x / xScale;
          iFrom_x := Trunc(sFrom_x);
          weight_x[1] := sFrom_x - iFrom_x;
          weight_x[0] := 1 - weight_x[1];
          total_red := 0.0;
          total_green := 0.0;
          total_blue := 0.0;

          for ix := 0 to 1 do
            begin
              for iy := 0 to 1 do
                begin
                  sli := aBitmap.Scanline[ifrom_y + iy];
                  new_red := sli[iFrom_x + ix].rgbtRed;
                  new_green := sli[iFrom_x + ix].rgbtGreen;
                  new_blue := sli[iFrom_x + ix].rgbtBlue;
                  weight := weight_x[ix] * weight_y[iy];
                  total_red := total_red + new_red * weight;
                  total_green := total_green + new_green * weight;
                  total_blue := total_blue + new_blue * weight;
                end;
            end;

         slo := bTmp.ScanLine[to_y];
         slo[to_x].rgbtRed := Round(total_red);
         slo[to_x].rgbtGreen := Round(total_green);
         slo[to_x].rgbtBlue := Round(total_blue);
        end;
    end;

  aBitmap.Width := bTmp.Width;
  aBitmap.Height := bTmp.Height;
  aBitmap.Canvas.Draw(0,
                      0,
                      bTmp);

  SafeDelete(bTmp);
end;


//
function NsecToMsec(nSec: Int64): Int64; inline;
begin
  Result := Round(nSec / 1000000);
end;

//
function CalcFrameDuration(aLatency: UINT32;
                           aFrameRate: Double): HNSTIME;
const
  msec = (1000 * 1000); // One millisecond = 1,000,000 nano seconds
begin
  Result := Round(ALatency * msec / AFrameRate);
end;

//
function PerformanceCounterMilliseconds(AFrequency: Int64): Int64;
var
  iCount: Int64;

begin
 if (AFrequency = 0) then
   Result := 0
 else
   begin
     QueryPerformanceCounter(iCount);
     Result := Round(iCount / AFrequency * 1000);
   end;
end;

initialization
  QueryPerformanceFrequency(TimerFrequency);

end.
