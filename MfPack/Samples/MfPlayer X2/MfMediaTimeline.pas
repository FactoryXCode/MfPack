// FactoryX
//
// Copyright ? FactoryX, Netherlands/Australia/Germany. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: MfMediaTimeline.pas
// Kind: Pascal Unit
// Release date: 29-07-2026
// Language: ENU
//
// Revision Version: 3.2.0
// Description: MfPlayer X2 timeline helper. Resolves a media time for decoded frames and
//              provides a monotonic fallback when a source or decoder omits usable sample
//              timestamps.
//
// Company: FactoryX
// Intiator(s): Tony (maXcomX).
// Contributor(s): Tony Kalf (maXcomX), Carmen (carmenh).
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 05/05/2026 All                 Bauhaus release  SDK 10.0.26100.4654 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 or higher.
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
// Source: -
//
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
unit MfMediaTimeline;

interface

uses

  {WinApi}
  WinApi.WinError,
  {System}
  System.Diagnostics,
  System.SysUtils,
  System.SyncObjs,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfIdl,
  WinApi.MediaFoundationApi.MfObjects;

type

  TMfSampleTimeStatus = (stsFromSample,
                         stsFromFrameRate,
                         stsFromTimelineClock);

  TMfMediaTimeline = class(TObject)
  private
    FLock: TCriticalSection;
    FBasePositionMs: Int64;
    FFrameRateNumerator: UINT32;
    FFrameRateDenominator: UINT32;
    FRate: Single;
    FRunning: Boolean;
    FPaused: Boolean;
    FStopwatch: TStopwatch;
    FHaveLastResolvedTime: Boolean;
    FLastResolvedTimeMs: Int64;

    function GetClockPositionMsUnlocked(): Int64;
    function HasFrameRateUnlocked(): Boolean;
    procedure ResetResolvedTimeUnlocked();

  public

    constructor Create();
    destructor Destroy(); override;

    procedure Reset();
    procedure Start(StartPositionMs: Int64);
    procedure Pause();
    procedure Resume();
    procedure Stop();
    procedure Seek(NewPositionMs: Int64);
    procedure SetRate(NewRate: Single);
    procedure SetFrameRate(Numerator: UINT32;
                           Denominator: UINT32);

    function GetPositionMs(): Int64;
    function ResolveSampleTimeMs(Sample: IMFSample;
                                 FrameIndex: UINT64;
                                 out TimeStatus: TMfSampleTimeStatus): Int64;
  end;


implementation


constructor TMfMediaTimeline.Create();
begin

  inherited Create();
  FLock := TCriticalSection.Create();
  Reset();
end;


destructor TMfMediaTimeline.Destroy();
begin

  FreeAndNil(FLock);
  inherited Destroy();
end;


procedure TMfMediaTimeline.ResetResolvedTimeUnlocked();
begin

  FHaveLastResolvedTime := False;
  FLastResolvedTimeMs := 0;
end;


procedure TMfMediaTimeline.Reset();
begin

  FLock.Acquire();

  try
    FBasePositionMs := 0;
    FFrameRateNumerator := 0;
    FFrameRateDenominator := 0;
    FRate := 1.0;
    FRunning := False;
    FPaused := False;
    FStopwatch.Reset();
    ResetResolvedTimeUnlocked();
  finally
    FLock.Release();
  end;
end;


procedure TMfMediaTimeline.Start(StartPositionMs: Int64);
begin

  if (StartPositionMs < 0) then
    StartPositionMs := 0;

  FLock.Acquire();
  try
    FBasePositionMs := StartPositionMs;
    FRunning := True;
    FPaused := False;
    FStopwatch := TStopwatch.StartNew();
    ResetResolvedTimeUnlocked();
  finally
    FLock.Release();
  end;
end;


procedure TMfMediaTimeline.Pause();
begin

  FLock.Acquire();
  try
    if FRunning and not FPaused then
      begin
        FBasePositionMs := GetClockPositionMsUnlocked();
        FPaused := True;
        FStopwatch.Stop();
      end;
  finally
    FLock.Release();
  end;
end;


procedure TMfMediaTimeline.Resume();
begin

  FLock.Acquire();
  try
    if FRunning and FPaused then
      begin
        FPaused := False;
        FStopwatch := TStopwatch.StartNew();
      end;
  finally
    FLock.Release();
  end;
end;


procedure TMfMediaTimeline.Stop();
begin

  FLock.Acquire();

  try
    if FRunning and not FPaused then
      FBasePositionMs := GetClockPositionMsUnlocked();

    FRunning := False;
    FPaused := False;
    FStopwatch.Stop();
  finally
    FLock.Release();
  end;
end;


procedure TMfMediaTimeline.Seek(NewPositionMs: Int64);
begin

  if (NewPositionMs < 0) then
    NewPositionMs := 0;

  FLock.Acquire();
  try
    FBasePositionMs := NewPositionMs;

    if FRunning and not FPaused then
      FStopwatch := TStopwatch.StartNew();

    ResetResolvedTimeUnlocked();
  finally
    FLock.Release();
  end;
end;


procedure TMfMediaTimeline.SetRate(NewRate: Single);
begin

  if (NewRate <= 0) then
    NewRate := 1.0;

  FLock.Acquire();
  try
    if FRunning and not FPaused then
      begin
        FBasePositionMs := GetClockPositionMsUnlocked();
        FStopwatch := TStopwatch.StartNew();
      end;

    FRate := NewRate;
  finally
    FLock.Release();
  end;
end;


procedure TMfMediaTimeline.SetFrameRate(Numerator: UINT32;
                                        Denominator: UINT32);
begin

  FLock.Acquire();

  try
    FFrameRateNumerator := Numerator;
    FFrameRateDenominator := Denominator;
  finally
    FLock.Release();
  end;
end;


function TMfMediaTimeline.HasFrameRateUnlocked(): Boolean;
begin

  Result := (FFrameRateNumerator > 0) and
            (FFrameRateDenominator > 0);
end;


function TMfMediaTimeline.GetClockPositionMsUnlocked(): Int64;
begin

  Result := FBasePositionMs;

  if FRunning and not FPaused then
    Result := FBasePositionMs +
              Round(FStopwatch.ElapsedMilliseconds * FRate);
end;


function TMfMediaTimeline.GetPositionMs(): Int64;
begin

  FLock.Acquire();

  try
    Result := GetClockPositionMsUnlocked();
  finally
    FLock.Release();
  end;
end;


function TMfMediaTimeline.ResolveSampleTimeMs(Sample: IMFSample;
                                              FrameIndex: UINT64;
                                              out TimeStatus: TMfSampleTimeStatus): Int64;
var
  sampleTimeHns: Int64;
  sampleTimeMs: Int64;
  candidateMs: Int64;
  useSampleTime: Boolean;

begin

  FLock.Acquire();

  try
    sampleTimeHns := 0;
    sampleTimeMs := 0;
    useSampleTime := Assigned(Sample) and
                     SUCCEEDED(Sample.GetSampleTime(@sampleTimeHns)) and
                     (sampleTimeHns >= 0);

    if useSampleTime then
      begin
        sampleTimeMs := sampleTimeHns div 10000;

        // Some source/decoder combinations report zero (or another frozen
        // value) on every decoded frame. Accept the first timestamp, but do
        // not let a repeated non-advancing timestamp pin all subtitles to it.
        useSampleTime := (not FHaveLastResolvedTime) or
                         (sampleTimeMs > FLastResolvedTimeMs);
      end;

    if useSampleTime then
      begin
        candidateMs := sampleTimeMs;
        TimeStatus := stsFromSample;
      end
    else
      if HasFrameRateUnlocked() then
        begin
          candidateMs := FBasePositionMs +
                         Int64((FrameIndex *
                                UInt64(FFrameRateDenominator) *
                                UInt64(1000)) div
                               UInt64(FFrameRateNumerator));
          TimeStatus := stsFromFrameRate;
        end
      else
        begin
          candidateMs := GetClockPositionMsUnlocked();
          TimeStatus := stsFromTimelineClock;
        end;

    if (candidateMs < 0) then
      candidateMs := 0;

    if FHaveLastResolvedTime and
       (candidateMs < FLastResolvedTimeMs) then
      candidateMs := FLastResolvedTimeMs;

    FLastResolvedTimeMs := candidateMs;
    FHaveLastResolvedTime := True;
    Result := candidateMs;
  finally
    FLock.Release();
  end;
end;

end.
