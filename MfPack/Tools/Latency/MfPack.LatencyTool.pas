//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - MediaFoundation
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: MfPack.LatencyTool.pas
// Kind: Pascal Unit
// Release date: 29-05-2024
// Language: ENU
//
// Revision Version: 4.1.0
//
// Description:
//   Latency measuring tool for Windows.
//   Code to measure minimum, maximum and average execution times of a routine in MICROSECONDS.
//   NOTE: build a RELEASE version of your code including this code to get useful results.

// Organisation: FactoryX
// Initiator(s): maXcomX
// Contributor(s): Tony (maXcomX)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 15/11/2026 All                 Eels release  SDK 10.0.28000.2705  (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 (22H2) or later.
//          Usage:
//            1) Add .."/MfPack/Tools/Latency" to your application search path.
//            2) Add "MfPack.LatencyTool" in the uses clause.
//            3) Create the class in your application.
//            4) Call Initialize within the method where measurment is needed.
//            5) Call Start() just before the code to be measured.
//            6) Call Stop() immediatly after the code been measured.
//            7) Call FreeAndNil() when not needed anymore.
//
// Related objects: -
// Related projects: MfPack/Tools
//
// Compiler version: 23 up to 35
// SDK version: 10.0.28000.2705
//
// Todo: -
//
//==============================================================================
// Source: https://www.resplendence.com/audiolatency
//         FactoryX
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
unit MfPack.LatencyTool;

interface

uses

  {WinApi}
  WinApi.Windows,
  WinApi.WinApiTypes,
  {System}
  System.Classes,
  System.SysUtils;


type
  TLatencyTool = class(TObject)
  private

    // The data that we are most interested in:
    prMinExecutionTime: Single;
    prMaxExecutionTime: Single;
    prAverageExecutionTime: Single;

    // Other vars.
    prCurrentExecutionTime: Single;
    prTotalExecutionTime: Single;
    prFreqFactor: Double;
    prTotalMicroseconds: Double;
    prExecutionCount: UInt64;
    prDoMeasure: Boolean;
    prRunning: Boolean;
    prThreadHandle: THandle;
    prPreviousAffinity: DWORD_PTR;

    //
    prStartP: TLargeInteger;

    procedure RestoreAffinity;

  public

    constructor Create();
    destructor Destroy(); override;

    // Run this code only once at initialization
    function Initialize(): Boolean;
    procedure UpdateStats(pElapsed: LONGLONG);

    // Code to be measured, this could be your audio process function.
    // Make sure that Initialize() has been called once before.
    // Params:
    //   pStart: Variable := 0
    //   pCPU: Affinity mask for this measurement; 0 leaves affinity unchanged.
    //         The default is CPU 0. Stop restores the previous affinity.
    //         Use each instance on one thread; do not share overlapping scopes.
    procedure Start(var pStart: TLargeInteger;
                    pCPU: DWORD_PTR = 1);

    //--------------------------------------------------------------------------
    // Run the code to be measured inbetween start and stop.
    //--------------------------------------------------------------------------

    // pEnd and pLatency are QPC ticks; statistics are microseconds.
    procedure Stop(var pEnd: TLargeInteger;
                   out pLatency: TLargeInteger);

    // Public properties.
    property MinExecutionTime: Single read prMinExecutionTime;
    property MaxExecutionTime: Single read prMaxExecutionTime;
    property AverageExecutionTime: Single read prAverageExecutionTime;
    property TotalExecutionTime: Single read prTotalExecutionTime;
  end;


implementation


constructor TLatencyTool.Create();
begin
  inherited Create();
end;


destructor TLatencyTool.Destroy();
begin
  try
    RestoreAffinity;
  finally
    inherited Destroy();
  end;
end;


procedure TLatencyTool.RestoreAffinity;
var
  ErrorCode: DWORD;
begin
  prRunning := False;
  prStartP := 0;
  if prThreadHandle = 0 then
    Exit;
  ErrorCode := 0;
  try
    if (prPreviousAffinity <> 0) and
       (SetThreadAffinityMask(prThreadHandle,prPreviousAffinity) = 0) then
      ErrorCode := GetLastError;
  finally
    CloseHandle(prThreadHandle);
    prThreadHandle := 0;
    prPreviousAffinity := 0;
  end;
  if ErrorCode <> 0 then
    RaiseLastOSError(ErrorCode);
end;


function TLatencyTool.Initialize(): Boolean;
var
  PerfFrequency: TLargeInteger;
begin
  RestoreAffinity;
  prCurrentExecutionTime := 0;
  prExecutionCount := 0;
  prMinExecutionTime := 0;
  prMaxExecutionTime := 0;
  prAverageExecutionTime := 0;
  prTotalExecutionTime := 0;
  prTotalMicroseconds := 0;
  prFreqFactor := 0;
  prDoMeasure := False;
  PerfFrequency := 0;
  Result := QueryPerformanceFrequency(PerfFrequency) and (PerfFrequency > 0);
  if Result then
    begin
      prFreqFactor := 1000000.0 / PerfFrequency;
      prDoMeasure := True;
    end;
end;


procedure TLatencyTool.UpdateStats(pElapsed: LONGLONG);
var
  ElapsedMicroseconds: Double;
begin
  if not prDoMeasure or (pElapsed < 0) then
    Exit;
  ElapsedMicroseconds := pElapsed * prFreqFactor;
  prCurrentExecutionTime := ElapsedMicroseconds;
  if (prExecutionCount = 0) or (prCurrentExecutionTime < prMinExecutionTime) then
    prMinExecutionTime := prCurrentExecutionTime;
  if prCurrentExecutionTime > prMaxExecutionTime then
    prMaxExecutionTime := prCurrentExecutionTime;
  Inc(prExecutionCount);
  prTotalMicroseconds := prTotalMicroseconds + ElapsedMicroseconds;
  prTotalExecutionTime := prTotalMicroseconds;
  prAverageExecutionTime := prTotalMicroseconds / prExecutionCount;
end;


procedure TLatencyTool.Start(var pStart: TLargeInteger;
                              pCPU: DWORD_PTR = 1);
begin
  pStart := 0;
  RestoreAffinity;
  if not prDoMeasure then
    Exit;
  try
    if pCPU <> 0 then
      begin
        if not DuplicateHandle(GetCurrentProcess,GetCurrentThread,GetCurrentProcess,
                                @prThreadHandle,0,False,DUPLICATE_SAME_ACCESS) then
          RaiseLastOSError;
        prPreviousAffinity := SetThreadAffinityMask(prThreadHandle,pCPU);
        if prPreviousAffinity = 0 then
          RaiseLastOSError;
      end;
    if QueryPerformanceCounter(pStart) then
      begin
        prStartP := pStart;
        prRunning := True;
      end
    else
      pStart := 0;
  finally
    if not prRunning then
      RestoreAffinity;
  end;
end;


procedure TLatencyTool.Stop(var pEnd: TLargeInteger;
                             out pLatency: TLargeInteger);
begin
  pEnd := 0;
  pLatency := 0;
  if not prDoMeasure or not prRunning then
    Exit;
  try
    if QueryPerformanceCounter(pEnd) then
      begin
        if pEnd >= prStartP then
          begin
            pLatency := pEnd - prStartP;
            UpdateStats(pLatency);
          end;
      end
    else
      pEnd := 0;
  finally
    RestoreAffinity;
  end;
end;

end.
