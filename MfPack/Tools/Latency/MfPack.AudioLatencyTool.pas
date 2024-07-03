//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - MediaFoundation
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: MfPack.AudioLatencyTool.pas
// Kind: Pascal Unit
// Release date: 29-05-2024
// Language: ENU
//
// Revision Version: 3.1.7
//
// Description:
//   Audio latency measuring tool for Windows.
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
// 30/06/2024 All                 RammStein release  SDK 10.0.26100.0 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 (22H2) or later.
//          Usage:
//            1) Add .."/MfPack/Tools/Latency" to your application search path.
//            2) Add "MfPack.AudioLatencyTool" in the uses clause.
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
// SDK version: 10.0.26100.0
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
unit MfPack.AudioLatencyTool;

interface

uses
  WinApi.Windows,
  WinApi.WinApiTypes,
  System.Classes;


type
  TAudioLatencyTool = class(TObject)
  private

    // The data that we are most interested in:
    prMinExecutionTime: Single;
    prMaxExecutionTime: Single;
    prAverageExecutionTime: Single;

    // Other vars.
    prCurrentExecutionTime: Single;
    prTotalExecutionTime: Single;
    prFreqFactor: Single;
    prExecutionCount: ULONG;
    prDoMeasure: Boolean;

    //
    prStartP: TLargeInteger;

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
    //   pCpu: Set this value to run at first CPU by default.
    //         If running on another CPU we get rescheduled immediately.
    procedure Start(var pStart: TLargeInteger;
                    pCPU: DWORD_PTR = 1);

    //--------------------------------------------------------------------------
    // Run the code to be measured inbetween start and stop.
    //--------------------------------------------------------------------------

    procedure Stop(var pEnd: TLargeInteger;
                   out pLatency: TLargeInteger);

    // Public properties.
    property MinExecutionTime: Single read prMinExecutionTime;
    property MaxExecutionTime: Single read prMaxExecutionTime;
    property AverageExecutionTime: Single read prAverageExecutionTime;
    property TotalExecutionTime: Single read prTotalExecutionTime;
  end;


implementation


constructor TAudioLatencyTool.Create();
begin
  inherited Create();

end;


destructor TAudioLatencyTool.Destroy();
begin

  inherited Destroy();
end;


function TAudioLatencyTool.Initialize(): Boolean;
var
  perfFrequency: TLargeInteger;

begin
  Result := False;
  prStartP := 0;
  prCurrentExecutionTime := 0;
  prExecutionCount := 0;
  prMinExecutionTime := 0;
  prMaxExecutionTime := 0;
  prAverageExecutionTime := 0;
  prTotalExecutionTime := 0;
  prDoMeasure := False;

  // Get performance frequency in counts per second.
  if (QueryPerformanceFrequency(perfFrequency) <> True) then
    begin
      // Something went wrong, measuring will not work.
      prFreqFactor := 0;
    end
  else
    begin
      // The freqFactor is the amount of microseconds of a single performance count.
      prFreqFactor := 1000000 / perfFrequency;
      prDoMeasure := True;
      Result := True;
    end;
end;


procedure TAudioLatencyTool.UpdateStats(pElapsed: LONGLONG);
var
  TotExecutionTime: Int64;
  CurExecutionTime: Int64;

begin
  // Calculated elapsed execution time in microseconds.
  prCurrentExecutionTime := pElapsed * prFreqFactor;

  // Update minimum execution time.
  if ((prCurrentExecutionTime < prMinExecutionTime) or (prMinExecutionTime = 0)) then
    prMinExecutionTime := prCurrentExecutionTime;

  // Update maximum execution time.
  if (prCurrentExecutionTime > prMaxExecutionTime) then
    prMaxExecutionTime := prCurrentExecutionTime;

  // Increase number of successful measurings.
  Inc(prExecutionCount);

  TotExecutionTime := Trunc(prTotalExecutionTime);
  CurExecutionTime := Trunc(prCurrentExecutionTime);

  // Update total and average.
  Inc(TotExecutionTime,
      CurExecutionTime);

  prAverageExecutionTime := TotExecutionTime / prExecutionCount;
end;


procedure TAudioLatencyTool.Start(var pStart: Int64;
                                  pCPU: DWORD_PTR = 1);
begin

  if (prDoMeasure = True) then
    begin
      SetThreadAffinityMask(GetCurrentThread(), pCPU);

      // Get the performance time stamp at the beginning of your code.
      QueryPerformanceCounter(pStart);
      // Remember.
      prStartP := pStart;
    end;
end;


procedure TAudioLatencyTool.Stop(var pEnd: TLargeInteger;
                                 out pLatency: TLargeInteger);
begin
  // Get the performance time stamp at the end of your code and update our data.
  if (prDoMeasure = True) then
    begin
      QueryPerformanceCounter(pEnd);

      if ((prStartP > 0) and (pEnd > 0)) then
        UpdateStats(pEnd - prStartP);

      // minExecutionTime, maxExecutionTime and averageExecutionTime are now filled
      // with useful data representing execution times in microseconds.
    end;
end;


end.
