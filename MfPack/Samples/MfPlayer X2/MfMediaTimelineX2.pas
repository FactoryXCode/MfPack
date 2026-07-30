// FactoryX
//
// Project: Media Foundation - MFPack - Samples
// Module: MfMediaTimelineX2.pas
// Kind: Pascal Unit
//
// MfPlayer X2 timeline helper. Resolves a media time for a decoded frame even
// when the incoming sample does not carry a usable timestamp.

unit MfMediaTimelineX2;

interface

uses

  {WinApi}
  WinApi.WinError,
  {System}
  System.Diagnostics,
  System.SysUtils,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfIdl,
  WinApi.MediaFoundationApi.MfObjects;

type

  TMfSampleTimeStatus = (stsFromSample,
                         stsFromFrameRate,
                         stsFromTimelineClock);

  TMfMediaTimeline = class(TObject)
  private
    FBasePositionMs: Int64;
    FFrameRateNumerator: UINT32;
    FFrameRateDenominator: UINT32;
    FRate: Single;
    FRunning: Boolean;
    FPaused: Boolean;
    FStopwatch: TStopwatch;

    function GetClockPositionMs(): Int64;
    function HasFrameRate(): Boolean;

  public

    constructor Create();

    procedure Reset();
    procedure Start(StartPositionMs: Int64);
    procedure Pause();
    procedure Resume();
    procedure Stop();
    procedure Seek(NewPositionMs: Int64);
    procedure SetRate(NewRate: Single);
    procedure SetFrameRate(Numerator: UINT32;
                           Denominator: UINT32);

    function ResolveSampleTimeMs(Sample: IMFSample;
                                 FrameIndex: UINT64;
                                 out TimeStatus: TMfSampleTimeStatus): Int64;
  end;


implementation


constructor TMfMediaTimeline.Create();
begin

  inherited Create();
  Reset();
end;


procedure TMfMediaTimeline.Reset();
begin

  FBasePositionMs := 0;
  FFrameRateNumerator := 0;
  FFrameRateDenominator := 0;
  FRate := 1.0;
  FRunning := False;
  FPaused := False;
  FStopwatch.Reset();
end;


procedure TMfMediaTimeline.Start(StartPositionMs: Int64);
begin

  FBasePositionMs := StartPositionMs;
  FRunning := True;
  FPaused := False;
  FStopwatch := TStopwatch.StartNew();
end;


procedure TMfMediaTimeline.Pause();
begin

  if FRunning and not FPaused then
    begin

      FBasePositionMs := GetClockPositionMs();
      FPaused := True;
      FStopwatch.Stop();
    end;
end;


procedure TMfMediaTimeline.Resume();
begin

  if FRunning and FPaused then
    begin

      FPaused := False;
      FStopwatch := TStopwatch.StartNew();
    end;
end;


procedure TMfMediaTimeline.Stop();
begin

  FRunning := False;
  FPaused := False;
  FStopwatch.Stop();
end;


procedure TMfMediaTimeline.Seek(NewPositionMs: Int64);
begin

  FBasePositionMs := NewPositionMs;
  if FRunning and not FPaused then
    FStopwatch := TStopwatch.StartNew();
end;


procedure TMfMediaTimeline.SetRate(NewRate: Single);
begin

  if (NewRate <= 0) then
    NewRate := 1.0;

  if FRunning and not FPaused then
    begin

      FBasePositionMs := GetClockPositionMs();
      FStopwatch := TStopwatch.StartNew();
    end;

  FRate := NewRate;
end;


procedure TMfMediaTimeline.SetFrameRate(Numerator: UINT32;
                                        Denominator: UINT32);
begin

  FFrameRateNumerator := Numerator;
  FFrameRateDenominator := Denominator;
end;


function TMfMediaTimeline.HasFrameRate(): Boolean;
begin

  Result := (FFrameRateNumerator > 0) and (FFrameRateDenominator > 0);
end;


function TMfMediaTimeline.GetClockPositionMs(): Int64;
begin

  Result := FBasePositionMs;

  if FRunning and not FPaused then
    Result := FBasePositionMs + Round(FStopwatch.ElapsedMilliseconds * FRate);
end;


function TMfMediaTimeline.ResolveSampleTimeMs(Sample: IMFSample;
                                             FrameIndex: UINT64;
                                             out TimeStatus: TMfSampleTimeStatus): Int64;
var
  sampleTimeHns: Int64;

begin

  sampleTimeHns := 0;

  if Assigned(Sample) and Succeeded(Sample.GetSampleTime(@sampleTimeHns)) then
    begin

      TimeStatus := stsFromSample;
      Result := sampleTimeHns div 10000;
      Exit;
    end;

  if HasFrameRate() then
    begin

      TimeStatus := stsFromFrameRate;
      Result := FBasePositionMs +
                Int64((FrameIndex * FFrameRateDenominator * 1000) div FFrameRateNumerator);
      Exit;
    end;

  TimeStatus := stsFromTimelineClock;
  Result := GetClockPositionMs();
end;

end.
