// Version 2.0
unit Support;

interface

uses
  {WinApi}
  WinAPI.Windows,
  WinAPI.psAPI,
  {System}
  System.TimeSpan;


  function ToSize(AKilobytes : int64) : string;
  function ProcessMemoryUsage : int64;
  function TimeSpanToDisplay(ATime : TTimeSpan; AIncludeMilliseconds : Boolean = False) : string;
  function DevicePixelPerInch : Integer;

type
  TLogType = (ltDebug, ltInfo, ltWarning, ltError);

  TLogTypeHelper = record helper for TLogType
    function AsDisplay : string;
  end;

const
  cTab = #9;

implementation

uses
  System.SysUtils;

function DevicePixelPerInch : Integer;
var
  oCompatibleDC : HDC;
  oDC : HDC;
begin
  oDC := GetDC(0);
  try
    oCompatibleDC := CreateCompatibleDC(oDC);
    try
      SetMapMode(oCompatibleDC, MM_TEXT);
      Result := GetDeviceCaps(oCompatibleDC, LOGPIXELSX);
    finally
      DeleteDC(oCompatibleDC);
    end;
  finally
    ReleaseDC(0, oDC);
  end;
end;

function ProcessMemoryUsage : int64;
var
  oCounters : TProcessMemoryCounters;
begin
  Result := 0;
  try
    FillChar(oCounters, SizeOf(oCounters), 0);
    oCounters.cb := SizeOf(TProcessMemoryCounters);
    GetProcessMemoryInfo(GetCurrentProcess, @oCounters, oCounters.cb);
    Result := oCounters.WorkingSetSize;
  except
    RaiseLastOSError;
  end;
end;

function ToSize(AKilobytes : int64) : string;
const
  MB : int64 = 1024;
  GB : int64 = 1048576;
  TB : int64 = 1073741823;
  ABBREVIATED_SIZES : array [0 .. 3] of string = ('KB', 'MB', 'GB', 'TB');
begin
  if AKilobytes <= MB then
    Result := Format('%d ', [AKilobytes]) + ABBREVIATED_SIZES[0]
  else if AKilobytes <= GB then
    Result := Format('%f ', [AKilobytes / MB]) + ABBREVIATED_SIZES[1]
  else if AKilobytes <= TB then
    Result := Format('%f ', [AKilobytes / GB]) + ABBREVIATED_SIZES[2]
  else
    Result := Format('%f ', [AKilobytes / TB]) + ABBREVIATED_SIZES[3];
end;

function TimeSpanToDisplay(ATime : TTimeSpan; AIncludeMilliseconds : Boolean = False) : string;
begin
  if AIncludeMilliseconds then
    Result := Format('%.2d:%.2d:%.2d:%.2d', [ATime.Hours, ATime.Minutes, ATime.Seconds, ATime.Milliseconds])
  else
    Result := Format('%.2d:%.2d:%.2d', [ATime.Hours, ATime.Minutes, ATime.Seconds]);
end;

{ TLogTypeHelper }

function TLogTypeHelper.AsDisplay : string;
begin
  case Self of
    ltInfo :
      Result := 'Info';
    ltWarning :
      Result := 'Warning';
    ltError :
      Result := 'Error';
    ltDebug :
      Result := 'Debug';
  end;
end;

end.
