// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - MediaFoundation
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: Support.pas
// Kind: Pascal Unit
// Release date: 29-03-2022
// Language: ENU
//
// Revision Version: 3.1.7
//
// Description:
//   This unit contains helpermethods for the MFFrameCapture project.
//
// Organisation: FactoryX
// Initiator(s): Ciaran
// Contributor(s): Ciaran, Tony (maXcomX)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 30/06/2024 All                 RammStein release  SDK 10.0.26100.0 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 (2H20) or later.
//
// Related objects: -
// Related projects: MfPackX317/Samples/CameraFrameCapture
//
// Compiler version: 23 up to 35
// SDK version: 10.0.26100.0
//
// Todo: -
//
//==============================================================================
// Source: -
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
unit Support;

interface

uses
  {WinApi}
  WinAPI.Messages,
  WinAPI.Windows,
  WinAPI.psAPI,
  {System}
  System.Classes,
  System.TimeSpan,
  {VLC}
  VCL.Graphics,
  VCL.Imaging.pngimage,
  VCL.Imaging.jpeg;

  function ToSize(AKilobytes: int64): string;
  function ProcessMemoryUsage: int64;
  function TimeSpanToDisplay(ATime: TTimeSpan;
                             AIncludeMilliseconds: Boolean = False): string;

type

  TLogType = (ltDebug1,
              ltDebug,
              ltInfo,
              ltWarning,
              ltError);

  TImageType = (itBitmap,
                itPNG,
                itJPG);

  TFrameDataEvent = procedure(AMemoryStream: TMemoryStream) of object;
  TLogEvent = reference to procedure(const AMessage: string;
                                     ALogType: TLogType);

  TLogTypeHelper = record helper for TLogType
    function AsDisplay: string;
  end;

  TVideoFormatInfo = record
  public
    iVideoWidth: Integer;
    iVideoHeight: Integer;
    iBufferWidth: Integer;
    iBufferHeight: Integer;
    iStride: Integer;
    oSubType: TGUID;

    procedure Reset();
  end;

  // CriticalSection
  TMFCritSec = class
  private
    FCriticalSection: TRTLCriticalSection;

  public
    constructor Create();
    destructor Destroy(); override;
    procedure Lock();
    procedure Unlock();
  end;

  procedure HandleThreadMessages(AThread: THandle;
                                 AWait: Cardinal = INFINITE);
  function GetMemoryUsed(): string;

  procedure SaveImage(AInput: TBitmap;
                      const APath: string;
                      AType: TImageType);
  function PerformanceCounterMilliseconds(AFrequency: Int64): Int64;

threadvar
  TimerFrequency: Int64;

const
  cTab = #9;


implementation


uses
  System.SysUtils;

function ProcessMemoryUsage(): int64;
var
  oCounters: TProcessMemoryCounters;

begin
  Result := 0;
  try
    FillChar(oCounters,
             SizeOf(oCounters),
             0);
    oCounters.cb := SizeOf(TProcessMemoryCounters);
    GetProcessMemoryInfo(GetCurrentProcess,
                         @oCounters,
                         oCounters.cb);
    Result := oCounters.WorkingSetSize;
  except
    RaiseLastOSError;
  end;
end;


function GetMemoryUsed(): string;
begin
  Result := ToSize(Round(ProcessMemoryUsage / 1024));
end;


function ToSize(AKilobytes: int64): string;
const
  MB: int64 = 1024;
  GB: int64 = 1048576;
  TB: int64 = 1073741823;
  ABBREVIATED_SIZES: array [0 .. 3] of string = ('KB', 'MB', 'GB', 'TB');

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


function TimeSpanToDisplay(ATime: TTimeSpan; AIncludeMilliseconds: Boolean = False): string;
begin
  if AIncludeMilliseconds then
    Result := Format('%.2d:%.2d:%.2d:%.2d', [ATime.Hours, ATime.Minutes, ATime.Seconds, ATime.Milliseconds])
  else
    Result := Format('%.2d:%.2d:%.2d', [ATime.Hours, ATime.Minutes, ATime.Seconds]);
end;


procedure SaveImage(AInput: TBitmap;
                    const APath: string;
                    AType: TImageType);
var
  pPng: TPngImage;
  pJpg: TJPEGImage;

begin
  case AType of
    itBitmap :
      begin
         AInput.SaveToFile(APath);
      end;
    itPNG :
      begin
         pPng := TPngImage.Create;
         pPng.Assign(AInput);
         AInput.SaveToFile(APath);

         if Assigned(pPng) then
           pPng.Free;
      end;
    itJPG :
      begin
        pJpg := TJPEGImage.Create;
        // Adjust performance, compression etc.
        pJpg.Performance := jpBestQuality;
        pJpg.ProgressiveEncoding := True;
        pJpg.ProgressiveDisplay := True;
        //pJpg.CompressionQuality := 30;
        pJpg.Compress;
        pJpg.Assign(AInput);
        pjpg.SaveToFile(APath);

        if Assigned(pJpg) then
          pJpg.Free();
      end;
  end;
end;

{ TMFCritSec }

constructor TMFCritSec.Create();
begin
  InitializeCriticalSection(FCriticalSection);
end;

destructor TMFCritSec.Destroy();
begin
  DeleteCriticalSection(FCriticalSection);
  inherited;
end;

procedure TMFCritSec.Lock();
begin
  EnterCriticalSection(FCriticalSection);
end;

procedure TMFCritSec.Unlock();
begin
  LeaveCriticalSection(FCriticalSection);
end;

procedure HandleThreadMessages(AThread: THandle;
                               AWait: Cardinal = INFINITE);
var
  oMsg: TMsg;

begin

  while (MsgWaitForMultipleObjects(1,
                                   AThread,
                                   False,
                                   AWait,
                                   QS_ALLINPUT) = WAIT_OBJECT_0 + 1) do
    begin
      PeekMessage(oMsg,
                  0,
                  0,
                  0,
                  PM_REMOVE);

      if oMsg.Message = WM_QUIT then
        Exit;

      TranslateMessage(oMsg);
      DispatchMessage(oMsg);
    end;
end;


{ TLogTypeHelper }
function TLogTypeHelper.AsDisplay: string;
begin
  case Self of
    ltInfo:
      Result := 'Info';
    ltWarning:
      Result := 'Warning';
    ltError:
      Result := 'Error';
    ltDebug:
      Result := 'Debug';
    ltDebug1:
      Result := 'Debug1';
  end;
end;

{ TVideoFormatInfo }
procedure TVideoFormatInfo.Reset();
begin
  iVideoWidth := 0;
  iVideoHeight := 0;
  iBufferWidth := 0;
  iBufferHeight := 0;
end;


function PerformanceCounterMilliseconds(AFrequency: Int64): Int64;
var
  iCount: Int64;

begin
 if AFrequency = 0 then
   Result := 0
 else
   begin
     QueryPerformanceCounter(iCount);
     Result := Round(iCount / AFrequency * 1000);
   end;
end; { PerformanceCounterToMS }


initialization
  QueryPerformanceFrequency(TimerFrequency);

end.
