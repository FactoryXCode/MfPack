// FactoryX
//
// Copyright (c) FactoryX. All rights reserved.
//
// Project: Media Foundation - MFPack - Cast
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: MfCastCaptureWriter.pas
// Kind: Delphi file
// Release date: 29-07-2026
// Language: ENU
//
// Revision Version: 4.0.1
// Description: MfPack Cast V2.1 capture writer.
//
// Intiator(s): Tony (maXcomX), Carmen (carmenh)
// Contributor(s): Tony Kalf (maXcomX), Carmen (carmenh).
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 15/11/2026 All                 Eels release  SDK 10.0.28000.2705 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 or higher.
//
// Related objects: -
// Related projects: MfPackX410
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.28000.2705
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
// Groupname: FactoryX
// The Initial Developers of the Original Code are: Tony Kalf (maXcomX)
//                                                  Peter Larson (ozships)
// Contributor(s): Tony Kalf (maXcomX), Peter Larson (ozships)
//
// Non commercial users may distribute this source code provided that this
// header is included this source code provided that this header is included
// in full at the top of the file.
// Commercial users are not allowed to include this API as part of their
// product without implicit permission.
//
//==============================================================================
unit MfCastCaptureWriter;

interface

uses
  
  {WinApi}
  Winapi.Windows,
  {System} 
  System.Classes, 
  System.SysUtils, 
  System.SyncObjs,
  System.Generics.Collections,
  {Cast} 
  MfCastTypes, 
  MfCastInterfaces;

type
  TMfCastCaptureWrite = function(const AAudio: Boolean;
                                 const AData: TBytes;
                                 const ATime: Int64;
                                 const ADuration: Int64): HRESULT of object;

  TMfCastCapturePacket = record
    Data: TBytes;
    Time: Int64;
    Duration: Int64;
  end;

  // The producer owns capture timing. This worker owns potentially blocking
  // encoder/presentation calls. Queued bytes are private copies, never DXGI or
  // WASAPI buffers, and overload discards old live data instead of growing RAM.
  TMfCastCaptureWriter = class(TThread)
  private
    FLock: TCriticalSection;
    FReady: TEvent;
    FVideo: TQueue<TMfCastCapturePacket>;
    FAudio: TQueue<TMfCastCapturePacket>;
    FPending: TMfCastCapturePacket;
    FWrite: TMfCastCaptureWrite;
    FLogger: IMfCastLogger;
    FSource: string;
    FResult: Longint;
    FVideoLimit: Integer;
    FVideoTrimTo: Integer;
    FDroppedVideo: Integer;
    FDroppedAudio: Integer;
    FPeakVideo: Integer;
    FPeakAudio: Integer;
    
    procedure FlushAudioLocked();
    function TakePacket(out APacket: TMfCastCapturePacket;
                        out AAudio: Boolean): Boolean;
  
  protected
    procedure Execute(); override;
  
  public
    constructor Create(const AWrite: TMfCastCaptureWrite;
                       const ALogger: IMfCastLogger;
                       const ASource: string;
                       const AVideoLimit: Integer = 8;
                       const AVideoTrimTo: Integer = 2);
    destructor Destroy(); override;
    
    function QueueVideo(const AData: TBytes;
                        const ATime: Int64;
                        ADuration: Int64): HRESULT;
                        
    function QueueAudio(const AData: PByte;
                        const ASize: Cardinal;
                        const ASilent: Boolean;
                        const ATime: Int64;
                        ADuration: Int64): HRESULT;
                        
    procedure FlushAudio();
    function GetResult(): HRESULT;
    
    procedure GetStats(out AVideoPeak: Integer;
                       out AAudioPeak: Integer;
                       out AVideoDropped: Integer;
                       out AAudioDropped: Integer);
  end;
  

implementation

uses

  {WinApi}
  WinApi.ActiveX;


constructor TMfCastCaptureWriter.Create(const AWrite: TMfCastCaptureWrite;
                                        const ALogger: IMfCastLogger;
                                        const ASource: string;
                                        const AVideoLimit: Integer;
                                        const AVideoTrimTo: Integer);
begin
  
  inherited Create(True);
  
  FreeOnTerminate := False;
  FLock := TCriticalSection.Create();
  FReady := TEvent.Create(nil, True, False, '');
  FVideo := TQueue<TMfCastCapturePacket>.Create();
  FAudio := TQueue<TMfCastCapturePacket>.Create();
  FWrite := AWrite;
  FLogger := ALogger;
  FSource := ASource;
  FVideoLimit := AVideoLimit;
  FVideoTrimTo := AVideoTrimTo;
  FResult := S_OK;
end;


destructor TMfCastCaptureWriter.Destroy();
begin
  
  Terminate();
  if Assigned(FReady) then
    FReady.SetEvent();
  // Also support cleanup if construction of the surrounding pipeline fails
  // before its suspended worker can be started.
  
  if Suspended then
    Start();
    
  WaitFor();
  FVideo.Free();
  FAudio.Free();
  FReady.Free();
  FLock.Free();
  
  inherited;
end;


function TMfCastCaptureWriter.GetResult(): HRESULT;
begin
  
  Result := InterlockedCompareExchange(FResult,
                                       0,
                                       0);
end;


procedure TMfCastCaptureWriter.GetStats(out AVideoPeak: Integer;
                                        out AAudioPeak: Integer;
                                        out AVideoDropped: Integer;
                                        out AAudioDropped: Integer);
begin
  
  FLock.Acquire();
 
  try
    AVideoPeak := FPeakVideo;
    AAudioPeak := FPeakAudio;
    AVideoDropped := FDroppedVideo;
    AAudioDropped := FDroppedAudio;
    
  finally
    FLock.Release();
  end;
end;


function TMfCastCaptureWriter.QueueVideo(const AData: TBytes;
                                         const ATime: Int64;
                                         ADuration: Int64): HRESULT;
var
  Packet: TMfCastCapturePacket;
  
begin
  
  Result := GetResult();
  if FAILED(Result) then
    Exit;
  
  // Never retain the producer's mutable working frame.
  Packet.Data := Copy(AData,
                      0,
                      Length(AData));
                      
  Packet.Time := ATime;
  Packet.Duration := ADuration;
  FLock.Acquire();
  
  try
    if (FVideo.Count >= FVideoLimit) then
      while (FVideo.Count > FVideoTrimTo) do
        begin
          FVideo.Dequeue();
          Inc(FDroppedVideo);
        end;
        
    FVideo.Enqueue(Packet);
    
    if (FVideo.Count > FPeakVideo) then
      FPeakVideo := FVideo.Count;
    FReady.SetEvent();
    
  finally
    FLock.Release();
  end;
end;


procedure TMfCastCaptureWriter.FlushAudioLocked();
begin
  
  if (Length(FPending.Data) = 0) then
    Exit;
  
  // Eight 100-ms blocks at most; retain the newest 200 ms on overload.
  if (FAudio.Count >= 8) then
    while (FAudio.Count > 2) do
      begin
        FAudio.Dequeue();
        Inc(FDroppedAudio);
      end;
      
  FAudio.Enqueue(FPending);
  FPending.Data := nil;
  FPending.Duration := 0;
  
  if (FAudio.Count > FPeakAudio) then
    FPeakAudio := FAudio.Count;
  FReady.SetEvent();
end;


function TMfCastCaptureWriter.QueueAudio(const AData: PByte;
                                         const ASize: Cardinal;
                                         const ASilent: Boolean;
                                         const ATime: Int64;
                                         ADuration: Int64): HRESULT;
var
  Offset: Integer;
  
begin
  
  Result := GetResult();
  if FAILED(Result) then
    Exit;
  
  if ASize = 0 then
    Exit(S_FALSE);
  
  if (not ASilent) and (AData = nil) then
    Exit(E_POINTER);
  
  FLock.Acquire();
  
  try
    
    // Do not concatenate across QPC gaps/discontinuities. Retain source time
    // rather than retiming queued audio after a video frame has been dropped.
    if (Length(FPending.Data) > 0) and (Abs(ATime - FPending.Time - FPending.Duration) > 20000) then
      FlushAudioLocked();
      
    Offset := Length(FPending.Data);
   
    if (Offset = 0) then
      FPending.Time := ATime;
      
    SetLength(FPending.Data,
              Offset + Integer(ASize));
    
    if ASilent then
      FillChar(FPending.Data[Offset],
               ASize,
               0)
    else 
      Move(AData^,
           FPending.Data[Offset],
           ASize);
           
    Inc(FPending.Duration,
        ADuration);
        
    if (FPending.Duration >= 1000000) then
      FlushAudioLocked(); // 100 ms
      
  finally
    FLock.Release();
  end;
end;


procedure TMfCastCaptureWriter.FlushAudio();
begin
  
  FLock.Acquire();
  
  try
    FlushAudioLocked();
    
  finally
    FLock.Release();
  end;
end;


function TMfCastCaptureWriter.TakePacket(out APacket: TMfCastCapturePacket;
                                         out AAudio: Boolean): Boolean;
begin
  
  FLock.Acquire();
  
  try
    Result := (FVideo.Count > 0) or (FAudio.Count > 0);
    
    if not Result then
      begin
        FReady.ResetEvent();
        Exit;
      end;
      
    AAudio := (FAudio.Count > 0) and ((FVideo.Count = 0) or (FAudio.Peek.Time <= FVideo.Peek.Time));
    if AAudio then
      APacket := FAudio.Dequeue()
    else APacket := FVideo.Dequeue();
    
  finally
    FLock.Release();
  end;
end;


procedure TMfCastCaptureWriter.Execute();
var
  Packet: TMfCastCapturePacket;
  Audio: Boolean;
  Hr: HRESULT;
  ComResult: HRESULT; 
  Started: Cardinal; 
  LastSlowLog: Cardinal;
  
begin
  
  ComResult := CoInitializeEx(nil,
                              COINIT_MULTITHREADED);
  LastSlowLog := 0;
  
  try
    try
      if FAILED(ComResult) then
        begin
          InterlockedExchange(FResult,
                              ComResult);
          Exit;
        end;
        
      while not Terminated do
        begin
          if not TakePacket(Packet,
                            Audio) then
            begin
              FReady.WaitFor(10);
              Continue;
            end;
            
          Started := GetTickCount();
          
          Hr := FWrite(Audio,
                       Packet.Data,
                       Packet.Time,
                       Packet.Duration);
                       
          Packet.Data := nil;
         
          if FAILED(Hr) then
            begin
              InterlockedExchange(FResult,
                                  Hr);
              Break;
            end;
            
          if Assigned(FLogger) and
             (GetTickCount() - Started >= 100) and
             ((LastSlowLog = 0) or
             (GetTickCount() - LastSlowLog >= 5000)) then
            begin
              LastSlowLog := GetTickCount();
              FLogger.Log(cllWarning, 
                          FSource,
                          Format('Slow output write: audio=%s elapsed=%d ms; capture remains independent.',
                                 [BoolToStr(Audio, True), LastSlowLog - Started]));
            end;
        end;
        
    except
      on E: Exception do
        begin
          InterlockedExchange(FResult,
                              E_FAIL);
          if Assigned(FLogger) then
            FLogger.Log(cllError,
                        FSource,
                        E.Message);
        end;
    end;
    
  finally
    if SUCCEEDED(ComResult) then
      CoUninitialize();
      
    if FAILED(GetResult()) and not Terminated and Assigned(FLogger) then
      FLogger.Log(cllError,
                  FSource,
                  Format('Output worker stopped: HRESULT $%.8x.',
                         [DWORD(GetResult())]));
  end;
end;

end.
