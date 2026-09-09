// FactoryX
//
// Copyright (c) FactoryX, Netherlands/Australia/Germany. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Module: FxServe.Protection.pas
// Kind: Pascal Unit
// Description: Thread-safe per-address request throttling.
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.
unit FxServe.Protection;

interface

uses
  System.SysUtils,
  System.SyncObjs,
  System.Generics.Collections;

type
  TFxServeProtectionResult = (prAllowed,
                              prRateLimited,
                              prConcurrentLimited);

  TFxServeProtectionEntry = class
  public
    Tokens: Double;
    LastRefillMs: UInt64;
    BlockedUntilMs: UInt64;
    LastSeenMs: UInt64;
    ActiveRequests: Integer;
  end;

  TFxServeRequestProtection = class
  private
    FRequestsPerMinute: Integer;
    FBurst: Integer;
    FMaxConcurrentPerAddress: Integer;
    FBlockSeconds: Integer;
    FEntries: TObjectDictionary<string, TFxServeProtectionEntry>;
    FLock: TCriticalSection;
    FCheckCount: Cardinal;

    procedure Prune(ANowMs: UInt64);
  public
    constructor Create(ARequestsPerMinute,
                       ABurst,
                       AMaxConcurrentPerAddress,
                       ABlockSeconds: Integer);
    destructor Destroy(); override;

    function TryBeginRequest(const AAddress: string;
                             out ARetryAfterSeconds: Integer): TFxServeProtectionResult;
    function TryBeginRequestAt(const AAddress: string;
                               ANowMs: UInt64;
                               out ARetryAfterSeconds: Integer): TFxServeProtectionResult;
    procedure EndRequest(const AAddress: string);
  end;

implementation

uses
  WinApi.Windows;

const
  ENTRY_IDLE_MS: UInt64 = 10 * 60 * 1000;


function MonotonicMilliseconds(): UInt64;
var
  Counter: Int64;
  Frequency: Int64;
begin
  if QueryPerformanceCounter(Counter) and
     QueryPerformanceFrequency(Frequency) and
     (Frequency > 0) then
    Result := UInt64(Counter div Frequency) * 1000 +
              UInt64((Counter mod Frequency) * 1000 div Frequency)
  else
    Result := GetTickCount();
end;


constructor TFxServeRequestProtection.Create(ARequestsPerMinute,
                                             ABurst,
                                             AMaxConcurrentPerAddress,
                                             ABlockSeconds: Integer);
begin
  inherited Create();

  FRequestsPerMinute := ARequestsPerMinute;
  FBurst := ABurst;
  FMaxConcurrentPerAddress := AMaxConcurrentPerAddress;
  FBlockSeconds := ABlockSeconds;
  FEntries := TObjectDictionary<string, TFxServeProtectionEntry>.Create([doOwnsValues]);
  FLock := TCriticalSection.Create;
end;


destructor TFxServeRequestProtection.Destroy();
begin
  FLock.Free;
  FEntries.Free;

  inherited Destroy();
end;


procedure TFxServeRequestProtection.Prune(ANowMs: UInt64);
var
  Keys: TArray<string>;
  Key: string;
  Entry: TFxServeProtectionEntry;
  MaxIdleMs: UInt64;
begin
  MaxIdleMs := ENTRY_IDLE_MS;
  if UInt64(FBlockSeconds) * 1000 > MaxIdleMs then
    MaxIdleMs := UInt64(FBlockSeconds) * 1000;

  Keys := FEntries.Keys.ToArray;
  for Key in Keys do
    if FEntries.TryGetValue(Key, Entry) and
       (Entry.ActiveRequests = 0) and
       (ANowMs >= Entry.LastSeenMs) and
       (ANowMs - Entry.LastSeenMs >= MaxIdleMs) then
      FEntries.Remove(Key);
end;


function TFxServeRequestProtection.TryBeginRequest(
  const AAddress: string;
  out ARetryAfterSeconds: Integer): TFxServeProtectionResult;
begin
  Result := TryBeginRequestAt(AAddress,
                              MonotonicMilliseconds(),
                              ARetryAfterSeconds);
end;


function TFxServeRequestProtection.TryBeginRequestAt(
  const AAddress: string;
  ANowMs: UInt64;
  out ARetryAfterSeconds: Integer): TFxServeProtectionResult;
var
  Key: string;
  Entry: TFxServeProtectionEntry;
  ElapsedMs: UInt64;
  RemainingMs: UInt64;
begin
  ARetryAfterSeconds := 0;
  Key := Trim(AAddress);

  // Do not combine unrelated clients into one bucket when no address exists.
  if Key = '' then
    Exit(prAllowed);

  FLock.Acquire;
  try
    Inc(FCheckCount);
    if (FCheckCount and $FF) = 0 then
      Prune(ANowMs);

    if not FEntries.TryGetValue(Key, Entry) then
      begin
        Entry := TFxServeProtectionEntry.Create;
        Entry.Tokens := FBurst;
        Entry.LastRefillMs := ANowMs;
        Entry.LastSeenMs := ANowMs;
        FEntries.Add(Key, Entry);
      end;

    Entry.LastSeenMs := ANowMs;

    if Entry.BlockedUntilMs > ANowMs then
      begin
        RemainingMs := Entry.BlockedUntilMs - ANowMs;
        ARetryAfterSeconds := (RemainingMs + 999) div 1000;
        Exit(prRateLimited);
      end;

    if ANowMs >= Entry.LastRefillMs then
      begin
        ElapsedMs := ANowMs - Entry.LastRefillMs;
        Entry.Tokens := Entry.Tokens +
          (ElapsedMs * (FRequestsPerMinute / 60000.0));
        if Entry.Tokens > FBurst then
          Entry.Tokens := FBurst;
        Entry.LastRefillMs := ANowMs;
      end;

    if Entry.ActiveRequests >= FMaxConcurrentPerAddress then
      begin
        ARetryAfterSeconds := 1;
        Exit(prConcurrentLimited);
      end;

    if Entry.Tokens < 1.0 then
      begin
        Entry.BlockedUntilMs := ANowMs + UInt64(FBlockSeconds) * 1000;
        ARetryAfterSeconds := FBlockSeconds;
        Exit(prRateLimited);
      end;

    Entry.Tokens := Entry.Tokens - 1.0;
    Inc(Entry.ActiveRequests);
    Result := prAllowed;
  finally
    FLock.Release;
  end;
end;


procedure TFxServeRequestProtection.EndRequest(const AAddress: string);
var
  Key: string;
  Entry: TFxServeProtectionEntry;
begin
  Key := Trim(AAddress);
  if Key = '' then
    Exit;

  FLock.Acquire;
  try
    if FEntries.TryGetValue(Key, Entry) and
       (Entry.ActiveRequests > 0) then
      Dec(Entry.ActiveRequests);
  finally
    FLock.Release;
  end;
end;

end.
