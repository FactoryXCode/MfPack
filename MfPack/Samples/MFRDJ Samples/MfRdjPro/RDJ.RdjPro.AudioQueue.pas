// FactoryX
//
// Copyright: FactoryX. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: RDJ.RdjPro.AudioQueue.pas
// Kind: Pascal Unit
// Release date: 24-01-2026
// Language: ENU
//
// Revision Version: 4.0.0
// Description: Provides a thread-safe FIFO queue for 32-bit floating-point PCM audio blocks.
//              It copies incoming audio data, stores format details,
//              and limits buffering to 128 blocks by dropping the oldest data when full.
//
// Company: FactoryX
// Intiator(s): Tony (maXcomX), Carmen (carmenh).
// Contributor(s): Tony Kalf (maXcomX), Carmen (carmenh).
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 24/08/2026 All                 Moby release  SDK 10.0.28000.2705  (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 or higher.
//          Please, read documentation carefully!
//
// Related objects: -
// Related projects: MfPackX400
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.28000.2705
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
unit RDJ.RdjPro.AudioQueue;

interface

uses

  {WinApi}
  WinApi.Windows,
  {System}
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  System.Generics.Collections;


type

  TRdjProPcmFloatBlock = record
    Samples: TArray<Single>;
    Frames: Integer;
    Channels: Integer;
    SampleRate: Integer;
  end;

  TRdjProPcmFloatQueue = class
  private

    FLock: TCriticalSection;
    FItems: TQueue<TRdjProPcmFloatBlock>;
    FMaxBlocks: Integer;

  public

    constructor Create();
    destructor Destroy(); override;

    procedure Clear();

    procedure PushFloat32(const pData: PSingle;
                          const Frames: Integer;
                          const Channels: Integer;
                          const SampleRate: Integer);

    function Pop(out ABlock: TRdjProPcmFloatBlock): Boolean;
  end;


implementation


constructor TRdjProPcmFloatQueue.Create();
begin

  inherited Create();

  FLock := TCriticalSection.Create();
  FItems := TQueue<TRdjProPcmFloatBlock>.Create();
  FMaxBlocks := 128;
end;


destructor TRdjProPcmFloatQueue.Destroy();
begin

  Clear();

  FItems.Free();
  FLock.Free();

  inherited Destroy();
end;


procedure TRdjProPcmFloatQueue.Clear();
var
  B: TRdjProPcmFloatBlock;

begin

  FLock.Enter();

  try

    while (FItems.Count > 0) do
      begin

        B := FItems.Dequeue();
        B := Default(TRdjProPcmFloatBlock);
      end;
  finally

    FLock.Leave();
  end;
end;


procedure TRdjProPcmFloatQueue.PushFloat32(const pData: PSingle;
                                           const Frames: Integer;
                                           const Channels: Integer;
                                           const SampleRate: Integer);
var
  B: TRdjProPcmFloatBlock;
  Count: Integer;

begin

  if (pData = nil) or
     (Frames <= 0) or
     (Channels <= 0) or
     (SampleRate <= 0) then
    Exit;

  Count := Frames * Channels;

  SetLength(B.Samples,
            Count);

  Move(pData^,
       B.Samples[0],
       Count * SizeOf(Single));

  B.Frames := Frames;
  B.Channels := Channels;
  B.SampleRate := SampleRate;

  FLock.Enter();

  try

    while (FItems.Count >= FMaxBlocks) do
      FItems.Dequeue();

    FItems.Enqueue(B);
  finally

    FLock.Leave();
  end;
end;


function TRdjProPcmFloatQueue.Pop(out ABlock: TRdjProPcmFloatBlock): Boolean;
begin

  Result := False;
  ABlock := Default(TRdjProPcmFloatBlock);

  FLock.Enter();

  try

    if (FItems.Count = 0) then
      Exit;

    ABlock := FItems.Dequeue();
    Result := True;
  finally

    FLock.Leave();
  end;
end;

end.
