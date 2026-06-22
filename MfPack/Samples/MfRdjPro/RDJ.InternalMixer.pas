// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - CoreAudio - WASAPI
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: RDJ.InternalMixer.pas
// Kind: Pascal / Delphi unit
// Release date: 02-04-2023
// Language: ENU
//
// Revision Version: 3.2.0
// Description: Mixer for all channels to main output.
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX).
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 05/05/2026 All                 Bauhaus release  SDK 10.0.26100.4654 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 8 or later.
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
//==============================================================================
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
unit RDJ.InternalMixer;

interface

uses

  {WinApi}
  WinApi.Windows,
  WinApi.WinApiTypes,
  {System}
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  System.Math,
  {CoreAudioApi}
  WinApi.CoreAudioApi.AudioClient,
  {Application}
  MfWasApiEffectsRack;

type

  TRDJReadOutputPcmFloat32Event = function(const Frames: Integer;
                                           const OutBuffer: PSingle;
                                           out Flags: DWORD): HRESULT of object;

  TRDJMixerChannel = class
  private

    FDeckEngineObj: TObject;
    FOnReadOutputPcmFloat32: TRDJReadOutputPcmFloat32Event;
    FEnabled: Boolean;
    FCueEnabled: Boolean;
    FMuted: Boolean;
    FVolL: Single;
    FVolR: Single;
    FCueVolL: Single;
    FCueVolR: Single;
    FWorkBuf: TArray<Single>;
    FWorkBufFrames: Integer;

  public

    constructor Create();

    property DeckEngineObj: TObject read FDeckEngineObj write FDeckEngineObj;
    property OnReadOutputPcmFloat32: TRDJReadOutputPcmFloat32Event read FOnReadOutputPcmFloat32 write FOnReadOutputPcmFloat32;
    property Enabled: Boolean read FEnabled write FEnabled;
    property CueEnabled: Boolean read FCueEnabled write FCueEnabled;
    property Muted: Boolean read FMuted write FMuted;
    property VolL: Single read FVolL write FVolL;
    property VolR: Single read FVolR write FVolR;
    property CueVolL: Single read FCueVolL write FCueVolL;
    property CueVolR: Single read FCueVolR write FCueVolR;
    property WorkBuf: TArray<Single> read FWorkBuf write FWorkBuf;
    property WorkBufFrames: Integer read FWorkBufFrames write FWorkBufFrames;
  end;

  TRDJInternalMixer = class
  private

    FChannels: TObjectList<TRDJMixerChannel>;
    FChannelSnapshot: TArray<TRDJMixerChannel>;
    FMasterRack: TMfWasApiEffectsRack;
    FSampleRate: Integer;
    FChannelCount: Integer;
    FSmallWorkBufHits: Integer;
    FPullErrors: Integer;
    FShuttingDown: Boolean;
    FDestroying: Boolean;

  private

    procedure RebuildChannelSnapshot();

    procedure EnsureWorkBuffer(AChannel: TRDJMixerChannel;
                               const Frames: Integer);

    procedure ClearFloatBuffer(const pData: PSingle;
                               const SampleCount: Integer);

    procedure MixAddStereo(const InBuf: PSingle;
                           const OutBuf: PSingle;
                           const Frames: Integer;
                           const GainL: Single;
                           const GainR: Single);

    function PullChannelPcm(AChannel: TRDJMixerChannel;
                            const Frames: Integer;
                            out AFlags: DWORD): HRESULT;
  public

    constructor Create();
    destructor Destroy(); override;

    procedure BeginShutdown();

    function AddChannel(): TRDJMixerChannel;
    procedure RemoveChannel(AChannel: TRDJMixerChannel);
    function FindChannelByDeckEngine(const ADeckEngine: TObject): TRDJMixerChannel;
    procedure PrepareWorkBuffers(const Frames: Integer);
    procedure ClearChannels();

    procedure SetFormat(const ASampleRate,
                        AChannelCount: Integer);

    function MixBlock(const Frames: Integer;
                      const OutMaster: PSingle;
                      const OutCue: PSingle): HRESULT;

    property MasterRack: TMfWasApiEffectsRack read FMasterRack write FMasterRack;
    property SampleRate: Integer read FSampleRate;
    property ChannelCount: Integer read FChannelCount;
    property Channels: TObjectList<TRDJMixerChannel> read FChannels;
    property SmallWorkBufHits: Integer read FSmallWorkBufHits;
    property PullErrors: Integer read FPullErrors;
    property ShuttingDown: Boolean read FShuttingDown;
  end;

implementation

{ TRDJMixerChannel }

constructor TRDJMixerChannel.Create();
begin

  inherited Create;

  FDeckEngineObj := nil;
  FOnReadOutputPcmFloat32 := nil;
  FEnabled := True;
  FCueEnabled := False;
  FMuted := False;
  FVolL := 1.0;
  FVolR := 1.0;
  FCueVolL := 1.0;
  FCueVolR := 1.0;
  FWorkBufFrames := 0;
end;

{ TRDJInternalMixer }

constructor TRDJInternalMixer.Create;
begin
  inherited Create;

  FChannels := TObjectList<TRDJMixerChannel>.Create(True);

  FSampleRate := 0;
  FChannelCount := 2;
  FSmallWorkBufHits := 0;
  FPullErrors := 0;
  FShuttingDown := False;
  FDestroying := False;
end;


destructor TRDJInternalMixer.Destroy();
begin

  FDestroying := True;
  FShuttingDown := True;
  SetLength(FChannelSnapshot,
            0);

  FChannels.Free;

  inherited Destroy();
end;


procedure TRDJInternalMixer.BeginShutdown();
begin

  FShuttingDown := True;
  SetLength(FChannelSnapshot,
            0);
end;


function TRDJInternalMixer.AddChannel(): TRDJMixerChannel;
begin

  if FShuttingDown or FDestroying then
    Exit(nil);

  Result := TRDJMixerChannel.Create;
  FChannels.Add(Result);
  RebuildChannelSnapshot();
end;


procedure TRDJInternalMixer.RebuildChannelSnapshot();
var
  i: Integer;
  NewSnapshot: TArray<TRDJMixerChannel>;

begin

  if FDestroying then
    Exit;

  SetLength(NewSnapshot,
            FChannels.Count);
  for i := 0 to FChannels.Count - 1 do
    NewSnapshot[i] := FChannels[i];

  FChannelSnapshot := NewSnapshot;
end;


function TRDJInternalMixer.FindChannelByDeckEngine(const ADeckEngine: TObject): TRDJMixerChannel;
var
  i: Integer;
  Snapshot: TArray<TRDJMixerChannel>;
  Ch: TRDJMixerChannel;

begin

  Result := nil;

  if (ADeckEngine = nil) then
    Exit;

  Snapshot := FChannelSnapshot;

  for i := 0 to High(Snapshot) do
    begin

      Ch := Snapshot[i];
      if (Ch <> nil) and (Ch.DeckEngineObj = ADeckEngine) then
        Exit(Ch);
    end;
end;


procedure TRDJInternalMixer.PrepareWorkBuffers(const Frames: Integer);
var
  i: Integer;
  Snapshot: TArray<TRDJMixerChannel>;
  Ch: TRDJMixerChannel;

begin

  if (Frames <= 0) or
     FShuttingDown or
     FDestroying then
    Exit;

  Snapshot := FChannelSnapshot;

  for i := 0 to High(Snapshot) do
    begin

      Ch := Snapshot[i];
      if (Ch = nil) then
        Continue;

      if (Length(Ch.FWorkBuf) < (Frames * 2)) then
        SetLength(Ch.FWorkBuf,
                  Frames * 2);

      Ch.FWorkBufFrames := Frames;
    end;
end;


procedure TRDJInternalMixer.RemoveChannel(AChannel: TRDJMixerChannel);
begin

  if (AChannel = nil) or
     FShuttingDown or
     FDestroying then
    Exit;

  FChannels.Remove(AChannel);
  RebuildChannelSnapshot;
end;


procedure TRDJInternalMixer.ClearChannels;
begin

  if FDestroying then
    Exit;

  FChannels.Clear();
  SetLength(FChannelSnapshot, 0);
end;


procedure TRDJInternalMixer.SetFormat(const ASampleRate,
                                      AChannelCount: Integer);
begin

  FSampleRate := ASampleRate;
  FChannelCount := AChannelCount;
end;


procedure TRDJInternalMixer.ClearFloatBuffer(const pData: PSingle;
                                             const SampleCount: Integer);
begin

  if (pData <> nil) and (SampleCount > 0) then
    FillChar(pData^,
             SampleCount * SizeOf(Single),
             0);
end;


procedure TRDJInternalMixer.EnsureWorkBuffer(AChannel: TRDJMixerChannel;
                                             const Frames: Integer);
var
  NeedSamples: Integer;

begin

  if (AChannel = nil) then
    Exit;

  NeedSamples := Frames * 2;

  if (NeedSamples <= 0) then
    Exit;

  if (Length(AChannel.FWorkBuf) < NeedSamples) then
    SetLength(AChannel.FWorkBuf,
              NeedSamples);

  AChannel.WorkBufFrames := Frames;
end;


procedure TRDJInternalMixer.MixAddStereo(const InBuf: PSingle;
                                         const OutBuf: PSingle;
                                         const Frames: Integer;
                                         const GainL: Single;
                                         const GainR: Single);
var
  i: Integer;
  Src: PSingle;
  Dst: PSingle;

begin

  if (InBuf = nil) or
     (OutBuf = nil) or
     (Frames <= 0) then
    Exit;

  if ((GainL = 0) and (GainR = 0)) then
    Exit;

  Src := InBuf;
  Dst := OutBuf;

  for i := 0 to Frames - 1 do
    begin

      Dst^ := Dst^ + (Src^ * GainL);
      Inc(Src);
      Inc(Dst);

      Dst^ := Dst^ + (Src^ * GainR);
      Inc(Src);
      Inc(Dst);
    end;
end;


function TRDJInternalMixer.PullChannelPcm(AChannel: TRDJMixerChannel;
                                          const Frames: Integer;
                                          out AFlags: DWORD): HRESULT;
begin

  Result := S_FALSE;
  AFlags := 0;

  if FShuttingDown or FDestroying then
    begin

      AFlags := AUDCLNT_BUFFERFLAGS_SILENT;
      Exit(S_OK);
    end;

  if (AChannel = nil) or not Assigned(AChannel.OnReadOutputPcmFloat32) then
    Exit;

  EnsureWorkBuffer(AChannel,
                   Frames);

  if Length(AChannel.WorkBuf) < (Frames * 2) then
    begin

      Inc(FSmallWorkBufHits);
      AFlags := AUDCLNT_BUFFERFLAGS_SILENT;
      Exit(E_FAIL);
    end;

  FillChar(AChannel.FWorkBuf[0],
           Frames * 2 * SizeOf(Single),
           0);

  Result := AChannel.OnReadOutputPcmFloat32(Frames,
                                            @AChannel.WorkBuf[0],
                                            AFlags);
end;


function TRDJInternalMixer.MixBlock(const Frames: Integer;
                                    const OutMaster: PSingle;
                                    const OutCue: PSingle): HRESULT;
var
  i: Integer;
  Snapshot: TArray<TRDJMixerChannel>;
  Ch: TRDJMixerChannel;
  Flags: DWORD;
  hr: HRESULT;
  Enabled: Boolean;
  CueEnabled: Boolean;
  Muted: Boolean;
  VolL: Single;
  VolR: Single;
  CueVolL: Single;
  CueVolR: Single;

begin

  Result := S_OK;

  if (Frames <= 0) or
     (OutMaster = nil) or
     (OutCue = nil) then
    Exit(E_INVALIDARG);

  if (FChannelCount <> 2) then
    Exit(E_NOTIMPL);

  ClearFloatBuffer(OutMaster,
                   Frames * 2);
  ClearFloatBuffer(OutCue,
                   Frames * 2);

  if FShuttingDown or FDestroying then
    Exit(S_OK);

  Snapshot := FChannelSnapshot;

  for i := 0 to High(Snapshot) do
    begin

      if FShuttingDown or FDestroying then
        Exit(S_OK);

      Flags := 0;
      Ch := Snapshot[i];

      if (Ch = nil) then
        Continue;

      Enabled := Ch.Enabled;
      CueEnabled := Ch.CueEnabled;
      Muted := Ch.Muted;
      VolL := Ch.VolL;
      VolR := Ch.VolR;
      CueVolL := Ch.CueVolL;
      CueVolR := Ch.CueVolR;

      if not Enabled or
         not Assigned(Ch.OnReadOutputPcmFloat32) then
        Continue;

      hr := PullChannelPcm(Ch,
                           Frames,
                           Flags);
      if FAILED(hr) then
        begin

          Inc(FPullErrors);
          Continue;
        end;

      if Length(Ch.WorkBuf) < (Frames * 2) then
        Continue;

      if ((Flags and AUDCLNT_BUFFERFLAGS_SILENT) = 0) then
        begin

          if not Muted then
            MixAddStereo(@Ch.FWorkBuf[0],
                         OutMaster,
                         Frames,
                         VolL,
                         VolR);

          if CueEnabled then
            MixAddStereo(@Ch.FWorkBuf[0],
                         OutCue,
                         Frames,
                         CueVolL,
                         CueVolR);
        end;
    end;

end;

end.
