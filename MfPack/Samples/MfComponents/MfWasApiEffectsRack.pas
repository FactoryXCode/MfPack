// FactoryX
//
// Copyright: FactoryX. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: MfWasApiEffectsRack.pas
// Kind: Pascal Unit
// Release date: 24-01-2026
// Language: ENU
//
// Revision Version: 4.0.0
// Description: The host-side chain builder.
//              Holds an ordered list (slots) of FX providers and runs them in sequence on blocks of PCM.
//
// How to use it in a NEW (non-WASAPIEngine) player:
// =================================================
//
// 1) Convert your engine's buffer to Float32 interleaved
//    PcmToFloat32Interleaved(InPcm, InBytes, InWfx,
//                            FloatBuf, Frames, Channels, SampleRate);
//
// 2) Process via rack
//    MfWasApiEffectsRack1.ProcessFloat32(@FloatBuf[0], Frames, Channels, SampleRate);
//
// 3) Convert back to your engine's PCM format.
//    Float32InterleavedToPcm(@FloatBuf[0], Frames, Channels,
//                            OutPcm, OutBytes, OutWfx);
//
// Note:
//   ProcessFloat32 uses the same MFT chaining method as the existing MF sample/buffer approach,
//   so it works with the existing EQ/Flanger/Dynamics MFTs.
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
unit MfWasApiEffectsRack;

interface

uses

  {WinApi}
  WinApi.Windows,
  {System}
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  System.Math,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfApi,
  WinApi.MediaFoundationApi.MfTransform,
  WinApi.MediaFoundationApi.MfObjects,
  WinApi.MediaFoundationApi.MfError,
  {WinMM}
  WinApi.WinMM.MMeApi,
  WinApi.WinMM.MMReg,
  {Application}
  MfWasApiFxIntf,
  MfWasApiFxComponentBase,
  PcmLib;

type

  TMfWasApiEffectsRack = class;

  TMfWasApiFxSlot = class(TCollectionItem)
  private

    FEnabled: Boolean;
    FEffect: TMfWasApiFxComponentBase;

    procedure SetEnabled(const Value: Boolean);
    procedure SetEffect(const Value: TMfWasApiFxComponentBase);

  public

    constructor Create(Collection: TCollection); override;

  published

    property Enabled: Boolean read FEnabled write SetEnabled default True;
    // Assign a component that implements IMfWasApiFxProvider (your EQ/Flanger/Dynamics components).
    property Effect: TMfWasApiFxComponentBase read FEffect write SetEffect;
  end;


  TMfWasApiFxSlots = class(TOwnedCollection)
  private

    function GetItem(Index: Integer): TMfWasApiFxSlot;
    procedure SetItem(Index: Integer;
                      const Value: TMfWasApiFxSlot);
  public

    constructor Create(AOwner: TPersistent);
    destructor Destroy(); override;

    property Items[Index: Integer]: TMfWasApiFxSlot read GetItem write SetItem; default;
  end;

  // Effects rack: chains IMfWasApiFxProvider MFTs in order.
  // Called from engine thread via Engine.OnProcessPcm.

  TMfWasApiEffectsRack = class(TComponent)
  private

    FSlots: TMfWasApiFxSlots;
    FLock: TCriticalSection;

    // Cached Float32 media type (for ProcessFloat32 / generic processing).
    FFloat32Type: IMFMediaType;
    FFloat32SR: Integer;
    FFloat32Ch: Integer;

    // Work buffer for PCM -> Float32 -> PCM processing
    FWorkFloat: PSingle;
    FWorkCapSamples: Integer;

    // NOTE: Make Slots writable so Delphi's streaming system can
    //       reliably load collection items from the DFM into this instance.
    //       Without a setter, some setups end up with Slots.Count = 0 at runtime.
    procedure SetSlots(const Value: TMfWasApiFxSlots);

    function EnsureFloat32Type(const SampleRate,
                               Channels: Integer): HRESULT;

    function ConfigureMftFloat32(const AMft: IMFTransform;
                                 const SampleRate,
                                 Channels: Integer): HRESULT;

    function ProcessMftInPlace(const AMft: IMFTransform;
                               pData: PByte;
                               const ByteCount: DWORD): HRESULT;
  protected

    procedure Notification(AComponent: TComponent;
                           Operation: TOperation); override;

  public

    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;

    // Engine hook (engine thread)
    procedure ProcessPcm(Sender: TObject;
                         pData: PByte;
                         const ByteCount: DWORD;
                         pwfx: PWAVEFORMATEX);

    // Generic hook for external engines that already operate on interleaved Float32.
    // pData points to Frames*Channels float samples.
    procedure ProcessFloat32(pData: PSingle;
                             Frames,
                             Channels,
                             SampleRate: Integer);


    // Slot lookup helpers. Useful when users can reorder slots at runtime.
    // Slot.Enabled is the single source of truth for bypass.
    function FindSlotByEffect(const AEffect: TMfWasApiFxComponentBase): TMfWasApiFxSlot;
    function FindSlotByEffectName(const AName: string): TMfWasApiFxSlot;
    function FindFirstSlotByEffectClass(const AClass: TClass): TMfWasApiFxSlot;
  published

    property Slots: TMfWasApiFxSlots read FSlots write SetSlots;
  end;

procedure Register;


implementation


procedure Register;
begin

  RegisterComponents('MfPack Core Audio Samples',
                     [TMfWasApiEffectsRack]);
end;

{ TMfWasApiFxSlot }

constructor TMfWasApiFxSlot.Create(Collection: TCollection);
begin

  inherited Create(Collection);

  FEnabled := True;
  FEffect := nil;
end;


procedure TMfWasApiFxSlot.SetEnabled(const Value: Boolean);
begin

  FEnabled := Value;
end;


procedure TMfWasApiFxSlot.SetEffect(const Value: TMfWasApiFxComponentBase);
var
  Rack: TMfWasApiEffectsRack;

begin

  if (FEffect = Value) then
    Exit;

  Rack := nil;

  if (Collection is TOwnedCollection) then
    Rack := TMfWasApiEffectsRack(TOwnedCollection(Collection).Owner);

  if (Rack <> nil) and (FEffect <> nil) then
    FEffect.RemoveFreeNotification(Rack);

  FEffect := Value;

  if (Rack <> nil) and (FEffect <> nil) then
    FEffect.FreeNotification(Rack);
end;


{ TMfWasApiFxSlots }

constructor TMfWasApiFxSlots.Create(AOwner: TPersistent);
begin

  inherited Create(AOwner,
                   TMfWasApiFxSlot);
end;


destructor TMfWasApiFxSlots.Destroy();
begin

  inherited;
end;


function TMfWasApiFxSlots.GetItem(Index: Integer): TMfWasApiFxSlot;
begin

  Result := TMfWasApiFxSlot(inherited GetItem(Index));
end;


procedure TMfWasApiFxSlots.SetItem(Index: Integer; const Value: TMfWasApiFxSlot);
begin

  inherited SetItem(Index,
                    Value);
end;


{ TMfWasApiEffectsRack helpers }

procedure TMfWasApiEffectsRack.SetSlots(const Value: TMfWasApiFxSlots);
begin

  // Delphi streaming will call the setter to load the collection from DFM.
  // We keep our collection instance and copy content into it.
  if (Value = nil) then
    Exit;

  FSlots.Assign(Value);
end;


{ TMfWasApiEffectsRack }

constructor TMfWasApiEffectsRack.Create(AOwner: TComponent);
begin

  inherited Create(AOwner);

  FLock := TCriticalSection.Create();
  FSlots := TMfWasApiFxSlots.Create(Self);

  // We'll lazily build Float32 media types on first use.
  FFloat32Type := nil;
  FFloat32SR := 0;
  FFloat32Ch := 0;

  FWorkFloat := nil;
  FWorkCapSamples := 0;
end;


destructor TMfWasApiEffectsRack.Destroy();
begin

  FreeAndNil(FSlots);
  FFloat32Type := nil;
  FLock.Free();

  if (FWorkFloat <> nil) then
    FreeMem(FWorkFloat);
  FWorkFloat := nil;
  FWorkCapSamples := 0;

  inherited;
end;


procedure TMfWasApiEffectsRack.Notification(AComponent: TComponent;
                                            Operation: TOperation);
var
  i: Integer;
begin
  inherited;

  if (Operation <> opRemove) then
    Exit;

  if (FSlots = nil) then
    Exit;

  for i := 0 to FSlots.Count - 1 do
  begin
    if (FSlots[i] <> nil) and (FSlots[i].Effect = AComponent) then
      FSlots[i].Effect := nil;
  end;
end;


function TMfWasApiEffectsRack.EnsureFloat32Type(const SampleRate,
                                                Channels: Integer): HRESULT;
var
  hr: HRESULT;
  blockAlign: Word;
  avgBps: INT32;

begin

  if (SampleRate <= 0) or (Channels <= 0) then
    Exit(E_INVALIDARG);

  if (FFloat32Type <> nil) and
     (FFloat32SR = SampleRate) and
     (FFloat32Ch = Channels) then
    Exit(S_OK);

  FFloat32Type := nil;
  FFloat32SR := 0;
  FFloat32Ch := 0;

  hr := MFCreateMediaType(FFloat32Type);
  if FAILED(hr) then
    Exit(hr);

  hr := FFloat32Type.SetGUID(MF_MT_MAJOR_TYPE,
                             MFMediaType_Audio);
  if FAILED(hr) then
    Exit(hr);

  hr := FFloat32Type.SetGUID(MF_MT_SUBTYPE,
                             MFAudioFormat_Float);
  if FAILED(hr) then
    Exit(hr);

  blockAlign := Channels * SizeOf(Single);
  avgBps := Cardinal(SampleRate) * Cardinal(blockAlign);

  hr := FFloat32Type.SetUINT32(MF_MT_AUDIO_BITS_PER_SAMPLE,
                               32);
  if FAILED(hr) then
    Exit(hr);

  hr := FFloat32Type.SetUINT32(MF_MT_AUDIO_SAMPLES_PER_SECOND,
                               SampleRate);
  if FAILED(hr) then
    Exit(hr);

  hr := FFloat32Type.SetUINT32(MF_MT_AUDIO_NUM_CHANNELS,
                               Channels);
  if FAILED(hr) then
    Exit(hr);

  hr := FFloat32Type.SetUINT32(MF_MT_AUDIO_BLOCK_ALIGNMENT,
                               blockAlign);
  if FAILED(hr) then
    Exit(hr);

  hr := FFloat32Type.SetUINT32(MF_MT_AUDIO_AVG_BYTES_PER_SECOND,
                               avgBps);
  if FAILED(hr) then
    Exit(hr);

  // Keep a stable key for cache.
  FFloat32SR := SampleRate;
  FFloat32Ch := Channels;

  Result := S_OK;
end;


function TMfWasApiEffectsRack.ConfigureMftFloat32(const AMft: IMFTransform;
                                                  const SampleRate,
                                                  Channels: Integer): HRESULT;
var
  hr: HRESULT;

begin

  if (AMft = nil) then
    Exit(E_POINTER);

  hr := EnsureFloat32Type(SampleRate,
                          Channels);
  if FAILED(hr) then
    Exit(hr);

  // Many custom MFTs accept identical in/out types.
  // We set both. If an MFT doesn't like it, it should return a failure and we stop processing.
  hr := AMft.SetInputType(0,
                          FFloat32Type,
                          0);
  if FAILED(hr) then
    Exit(hr);

  hr := AMft.SetOutputType(0,
                           FFloat32Type,
                           0);
  if FAILED(hr) then
    Exit(hr);

  // Notify streaming state (some MFTs ignore, some rely on it).
  AMft.ProcessMessage(MFT_MESSAGE_NOTIFY_BEGIN_STREAMING,
                      0);

  AMft.ProcessMessage(MFT_MESSAGE_NOTIFY_START_OF_STREAM,
                      0);

  Result := S_OK;
end;


function TMfWasApiEffectsRack.ProcessMftInPlace(const AMft: IMFTransform;
                                                pData: PByte;
                                                const ByteCount: DWORD): HRESULT;
var
  hr: HRESULT;
  inSample,
  outSample: IMFSample;
  inBuf,
  outBuf: IMFMediaBuffer;
  outData: MFT_OUTPUT_DATA_BUFFER;
  status: DWORD;
  pIn,
  pOut: PByte;
  cbCur: DWORD;

begin

  if (AMft = nil) or
     (pData = nil) or
     (ByteCount = 0) then
    Exit(S_OK);

  // --- Create input sample ---
  hr := MFCreateSample(inSample);
  if FAILED(hr) then
    Exit(hr);

  hr := MFCreateMemoryBuffer(ByteCount,
                             inBuf);
  if FAILED(hr) then
    Exit(hr);

  hr := inBuf.Lock(pIn,
                   nil,
                   nil);
  if FAILED(hr) then
    Exit(hr);

  try

    Move(pData^,
         pIn^,
         ByteCount);
  finally

    inBuf.Unlock;
  end;

  hr := inBuf.SetCurrentLength(ByteCount);
  if FAILED(hr) then
    Exit(hr);

  hr := inSample.AddBuffer(inBuf);
  if FAILED(hr) then
    Exit(hr);

  // --- Create output sample (MUST be provided) ---
  hr := MFCreateSample(outSample);
  if FAILED(hr) then
    Exit(hr);

  hr := MFCreateMemoryBuffer(ByteCount,
                             outBuf);
  if FAILED(hr) then
    Exit(hr);

  hr := outSample.AddBuffer(outBuf);
  if FAILED(hr) then
    Exit(hr);

  // --- Feed ---
  hr := AMft.ProcessInput(0,
                          inSample,
                          0);

  if (hr = MF_E_NOTACCEPTING) then
    begin

      ZeroMemory(@outData,
                 SizeOf(outData));

      outData.pSample := outSample;
      status := 0;

      hr := AMft.ProcessOutput(0,
                               1,
                               @outData, status);
      if FAILED(hr) and (hr <> MF_E_TRANSFORM_NEED_MORE_INPUT) then
        Exit(hr);

      hr := AMft.ProcessInput(0,
                              inSample,
                              0);
    end;

  if FAILED(hr) then
    Exit(hr);

  // --- Pull output ---
  ZeroMemory(@outData,
             SizeOf(outData));

  outData.pSample := outSample;
  status := 0;

  hr := AMft.ProcessOutput(0,
                           1,
                           @outData,
                           status);
  if (hr = MF_E_TRANSFORM_NEED_MORE_INPUT) then
    Exit(S_OK);

  if FAILED(hr) then
    Exit(hr);

  // --- Copy back ---
  cbCur := 0;

  hr := outBuf.GetCurrentLength(cbCur);
  if FAILED(hr) then
    Exit(hr);

  hr := outBuf.Lock(pOut,
                    nil,
                    nil);
  if FAILED(hr) then
    Exit(hr);

  try

    // Only copy what the MFT actually produced.
    Move(pOut^,
         pData^,
         Min(cbCur, ByteCount));

    // Optional: if cbCur < ByteCount, zero the tail to avoid old samples bleeding through.
    if (cbCur < ByteCount) then
      FillChar((pData + cbCur)^,
                ByteCount - cbCur,
                0);
  finally

    outBuf.Unlock();
  end;

  Result := S_OK;
end;



procedure TMfWasApiEffectsRack.ProcessPcm(Sender: TObject;
                                          pData: PByte;
                                          const ByteCount: DWORD;
                                          pwfx: PWAVEFORMATEX);
var
  hr: HRESULT;
  i: Integer;
  slot: TMfWasApiFxSlot;
  prov: IMfWasApiFxProvider;
  mft: IMFTransform;
  Bits: Integer;
  IsFloat: Boolean;
  BytesPerSample: Integer;
  Samples: Integer;
  FloatByteCount: DWORD;
  pFloat: PSingle;

begin

  if (csDesigning in ComponentState) then
    Exit;

  if (pData = nil) or
     (ByteCount = 0) or
     (pwfx = nil) then
    Exit;

  if not GetWfxBitsAndFloat(pwfx,
                            Bits,
                            IsFloat) then
    Exit;

  // Fast path: already Float32 interleaved.
  if IsFloat then
    begin

      FLock.Enter();

      try

        for i := 0 to FSlots.Count - 1 do
          begin

            slot := FSlots[i];
            if (slot = nil) or
               (not slot.Enabled) or
               (slot.Effect = nil) then
              Continue;

            if not Supports(slot.Effect,
                            IMfWasApiFxProvider,
                            prov) then
              Continue;

            mft := prov.GetMft();
            if (mft = nil) then
              Continue;

            hr := ConfigureMftFloat32(mft,
                                      pwfx^.nSamplesPerSec,
                                      pwfx^.nChannels);
            if FAILED(hr) then
              Break;

            hr := ProcessMftInPlace(mft,
                                    pData,
                                    ByteCount);
            if FAILED(hr) then
              Break;
          end;
      finally

        FLock.Leave();
      end;

      Exit;
    end;

  // PCM path: convert PCM -> Float32 -> process -> convert back.
  BytesPerSample := Bits div 8;
  if (BytesPerSample <= 0) then
    Exit;

  Samples := Integer(ByteCount div DWORD(BytesPerSample));
  if (Samples <= 0) then
    Exit;

  EnsureDynFloatBuf(FWorkFloat,
                    FWorkCapSamples,
                    Samples);

  pFloat := FWorkFloat;

  case Bits of
    16: Int16ToFloat(pData,
                     pFloat,
                     Samples);

    24: Int24ToFloat(pData,
                     pFloat,
                     Samples);

    32: Int32ToFloat(pData,
                     pFloat,
                     Samples);
  else
    Exit; // unsupported PCM bit depth
  end;

  // Safety: avoid NaN/Inf propagation if any upstream produced junk.
  MfSanitizeInterleavedFloat32(pFloat,
                               Samples,
                               16.0);

  FloatByteCount := DWORD(Samples * SizeOf(Single));

  FLock.Enter();

  try

    for i := 0 to FSlots.Count - 1 do
      begin

        slot := FSlots[i];
        if (slot = nil) or
           (not slot.Enabled) or
           (slot.Effect = nil) then
          Continue;

        if not Supports(slot.Effect,
                        IMfWasApiFxProvider,
                        prov) then
          Continue;

        mft := prov.GetMft;
        if (mft = nil) then
          Continue;

        hr := ConfigureMftFloat32(mft,
                                  pwfx^.nSamplesPerSec,
                                  pwfx^.nChannels);
        if FAILED(hr) then
          Break;

        hr := ProcessMftInPlace(mft,
                                PByte(pFloat),
                                FloatByteCount);
        if FAILED(hr) then
          Break;
      end;
  finally

    FLock.Leave();
  end;

  case Bits of
    16: FloatToInt16(pFloat,
                     pData,
                     Samples);

    24: FloatToInt24(pFloat,
                     pData, Samples);

    32: FloatToInt32(pFloat,
                     pData,
                     Samples);
  end;
end;


procedure TMfWasApiEffectsRack.ProcessFloat32(pData: PSingle;
                                              Frames,
                                              Channels,
                                              SampleRate: Integer);
var
  hr: HRESULT;
  i: Integer;
  slot: TMfWasApiFxSlot;
  prov: IMfWasApiFxProvider;
  mft: IMFTransform;
  byteCount: DWORD;

begin

  if (csDesigning in ComponentState) then
    Exit;

  if (pData = nil) or
     (Frames <= 0) or
     (Channels <= 0) or
     (SampleRate <= 0) then
    Exit;

  byteCount := DWORD(Frames * Channels * SizeOf(Single));

  FLock.Enter();

  try

    for i := 0 to FSlots.Count - 1 do
      begin

        slot := FSlots[i];
        if (slot = nil) or
           (not slot.Enabled) or
           (slot.Effect = nil) then
          Continue;

        if not Supports(slot.Effect,
                        IMfWasApiFxProvider,
                        prov) then
          Continue;

        mft := prov.GetMft;
        if (mft = nil) then
          Continue;

        hr := ConfigureMftFloat32(mft,
                                  SampleRate,
                                  Channels);
        if FAILED(hr) then
          Break;

        hr := ProcessMftInPlace(mft,
                                PByte(pData),
                                byteCount);
        if FAILED(hr) then
          Break;
      end;
  finally

    FLock.Leave();
  end;
end;

// Effectslot helpers ----------------------------------------------------------
function TMfWasApiEffectsRack.FindSlotByEffect(const AEffect: TMfWasApiFxComponentBase): TMfWasApiFxSlot;
var
  i: Integer;
  slot: TMfWasApiFxSlot;

begin

  Result := nil;

  if (AEffect = nil) or (Slots = nil) then
    Exit;

  for i := 0 to Slots.Count - 1 do
    begin

      slot := TMfWasApiFxSlot(Slots.Items[i]);
      if (slot <> nil) and (slot.Effect = AEffect) then
        Exit(slot);
    end;
end;


function TMfWasApiEffectsRack.FindSlotByEffectName(const AName: string): TMfWasApiFxSlot;
var
  i: Integer;
  slot: TMfWasApiFxSlot;
  eff: TMfWasApiFxComponentBase;

begin

  Result := nil;

  if (AName = '') or (Slots = nil) then
    Exit;

  for i := 0 to Slots.Count - 1 do
    begin

      slot := TMfWasApiFxSlot(Slots.Items[i]);

      if (slot <> nil) then
        begin

          eff := slot.Effect;
          if (eff <> nil) and SameText(eff.Name, AName) then
            Exit(slot);
        end;
    end;
end;


function TMfWasApiEffectsRack.FindFirstSlotByEffectClass(const AClass: TClass): TMfWasApiFxSlot;
var
  i: Integer;
  slot: TMfWasApiFxSlot;
  eff: TMfWasApiFxComponentBase;

begin

  Result := nil;

  if (AClass = nil) or (Slots = nil) then
    Exit;

  for i := 0 to Slots.Count - 1 do
    begin

      slot := TMfWasApiFxSlot(Slots.Items[i]);

      if (slot <> nil) then
        begin

          eff := slot.Effect;
          if (eff <> nil) and eff.InheritsFrom(AClass) then
            Exit(slot);
        end;
    end;
end;


end.
