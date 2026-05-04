// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - CoreAudio - WASAPI
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: MfMicrophoneDeckEngine.pas
// Kind: Pascal / Delphi unit
// Release date: 02-04-2023
// Language: ENU
//
// Revision Version: 3.1.9
// Description: Microphone audio handler incl effects unit.
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
// Remarks: Requires Windows 10 or later.
//
// Related objects: -
// Related projects: MfPackX319
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
unit MfMicrophoneDeckEngine;

interface

uses

  Winapi.Windows,
  WinApi.WinError,
  Winapi.ActiveX,

  System.Services.Avrt,
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  System.Math,

  Winapi.CoreAudioApi.AudioClient,
  Winapi.CoreAudioApi.MMDeviceApi,
  WinApi.CoreAudioApi.AudioSessionTypes,
  {WinMM}
  WinApi.WinMM.MMeApi,
  WinApi.WinMM.MMReg,
  {Application}
  PcmLib,
  MfMicrophoneFx,
  MfWasApiEffectsRack,
  MfParametricEqComponent;

type

  TMfMicrophoneDeckEngine = class;

  TMfMicrophoneCaptureThread = class(TThread)
  private

    FOwner: TMfMicrophoneDeckEngine;
  protected

    procedure Execute(); override;
  public

    constructor Create(const AOwner: TMfMicrophoneDeckEngine);
  end;

  TMfMicrophoneDeckEngine = class
  private const

    CDefaultBufferMs = 120;
    CMinRingFrames = 8192;
    CMaxConvertChannels = 8;

  private

    FLock: TCriticalSection;
    FStateEvent: TEvent;
    FStopEvent: THandle;
    FCaptureEvent: THandle;
    FCaptureThread: TMfMicrophoneCaptureThread;

    FDeviceId: string;
    FRunning: Boolean;
    FStarted: Boolean;
    FStopping: Boolean;

    FInputGainDb: Single;
    FInputGainLinear: Single;
    FVolumeL: Single;
    FVolumeR: Single;
    FMute: Boolean;

    FCompressor: TMfSimpleMicCompressor;
    FEcho: TMfSpringEcho;

    FNoiseGate: TMfSimpleNoiseGate;

    FWaveFormat: PWAVEFORMATEX;
    FSampleRate: Integer;
    FSourceChannels: Integer;
    FSourceBits: Integer;
    FSourceIsFloat: Boolean;
    FValidBitsPerSample: Integer;

    FRingBuffer: array of Single; // interleaved stereo
    FRingCapacityFrames: Integer;
    FRingReadPos: Integer;
    FRingWritePos: Integer;
    FRingBufferedFrames: Integer;

    FTempStereo: array of Single;

    FPeakL: Single;
    FPeakR: Single;
    FLastError: HRESULT;

    FAudioRack: TMfWasApiEffectsRack;
    FParamEq: TMfParametricEqEffect;

    procedure FreeWaveFormat();
    procedure ResetRuntimeState();
    procedure ResetRingBuffer(const AFrames: Integer);
    procedure ClearRingBuffer;
    function RingWriteFrames(const AData: PSingle;
                             const AFrames: Integer): Integer;
    function RingReadFrames(const AData: PSingle;
                            const AFrames: Integer): Integer;

    procedure UpdateInputGainLinear();
    procedure UpdatePeaksFromBuffer(ABuffer: PSingle;
                                    const AFrames: Integer);
    procedure ProcessCapturedBlock(ABuffer: PSingle;
                                   const AFrames: Integer);

    function ConvertCaptureToStereoFloat(const AData: Pointer;
                                         const AFrames: Integer;
                                         const ASilent: Boolean): Integer;

    procedure ThreadExecute();

    function ThreadOpenAudio(out AEnumerator: IMMDeviceEnumerator;
                             out ADevice: IMMDevice;
                             out AAudioClient: IAudioClient;
                             out ACaptureClient: IAudioCaptureClient;
                             out ABufferFrameCount: UINT32): HRESULT;

    procedure ThreadCloseAudio(var AEnumerator: IMMDeviceEnumerator;
                               var ADevice: IMMDevice;
                               var AAudioClient: IAudioClient;
                               var ACaptureClient: IAudioCaptureClient);

    function InternalStartThread(): HRESULT;
    procedure InternalStopThread();

    procedure FlushLiveState();

  public

    constructor Create();
    destructor Destroy(); override;

    function OpenDevice(const ADeviceId: string): HRESULT;
    procedure CloseDevice();

    function Start(): HRESULT;
    procedure Stop();

    function ReadOutputPcmFloat32(ABuffer: PSingle;
                                  const AFrames: Integer): HRESULT;

    procedure SetInputGainDb(const AValue: Single);
    procedure SetVolume(const ALeft,
                        ARight: Single);
    procedure SetMute(const AValue: Boolean);

    procedure SetCompressorEnabled(const AValue: Boolean);
    procedure SetCompressorParams(const AThresholdDb,
                                  ARatio,
                                  AAttackMs,
                                  AReleaseMs,
                                  AMakeupDb,
                                  ASoftKneeDb: Single);

    procedure SetEchoEnabled(const AValue: Boolean);
    procedure SetEchoParams(const AMix,
                            ADelayMs,
                            AFeedback,
                            ATone,
                            ASpring,
                            AWowDepthMs,
                            AWowRateHz: Single);

    function GetMeterPeakL(): Single;
    function GetMeterPeakR(): Single;
    function GetSampleRate(): Integer;
    function GetDeviceId(): string;
    function GetLastError(): HRESULT;
    function IsRunning(): Boolean;

    procedure SetNoiseGateEnabled(const AValue: Boolean);
    procedure SetNoiseGateParams(const AThresholdDb,
                                 AAttackMs,
                                 AReleaseMs,
                                 AFloorDb,
                                 AHoldMs: Single);

    property AudioRack: TMfWasApiEffectsRack read FAudioRack;
  end;

implementation

const
  CLSCTX_ALL_ = CLSCTX_INPROC_SERVER or CLSCTX_INPROC_HANDLER or
                CLSCTX_LOCAL_SERVER or CLSCTX_REMOTE_SERVER;

  REFTIMES_PER_SEC  = 10000000;
  REFTIMES_PER_MSEC = 10000;

{ TMfMicrophoneCaptureThread }

constructor TMfMicrophoneCaptureThread.Create(const AOwner: TMfMicrophoneDeckEngine);
begin

  inherited Create(False);

  FreeOnTerminate := False;
  FOwner := AOwner;
end;


procedure TMfMicrophoneCaptureThread.Execute();
begin

  if Assigned(FOwner) then
    FOwner.ThreadExecute;
end;

{ TMfMicrophoneDeckEngine }

constructor TMfMicrophoneDeckEngine.Create();
begin

  inherited Create;

  FLock := TCriticalSection.Create;
  FStateEvent := TEvent.Create(nil,
                               True,
                               False,
                               '');
  FStopEvent := 0;
  FCaptureEvent := 0;
  FCaptureThread := nil;

  FWaveFormat := nil;
  FSampleRate := 0;
  FSourceChannels := 2;
  FSourceBits := 32;
  FSourceIsFloat := True;
  FValidBitsPerSample := 32;

  FInputGainDb := 0.0;
  FInputGainLinear := 1.0;
  FVolumeL := 1.0;
  FVolumeR := 1.0;
  FMute := False;

  FCompressor := TMfSimpleMicCompressor.Create();
  FEcho := TMfSpringEcho.Create();
  FNoiseGate := TMfSimpleNoiseGate.Create();
  FParamEq := nil;

  FNoiseGate.Enabled := True;
  FNoiseGate.ThresholdDb := -45.0;
  FNoiseGate.AttackMs := 5.0;
  FNoiseGate.ReleaseMs := 120.0;
  FNoiseGate.FloorDb := -35.0;

  FPeakL := 0.0;
  FPeakR := 0.0;
  FLastError := S_OK;

  ResetRingBuffer(CMinRingFrames);
  ResetRuntimeState();

  FAudioRack := TMfWasApiEffectsRack.Create(nil);
  with TMfWasApiFxSlot(FAudioRack.Slots.Add) do
    begin

      FParamEq := TMfParametricEqEffect.Create(nil);
      Effect := FParamEq;
      Enabled := True;

      FParamEq.CenterFreqHz := 1500.0;
      FParamEq.Q := 1.0;
      FParamEq.GainDb := 0.0;
    end;
end;


destructor TMfMicrophoneDeckEngine.Destroy();
var
  Slot: TMfWasApiFxSlot;

begin

  InternalStopThread();

  FreeWaveFormat();

  if Assigned(FAudioRack) and
     (FAudioRack.Slots.Count > 0) then
    begin

      Slot := TMfWasApiFxSlot(FAudioRack.Slots[0]);

      if Assigned(Slot) and
         (Slot.Effect = FParamEq) then
        Slot.Effect := nil;
    end;

  FreeAndNil(FParamEq);
  FreeAndNil(FCompressor);
  FreeAndNil(FEcho);
  FreeAndNil(FNoiseGate);
  FreeAndNil(FAudioRack);

  FreeAndNil(FStateEvent);
  FreeAndNil(FLock);

  inherited Destroy;
end;


procedure TMfMicrophoneDeckEngine.FreeWaveFormat();
begin

  if Assigned(FWaveFormat) then
    begin

      CoTaskMemFree(FWaveFormat);
      FWaveFormat := nil;
    end;
end;


procedure TMfMicrophoneDeckEngine.ResetRuntimeState();
begin

  FRunning := False;
  FStarted := False;
  FStopping := False;
  FPeakL := 0.0;
  FPeakR := 0.0;
  FLastError := S_OK;
  ClearRingBuffer();
  FCompressor.Reset();
  FEcho.Reset();
end;


procedure TMfMicrophoneDeckEngine.ResetRingBuffer(const AFrames: Integer);
var
  Frames: Integer;

begin

  Frames := Max(AFrames,
                CMinRingFrames);

  FRingCapacityFrames := Frames;
  SetLength(FRingBuffer,
            FRingCapacityFrames * 2);

  FRingReadPos := 0;
  FRingWritePos := 0;
  FRingBufferedFrames := 0;

  if (Length(FRingBuffer) > 0) then
    FillChar(FRingBuffer[0],
             Length(FRingBuffer) * SizeOf(Single),
             0);
end;


procedure TMfMicrophoneDeckEngine.ClearRingBuffer;
begin

  FRingReadPos := 0;
  FRingWritePos := 0;
  FRingBufferedFrames := 0;

  if Length(FRingBuffer) > 0 then
    FillChar(FRingBuffer[0],
             Length(FRingBuffer) * SizeOf(Single),
             0);
end;


function TMfMicrophoneDeckEngine.RingWriteFrames(const AData: PSingle;
                                                 const AFrames: Integer): Integer;
var
  FramesToWrite: Integer;
  FramesToDrop: Integer;
  FirstPart: Integer;
  Src: PSingle;
  TargetBufferedFrames: Integer;

begin

  Result := 0;

  if (AData = nil) or
     (AFrames <= 0) or
     (FRingCapacityFrames <= 0) then
    Exit;

  FramesToWrite := AFrames;
  if (FramesToWrite > FRingCapacityFrames) then
    FramesToWrite := FRingCapacityFrames;

  if (FRingBufferedFrames + FramesToWrite > FRingCapacityFrames) then
    begin

      FramesToDrop := (FRingBufferedFrames + FramesToWrite) - FRingCapacityFrames;
      Inc(FRingReadPos, FramesToDrop);

      while (FRingReadPos >= FRingCapacityFrames) do
        Dec(FRingReadPos, FRingCapacityFrames);
      Dec(FRingBufferedFrames, FramesToDrop);

      if (FRingBufferedFrames < 0) then
        FRingBufferedFrames := 0;
    end;

  Src := AData;

  FirstPart := Min(FramesToWrite,
                   FRingCapacityFrames - FRingWritePos);
  Move(Src^,
       FRingBuffer[FRingWritePos * 2],
       FirstPart * 2 * SizeOf(Single));

  Inc(Src, FirstPart * 2);

  if (FramesToWrite > FirstPart) then
    Move(Src^,
         FRingBuffer[0],
         (FramesToWrite - FirstPart) * 2 * SizeOf(Single));

  Inc(FRingWritePos, FramesToWrite);
  while (FRingWritePos >= FRingCapacityFrames) do
    Dec(FRingWritePos, FRingCapacityFrames);

  Inc(FRingBufferedFrames, FramesToWrite);
  if (FRingBufferedFrames > FRingCapacityFrames) then
    FRingBufferedFrames := FRingCapacityFrames;

  // Keep the microphone source close to real-time.
  // If the capture side gets ahead, drop old buffered mic audio.
  TargetBufferedFrames := Max(256, FRingCapacityFrames div 4);

  if (FRingBufferedFrames > TargetBufferedFrames) then
    begin

      FramesToDrop := FRingBufferedFrames - TargetBufferedFrames;

      Inc(FRingReadPos, FramesToDrop);
      while (FRingReadPos >= FRingCapacityFrames) do
        Dec(FRingReadPos, FRingCapacityFrames);

      Dec(FRingBufferedFrames, FramesToDrop);
      if (FRingBufferedFrames < 0) then
        FRingBufferedFrames := 0;
    end;

  Result := FramesToWrite;
end;


function TMfMicrophoneDeckEngine.RingReadFrames(const AData: PSingle;
                                                const AFrames: Integer): Integer;
var
  FramesToRead: Integer;
  FirstPart: Integer;
  Dst: PSingle;

begin

  Result := 0;

  if (AData = nil) or
     (AFrames <= 0) or
     (FRingBufferedFrames <= 0) then
    Exit;

  FramesToRead := Min(AFrames, FRingBufferedFrames);
  Dst := AData;

  FirstPart := Min(FramesToRead, FRingCapacityFrames - FRingReadPos);
  Move(FRingBuffer[FRingReadPos * 2],
       Dst^,
       FirstPart * 2 * SizeOf(Single));

  Inc(Dst, FirstPart * 2);

  if (FramesToRead > FirstPart) then
    Move(FRingBuffer[0],
         Dst^,
         (FramesToRead - FirstPart) * 2 * SizeOf(Single));

  Inc(FRingReadPos, FramesToRead);
  while (FRingReadPos >= FRingCapacityFrames) do
    Dec(FRingReadPos, FRingCapacityFrames);

  Dec(FRingBufferedFrames, FramesToRead);
  if (FRingBufferedFrames < 0) then
    FRingBufferedFrames := 0;

  Result := FramesToRead;
end;


procedure TMfMicrophoneDeckEngine.UpdateInputGainLinear();
begin

  FInputGainDb := EnsureRange(FInputGainDb,
                              -18.0,
                              12.0);

  FInputGainLinear := TMfMicrophoneFxHelper.DbToLinear(FInputGainDb);
end;


procedure TMfMicrophoneDeckEngine.UpdatePeaksFromBuffer(ABuffer: PSingle;
                                                        const AFrames: Integer);
var
  I: Integer;
  L: Single;
  R: Single;
  PeakL: Single;
  PeakR: Single;

begin

  PeakL := 0.0;
  PeakR := 0.0;

  for I := 0 to AFrames - 1 do
    begin

      L := ABuffer^;
      Inc(ABuffer);

      R := ABuffer^;
      Inc(ABuffer);

      PeakL := Max(PeakL, Abs(L));
      PeakR := Max(PeakR, Abs(R));
    end;

  FPeakL := Max(PeakL, FPeakL * 0.92);
  FPeakR := Max(PeakR, FPeakR * 0.92);
end;


procedure TMfMicrophoneDeckEngine.ProcessCapturedBlock(ABuffer: PSingle;
                                                       const AFrames: Integer);
var
  I: Integer;
  P: PSingle;
  L: Single;
  R: Single;

  function LimitSample(const X: Single): Single; inline;
  begin

    if (X > 0.98) then
      Exit(0.98);

    if (X < -0.98) then
      Exit(-0.98);

    Result := X;
  end;

begin

  if (ABuffer = nil) or
     (AFrames <= 0) then
    Exit;

  P := ABuffer;

  // Input gain
  for I := 0 to (AFrames * 2) - 1 do
    begin
      P^ := P^ * FInputGainLinear;
      Inc(P);
    end;

  // Parametric EQ rack
  if Assigned(FAudioRack) then
    FAudioRack.ProcessFloat32(ABuffer,
                              AFrames,
                              2,
                              FSampleRate);

  // Noise gate NOTE: Must process before compressor!
  FNoiseGate.ProcessStereoInterleaved(ABuffer,
                                      AFrames);

  // Dynamics / echo
  FCompressor.ProcessStereoInterleaved(ABuffer,
                                       AFrames);
  FEcho.ProcessStereoInterleaved(ABuffer,
                                 AFrames);

  // Output volume / mute / final safety limit
  P := ABuffer;

  for I := 0 to AFrames - 1 do
    begin

      L := P^ * FVolumeL;
      Inc(P);

      R := P^ * FVolumeR;
      Inc(P);

      if FMute then
        begin
          L := 0.0;
          R := 0.0;
        end
      else
        begin
          L := LimitSample(L);
          R := LimitSample(R);
        end;

      Dec(P,
          2);
      P^ := L;
      Inc(P);
      P^ := R;
      Inc(P);
    end;

  UpdatePeaksFromBuffer(ABuffer,
                        AFrames);
end;


function TMfMicrophoneDeckEngine.ConvertCaptureToStereoFloat(const AData: Pointer;
                                                             const AFrames: Integer;
                                                             const ASilent: Boolean): Integer;
var
  I: Integer;
  Ch: Integer;
  SampleCount: Integer;
  SrcFloat: PSingle;
  Src16: PSmallInt;
  Src32: PInteger;
  Temp24: array of Single;
  BaseIndex: Integer;
  L: Single;
  R: Single;

begin

  Result := 0;

  if (AFrames <= 0) then
    Exit;

  SetLength(FTempStereo, AFrames * 2);

  if ASilent or (AData = nil) then
    begin

      FillChar(FTempStereo[0],
               Length(FTempStereo) * SizeOf(Single),
               0);
      Exit(AFrames);
    end;

  Ch := Max(1,
            FSourceChannels);
  if (Ch > CMaxConvertChannels) then
    Ch := CMaxConvertChannels;

  case FSourceBits of
    16: // bits
      begin
        Src16 := PSmallInt(AData);

        for I := 0 to AFrames - 1 do
          begin

            L := Src16^ / 32768.0;

            if (Ch = 1) then
              R := L
            else
              R := PSmallInt(NativeUInt(Src16) + SizeOf(SmallInt))^ / 32768.0;

            FTempStereo[(I * 2)] := L;
            FTempStereo[(I * 2) + 1] := R;

            Inc(Src16,
                Ch);
          end;
      end;

    24: // bits
      begin

        SampleCount := AFrames * Ch;
        SetLength(Temp24, SampleCount);

        Int24ToFloat(PByte(AData),
                     @Temp24[0],
                     SampleCount);

        for I := 0 to AFrames - 1 do
          begin
            BaseIndex := I * Ch;

            L := Temp24[BaseIndex];

            if (Ch = 1) then
              R := L
            else
              R := Temp24[BaseIndex + 1];

            FTempStereo[(I * 2)]     := L;
            FTempStereo[(I * 2) + 1] := R;
          end;
      end;

    32: // bits
      begin
        if FSourceIsFloat then
          begin
            SrcFloat := PSingle(AData);

            for I := 0 to AFrames - 1 do
              begin

                L := SrcFloat^;

                if (Ch = 1) then
                  R := L
                else
                  R := PSingle(NativeUInt(SrcFloat) + SizeOf(Single))^;

                FTempStereo[(I * 2)]     := L;
                FTempStereo[(I * 2) + 1] := R;

                Inc(SrcFloat, Ch);
              end;
          end
        else
          begin
            SampleCount := AFrames * Ch;
            SetLength(Temp24, SampleCount);

            Int32ToFloat(PByte(AData),
                         @Temp24[0],
                         SampleCount);

            for I := 0 to AFrames - 1 do
              begin
                BaseIndex := I * Ch;

                L := Temp24[BaseIndex];

                if (Ch = 1) then
                  R := L
                else
                  R := Temp24[BaseIndex + 1];

                FTempStereo[(I * 2)]     := L;
                FTempStereo[(I * 2) + 1] := R;
              end;
          end;
      end;
  else
    begin

      FillChar(FTempStereo[0],
               Length(FTempStereo) * SizeOf(Single),
                                            0);
    end;
  end;

  Result := AFrames;
end;


function TMfMicrophoneDeckEngine.OpenDevice(const ADeviceId: string): HRESULT;
begin

  if FRunning then
    InternalStopThread();

  FLock.Acquire;

  try

    FDeviceId := ADeviceId;
    ResetRuntimeState();
    Result := S_OK;
  finally

    FLock.Release;
  end;
end;


procedure TMfMicrophoneDeckEngine.CloseDevice();
begin

  InternalStopThread();

  FLock.Acquire;

  try

    FDeviceId := '';
    ResetRuntimeState;
  finally

    FLock.Release;
  end;
end;


function TMfMicrophoneDeckEngine.InternalStartThread(): HRESULT;
begin

  Result := S_OK;

  if Assigned(FCaptureThread) then
    Exit;

  FStateEvent.ResetEvent;

  if (FStopEvent = 0) then
    FStopEvent := CreateEvent(nil, True, False, nil);

  if (FCaptureEvent = 0) then
    FCaptureEvent := CreateEvent(nil, False, False, nil);

  if (FStopEvent = 0) or (FCaptureEvent = 0) then
    begin

      Result := HRESULT_FROM_WIN32(GetLastError);
      FLastError := Result;
      Exit;
    end;

  ResetEvent(FStopEvent);

  FStopping := False;
  FCaptureThread := TMfMicrophoneCaptureThread.Create(Self);
end;


procedure TMfMicrophoneDeckEngine.InternalStopThread();
begin

  FStopping := True;

  if (FStopEvent <> 0) then
    SetEvent(FStopEvent);

  if Assigned(FCaptureThread) then
    begin

      FCaptureThread.WaitFor;
      FreeAndNil(FCaptureThread);
    end;

  if (FCaptureEvent <> 0) then
    begin

      CloseHandle(FCaptureEvent);
      FCaptureEvent := 0;
    end;

  if (FStopEvent <> 0) then
    begin

      CloseHandle(FStopEvent);
      FStopEvent := 0;
    end;

  FLock.Acquire;

  try

    FRunning := False;
    FStarted := False;
    FStopping := False;
  finally

    FLock.Release;
  end;
end;


procedure TMfMicrophoneDeckEngine.FlushLiveState;
begin

  ClearRingBuffer();
  FNoiseGate.Reset();
  FCompressor.Reset();
  FEcho.Reset();

  FPeakL := 0.0;
  FPeakR := 0.0;
end;


function TMfMicrophoneDeckEngine.Start(): HRESULT;
begin

  FLock.Acquire;

  try

    Result := InternalStartThread();
  finally

    FLock.Release;
  end;
end;


procedure TMfMicrophoneDeckEngine.Stop();
begin

  InternalStopThread();

  FLock.Acquire;

  try

    FlushLiveState();
  finally

    FLock.Release;
  end;
end;


function TMfMicrophoneDeckEngine.ReadOutputPcmFloat32(ABuffer: PSingle;
                                                      const AFrames: Integer): HRESULT;
var
  ReadFrames: Integer;
  Remaining: Integer;
  // TEST:
  //Msg: string;

begin

  Result := S_OK;

  if (ABuffer = nil) or
     (AFrames <= 0) then
    Exit(E_INVALIDARG);

  // TEST: if we get the right wav format if it's like frames = 441 and samplerate is 48000, we got 44100: that's wrong.
  //Msg := Format('Mic MixerReadOutputPcmFloat32: Frames=%d, EngineSampleRate=%d',
  //              [AFrames,
  //               GetSampleRate]);
  //OutputDebugString(PChar(Msg));

  FLock.Acquire;

  try

  ReadFrames := RingReadFrames(ABuffer,
                               AFrames);

  Remaining := AFrames - ReadFrames;
  if (Remaining > 0) then
    FillChar(PByte(NativeUInt(ABuffer) +
                   NativeUInt(ReadFrames * 2 * SizeOf(Single)))^,
             Remaining * 2 * SizeOf(Single),
             0);
  finally

    FLock.Release;
  end;
end;


procedure TMfMicrophoneDeckEngine.SetInputGainDb(const AValue: Single);
begin

  FLock.Acquire;

  try

    FInputGainDb := AValue;
    UpdateInputGainLinear;
  finally

    FLock.Release;
  end;
end;


procedure TMfMicrophoneDeckEngine.SetVolume(const ALeft,
                                            ARight: Single);
begin

  FLock.Acquire;

  try

    FVolumeL := EnsureRange(ALeft,
                            0.0,
                            1.25);
    FVolumeR := EnsureRange(ARight,
                            0.0,
                            1.25);
  finally

    FLock.Release;
  end;
end;


procedure TMfMicrophoneDeckEngine.SetMute(const AValue: Boolean);
begin

  FLock.Acquire;

  try

    FMute := AValue;
  finally

    FLock.Release;
  end;
end;


procedure TMfMicrophoneDeckEngine.SetCompressorEnabled(const AValue: Boolean);
begin

  FLock.Acquire;

  try

    if (FCompressor.Enabled <> AValue) then
      begin

        FCompressor.Enabled := AValue;
        FlushLiveState();
      end
    else
      FCompressor.Enabled := AValue;
  finally

    FLock.Release;
  end;
end;


procedure TMfMicrophoneDeckEngine.SetCompressorParams(const AThresholdDb,
                                                      ARatio,
                                                      AAttackMs,
                                                      AReleaseMs,
                                                      AMakeupDb,
                                                      ASoftKneeDb: Single);
begin

  FLock.Acquire;

  try

    FCompressor.ThresholdDb := AThresholdDb;
    FCompressor.Ratio := ARatio;
    FCompressor.AttackMs := AAttackMs;
    FCompressor.ReleaseMs := AReleaseMs;
    FCompressor.MakeupDb := AMakeupDb;
    FCompressor.SoftKneeDb := ASoftKneeDb;
    FCompressor.Reset();
  finally

    FLock.Release;
  end;
end;


procedure TMfMicrophoneDeckEngine.SetEchoEnabled(const AValue: Boolean);
begin

  FLock.Acquire;

  try

    if (FEcho.Enabled <> AValue) then
      begin

        FEcho.Enabled := AValue;
        FlushLiveState();
      end
    else
      FEcho.Enabled := AValue;
  finally

    FLock.Release;
  end;
end;


procedure TMfMicrophoneDeckEngine.SetEchoParams(const AMix,
                                                ADelayMs,
                                                AFeedback,
                                                ATone,
                                                ASpring,
                                                AWowDepthMs,
                                                AWowRateHz: Single);
begin

  FLock.Acquire;

  try

    FEcho.Mix := AMix;
    FEcho.DelayMs := ADelayMs;
    FEcho.Feedback := AFeedback;
    FEcho.Tone := ATone;
    FEcho.Spring := ASpring;
    FEcho.WowDepthMs := AWowDepthMs;
    FEcho.WowRateHz := AWowRateHz;
  finally

    FLock.Release;
  end;
end;


function TMfMicrophoneDeckEngine.GetMeterPeakL: Single;
begin

  FLock.Acquire;

  try

    Result := FPeakL;
  finally

    FLock.Release;
  end;
end;


function TMfMicrophoneDeckEngine.GetMeterPeakR: Single;
begin

  FLock.Acquire;

  try

    Result := FPeakR;
  finally

    FLock.Release;
  end;
end;


function TMfMicrophoneDeckEngine.GetSampleRate: Integer;
begin

  FLock.Acquire;

  try

    Result := FSampleRate;
  finally

    FLock.Release;
  end;
end;


function TMfMicrophoneDeckEngine.GetDeviceId: string;
begin

  FLock.Acquire;

  try

    Result := FDeviceId;
  finally

    FLock.Release;
  end;
end;


function TMfMicrophoneDeckEngine.GetLastError: HRESULT;
begin

  FLock.Acquire;

  try

    Result := FLastError;
  finally

    FLock.Release;
  end;
end;


function TMfMicrophoneDeckEngine.IsRunning(): Boolean;
begin

  FLock.Acquire;

  try

    Result := FRunning;
  finally

    FLock.Release;
  end;
end;

// Noise gate
procedure TMfMicrophoneDeckEngine.SetNoiseGateEnabled(const AValue: Boolean);
begin

  FLock.Acquire;

  try

    if Assigned(FNoiseGate) then
      begin

        FNoiseGate.Enabled := AValue;
        FNoiseGate.Reset();
      end;
  finally

    FLock.Release;
  end;
end;


procedure TMfMicrophoneDeckEngine.SetNoiseGateParams(const AThresholdDb,
                                                     AAttackMs,
                                                     AReleaseMs,
                                                     AFloorDb,
                                                     AHoldMs: Single);
begin

  FLock.Acquire;
  try

    if Assigned(FNoiseGate) then
      begin

        FNoiseGate.ThresholdDb := AThresholdDb;
        FNoiseGate.AttackMs := AAttackMs;
        FNoiseGate.ReleaseMs := AReleaseMs;
        FNoiseGate.FloorDb := AFloorDb;
        FNoiseGate.HoldMs := AHoldMs;
        FNoiseGate.Reset();
      end;
  finally

    FLock.Release;
  end;
end;


function TMfMicrophoneDeckEngine.ThreadOpenAudio(out AEnumerator: IMMDeviceEnumerator;
                                                 out ADevice: IMMDevice;
                                                 out AAudioClient: IAudioClient;
                                                 out ACaptureClient: IAudioCaptureClient;
                                                 out ABufferFrameCount: UINT32): HRESULT;
var
  HnsBufferDuration: Int64;
  Hr: HRESULT;
  TargetWfx: TWAVEFORMATEX;

begin

  AEnumerator := nil;
  ADevice := nil;
  AAudioClient := nil;
  ACaptureClient := nil;
  ABufferFrameCount := 0;

  Hr := CoCreateInstance(CLSID_MMDeviceEnumerator,
                         nil,
                         CLSCTX_ALL_,
                         IMMDeviceEnumerator,
                         AEnumerator);
  if Failed(Hr) then
    Exit(Hr);

  if FDeviceId <> '' then
    Hr := AEnumerator.GetDevice(PWideChar(WideString(FDeviceId)),
                                ADevice)
  else
    Hr := AEnumerator.GetDefaultAudioEndpoint(eCapture,
                                              eConsole,
                                              ADevice);

  if Failed(Hr) then
    Exit(Hr);

  Hr := ADevice.Activate(IAudioClient,
                         CLSCTX_ALL_,
                         nil,
                         Pointer(AAudioClient));
  if Failed(Hr) then
    Exit(Hr);

  FreeWaveFormat;
  Hr := AAudioClient.GetMixFormat(FWaveFormat);
  if Failed(Hr) then
    Exit(Hr);

  // Debug: actual microphone device mix format.
  FSampleRate := FWaveFormat^.nSamplesPerSec;
  FSourceChannels := FWaveFormat^.nChannels;
  FValidBitsPerSample := FWaveFormat^.wBitsPerSample;
  GetWfxBitsAndFloat(FWaveFormat, FSourceBits, FSourceIsFloat);
  if (FSourceBits <= 0) then
    FSourceBits := FWaveFormat^.wBitsPerSample;

  {OutputDebugString(PChar(Format('Mic GetMixFormat: %d Hz, %d ch, %d bits, float=%s',
                                 [FSampleRate,
                                  FSourceChannels,
                                  FSourceBits,
                                  BoolToStr(FSourceIsFloat, True)])));
  }

  // Force capture into Carmen's current engine format.
  // Current mixer/master path is effectively 44100 Hz stereo float.
  FillChar(TargetWfx, SizeOf(TargetWfx), 0);
  TargetWfx.wFormatTag := WAVE_FORMAT_IEEE_FLOAT;
  TargetWfx.nChannels := 2;
  TargetWfx.nSamplesPerSec := 44100;
  TargetWfx.wBitsPerSample := 32;
  TargetWfx.nBlockAlign := (TargetWfx.nChannels * TargetWfx.wBitsPerSample) div 8;
  TargetWfx.nAvgBytesPerSec := TargetWfx.nSamplesPerSec * TargetWfx.nBlockAlign;
  TargetWfx.cbSize := 0;

  HnsBufferDuration := CDefaultBufferMs * REFTIMES_PER_MSEC;

  Hr := AAudioClient.Initialize(AUDCLNT_SHAREMODE_SHARED,
                                AUDCLNT_STREAMFLAGS_EVENTCALLBACK or
                                AUDCLNT_STREAMFLAGS_AUTOCONVERTPCM or
                                AUDCLNT_STREAMFLAGS_SRC_DEFAULT_QUALITY or
                                AUDCLNT_STREAMFLAGS_NOPERSIST,
                                HnsBufferDuration,
                                0,
                                @TargetWfx,
                                nil);
  if Failed(Hr) then
    Exit(Hr);

  Hr := AAudioClient.SetEventHandle(FCaptureEvent);
  if Failed(Hr) then
    Exit(Hr);

  Hr := AAudioClient.GetBufferSize(ABufferFrameCount);
  if Failed(Hr) then
    Exit(Hr);

  Hr := AAudioClient.GetService(IAudioCaptureClient, ACaptureClient);
  if Failed(Hr) then
    Exit(Hr);

  // From here on, the engine should treat the capture stream as the requested format.
  FSampleRate := TargetWfx.nSamplesPerSec;
  FSourceChannels := TargetWfx.nChannels;
  FSourceBits := TargetWfx.wBitsPerSample;
  FSourceIsFloat := True;
  FValidBitsPerSample := TargetWfx.wBitsPerSample;

  FCompressor.SetSampleRate(FSampleRate);
  FEcho.SetSampleRate(FSampleRate);

  {OutputDebugString(PChar(Format('Mic Capture Initialized: %d Hz, %d ch, %d bits, float=%s',
                                 [FSampleRate,
                                  FSourceChannels,
                                  FSourceBits,
                                  BoolToStr(FSourceIsFloat, True)])));
  }
  Result := S_OK;
end;


procedure TMfMicrophoneDeckEngine.ThreadCloseAudio(var AEnumerator: IMMDeviceEnumerator;
                                                   var ADevice: IMMDevice;
                                                   var AAudioClient: IAudioClient;
                                                   var ACaptureClient: IAudioCaptureClient);
begin
  ACaptureClient := nil;
  AAudioClient := nil;
  ADevice := nil;
  AEnumerator := nil;
  FreeWaveFormat;
end;

procedure TMfMicrophoneDeckEngine.ThreadExecute;
var
  Hr: HRESULT;
  Enumerator: IMMDeviceEnumerator;
  Device: IMMDevice;
  AudioClient: IAudioClient;
  CaptureClient: IAudioCaptureClient;
  BufferFrameCount: UINT32;
  PacketLength: UINT32;
  NumFramesAvailable: UINT32;
  Flags: DWORD;
  DataPtr: PByte;
  DevicePosition: UINT64;
  QpcPosition: UINT64;
  WaitRes: DWORD;
  WaitHandles: array[0..1] of THandle;
  TaskIndex: DWORD;
  MmcssHandle: THandle;

begin

  Enumerator := nil;
  Device := nil;
  AudioClient := nil;
  CaptureClient := nil;
  BufferFrameCount := 0;
  PacketLength := 0;
  NumFramesAvailable := 0;
  Flags := 0;
  DataPtr := nil;
  DevicePosition := 0;
  QpcPosition := 0;
  TaskIndex := 0;
  MmcssHandle := 0;

  Hr := CoInitializeEx(nil,
                       COINIT_MULTITHREADED);

  if Failed(Hr) and (Hr <> RPC_E_CHANGED_MODE) then
    begin

      FLock.Acquire;

      try

        FLastError := Hr;
        FRunning := False;
      finally

        FLock.Release;
      end;
      Exit;
    end;

  try

    MmcssHandle := AvSetMmThreadCharacteristics('Audio',
                                                @TaskIndex);

    Hr := ThreadOpenAudio(Enumerator,
                          Device,
                          AudioClient,
                          CaptureClient,
                          BufferFrameCount);
    if Failed(Hr) then
      begin

        FLock.Acquire;

        try

          FLastError := Hr;
          FRunning := False;
        finally

          FLock.Release;
        end;
        Exit;
      end;

    FLock.Acquire;

    try

      //ResetRingBuffer(Max(Integer(BufferFrameCount) * 16,
      //                    CMinRingFrames));
      ResetRingBuffer(Max(Integer(BufferFrameCount) * 4,
                          2048));
      FPeakL := 0.0;
      FPeakR := 0.0;

      FCompressor.Reset;
      FEcho.Reset;
      FRunning := True;
      FStarted := True;
      FLastError := S_OK;
    finally

      FLock.Release;
    end;

    Hr := AudioClient.Start();
    if Failed(Hr) then
      begin

        FLock.Acquire;

        try

          FLastError := Hr;
          FRunning := False;
        finally

          FLock.Release;
        end;
        Exit;
      end;

    WaitHandles[0] := FStopEvent;
    WaitHandles[1] := FCaptureEvent;

    while not FStopping do
      begin

        WaitRes := WaitForMultipleObjects(2,
                                          @WaitHandles[0],
                                          False,
                                          INFINITE);

         // Extra security when stopping, can't hurt either.
         if FStopping then
           Break;

        if (WaitRes = WAIT_OBJECT_0) then
          Break;

        if (WaitRes <> (WAIT_OBJECT_0 + 1)) then
          Continue;

        Hr := CaptureClient.GetNextPacketSize(PacketLength);
        if Failed(Hr) then
          begin

            FLock.Acquire;

            try

              FLastError := Hr;
            finally

              FLock.Release;
            end;
            Break;
          end;

        while (PacketLength > 0) do
          begin

            DataPtr := nil;
            NumFramesAvailable := 0;
            Flags := 0;
            DevicePosition := 0;
            QpcPosition := 0;

            Hr := CaptureClient.GetBuffer(DataPtr,
                                          NumFramesAvailable,
                                          Flags,
                                          @DevicePosition,
                                          @QpcPosition);
            if Failed(Hr) then
              begin

                FLock.Acquire;

                try

                  FLastError := Hr;
                finally

                  FLock.Release;
                end;
                Break;
              end;

            try

              if (NumFramesAvailable > 0) then
                begin

                  ConvertCaptureToStereoFloat(DataPtr,
                                              Integer(NumFramesAvailable),
                                              (Flags and AUDCLNT_BUFFERFLAGS_SILENT) <> 0);
                  // Debug
                  //if (Length(FTempStereo) >= 8) then
                  //  OutputDebugString(PChar(Format('Mic FirstSamples: %.6f %.6f %.6f %.6f %.6f %.6f %.6f %.6f',
                  //               [FTempStereo[0],
                  //                FTempStereo[1],
                  //                FTempStereo[2],
                  //                FTempStereo[3],
                  //                FTempStereo[4],
                  //                FTempStereo[5],
                  //                FTempStereo[6],
                  //                FTempStereo[7]])));

                  FLock.Acquire;

                  try

                    ProcessCapturedBlock(@FTempStereo[0], Integer(NumFramesAvailable));
                    RingWriteFrames(@FTempStereo[0], Integer(NumFramesAvailable));
                  finally

                    FLock.Release;
                  end;
                end;
            finally

              CaptureClient.ReleaseBuffer(NumFramesAvailable);
            end;

            Hr := CaptureClient.GetNextPacketSize(PacketLength);
            if Failed(Hr) then
              begin

                FLock.Acquire;

                try

                  FLastError := Hr;
                finally

                  FLock.Release;
                end;
                Break;
              end;
          end;
      end;

    AudioClient.Stop();
    AudioClient.Reset();

  finally

    ThreadCloseAudio(Enumerator,
                     Device,
                     AudioClient,
                     CaptureClient);

    if (MmcssHandle <> 0) then
      AvRevertMmThreadCharacteristics(MmcssHandle);

    FLock.Acquire;

    try

      FRunning := False;
      FStarted := False;
    finally

      FLock.Release;
    end;

    CoUninitialize();
  end;
end;

end.
