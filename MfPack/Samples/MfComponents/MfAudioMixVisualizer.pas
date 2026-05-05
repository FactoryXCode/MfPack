// FactoryX
//
// Copyright:  FactoryX. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: MfAudioMixVisualizer.pas
// Kind: Pascal Unit Component
// Release date: 24-01-2019
// Language: ENU
//
// Version: 3.2.0
// Description: Single visual component: WASAPI loopback + Peak/RMS + optional Spectrum bars.
//
// Company: FactoryX
// Intiator(s): Tony (maXcomX), Carmen (carmenh).
// Contributor(s): Tony Kalf (maXcomX), Carmen (carmenh).
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 05/05/2026 All                 Bauhaus release  SDK 10.0.26100.4654 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: To install the visual components, choose Install in the Project Manager.
//          Requires Windows 10 or later.
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
// =============================================================================
// Source: FactoryX.code
//
// Copyright (c) FactoryX. All rights reserved.
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
unit MfAudioMixVisualizer;

interface

uses

  {WinApi}
  WinApi.Windows,
  WinApi.WinApiTypes,
  WinApi.ComBaseApi,
  {ActiveX}
  WinApi.ActiveX.PropIdl,
  WinApi.ActiveX.PropSys,
  WinApi.ActiveX.ObjBase,
  {System}
  System.SysUtils,
  System.Classes,
  System.Math,
  {CoreAudioApi}
  WinApi.CoreAudioApi.MMDeviceApi,
  Winapi.CoreAudioApi.AudioClient,
  WinApi.CoreAudioApi.AudioSessionTypes,
  WinApi.CoreAudioApi.EndpointVolume,
  {WinMM}
  WinApi.WinMM.MMReg,
  WinApi.WinMM.MMeApi,
  {VCL}
  Vcl.Controls,
  Vcl.Graphics,
  Vcl.ExtCtrls;

type

  TMfAnalyzerMode = (amLevelsOnly,
                     amLevelsAndSpectrum);

  TMfVizMode      = (vmMeters,
                     vmSpectrum);

  TMfAudioInputSource = (isLoopback,
                         isExternalFeed);


  TMfVolumeScaleMode = (vsmLinear,
                        vsmPerceptual,
                        vsmDbPerceptual);

  TMfClipIndicatorMode = (cimPerChannel,
                          cimSingle);


  TMfLevels = record
    PeakL: Single;   // 0..1
    PeakR: Single;   // 0..1
    RmsL: Single;    // 0..1
    RmsR: Single;    // 0..1
    PeakDbL: Single; // dBFS
    PeakDbR: Single;
    RmsDbL: Single;
    RmsDbR: Single;
  end;

  TSimpleFFT = record

    class procedure HannWindow(var X: TArray<Single>); static;
    class procedure FFT(var Re, Im: TArray<Single>); static;
  end;

  // Forwarded class.
  TMfAudioMixVisualizer = class;

  // Device notification COM interface (default-device switching etc.)
  TMfDeviceNotificationClient = class(TInterfacedObject, IMMNotificationClient)
  private

    FOwner: TMfAudioMixVisualizer;

  public

    constructor Create(AOwner: TMfAudioMixVisualizer);

    function OnDeviceStateChanged(pwstrDeviceId: LPCWSTR;
                                  dwNewState: DWord): HResult; stdcall;

    function OnDeviceAdded(pwstrDeviceId: LPCWSTR): HResult; stdcall;

    function OnDeviceRemoved(pwstrDeviceId: LPCWSTR): HResult; stdcall;

    function OnDefaultDeviceChanged(flow: EDataFlow;
                                    role: ERole;
                                    pwstrDefaultDeviceId: PWideChar): HResult; stdcall;

    function OnPropertyValueChanged(pwstrDeviceId: LPCWSTR;
                                    const key: PROPERTYKEY): HResult; stdcall;
  end;

  TMfCaptureThread = class(TThread)
  private

    FOwner: TMfAudioMixVisualizer;
  protected

    procedure Execute(); override;
  public

    constructor Create(AOwner: TMfAudioMixVisualizer);
  end;

  TMfSpectrum = TArray<Single>; // 0..1 per bar

  TMfAudioMixVisualizer = class(TGraphicControl)
  private

    // Published settings.
    FActive: Boolean;
    FAutoStart: Boolean;

    FInputSource: TMfAudioInputSource;

    FDataFlow: EDataFlow;
    FRole: ERole;

    // Visual scaling based on endpoint master volume slider.
    FVolumeScaleMode: TMfVolumeScaleMode;
    FEndpointVol: IAudioEndpointVolume;
    FVolScalarBits: Integer; // atomic Single bits (0..1)
    FEnableVolumeSmoothing: Boolean;
    FVolumeSmoothingMs: Integer;

    FMode: TMfAnalyzerMode;
    FView: TMfVizMode;

    FBarCount: Integer;
    FFftSize: Integer;

    FAttackMs: Integer;
    FReleaseMs: Integer;
    FDispatchEveryMs: Integer;

    FFps: Integer;

    FShowRms: Boolean;
    FShowPeakHold: Boolean;
    FPeakHoldMs: Integer;

    // Bar colors + peak cap.
    FBarColor: TColor;
    FPeakColor: TColor;
    FRmsColor: TColor;
    FBorderColor: TColor;

    // Optional dBFS scale overlay (left gutter).
    FShowDbScale: Boolean;
    FDbTop: Single; // positive/ceiling (e.g. +6)
    FDbMin: Single; // negative (e.g. -60)
    FDbTickStep: Integer;
    FDbLabelStep: Integer;
    FDbScaleWidth: Integer;

    // Peak cap settings.
    FPeakThreshold: Single; // 0..1 (e.g. 0.98)
    FPeakCapFrac: Single;   // 0..1 fraction of bar height (e.g. 0.12)

    // Clip indicator (latched).
    FShowClipIndicator: Boolean;
    FClipIndicatorMode: TMfClipIndicatorMode;
    FClipThreshold: Single; // 0..1 (e.g. 0.999)
    FClipHoldMs: Integer;   // e.g. 800
    // Latches stored as tick deadlines (GetTickCount + hold ms).
    FClipUntilL: Integer;   // atomic
    FClipUntilR: Integer;   // atomic
    FClipUntilAny: Integer; // atomic (either channel)
    FLastDataTick: Integer; // atomic: GetTickCount of last published data

    // Runtime-only UI helpers.
    FTimer: TTimer;
    FBack: TBitmap;
    FBackColor: TColor;

    // Atomic snapshots.
    FLevelsBits: array[0..7] of Integer;
    FSpectrumBits: TArray<Integer>;

    // Threading (runtime only).
    FCaptureThread: TThread;
    FStopEvent: THandle;

    // Format published by capture thread.
    FSampleRate: Integer;
    FChannels: Integer;
    FIsFloat: Boolean;

    // External feed smoothing state.
    FSmPeakL: Single;
    FSmPeakR: Single;
    FSmRmsL: Single;
    FSmRmsR: Single;
    FExtAttackA: Single;
    FExtReleaseA: Single;
    FExtLastDispatchTick: DWORD;

    // Spectrum ring buffer (mono).
    FMonoRing: TArray<Single>;
    FRingWrite: Integer;
    FRingCount: Integer;

    // Peak hold.
    FHoldL,
    FHoldR: Single;
    FHoldTickL,
    FHoldTickR: DWORD;

    // The endpoint deviceId the visualizer will be wired to.
    FDeviceId: string;

    // Device change restart guard (default render device switches).
    FRestartQueued: Integer;

    // Show header (meter info text)
    FShowMeters: Boolean;
    FInputTrimDb: Single;

    procedure QueueRestart();
    procedure DoQueuedRestart();

    procedure TimerTick(Sender: TObject);

    procedure SetActive(Value: Boolean);
    procedure SetAutoStart(Value: Boolean);
    procedure SetInputSource(Value: TMfAudioInputSource);

    procedure SetDeviceDataFlow(Value: EDataFlow);
    procedure SetDeviceRole(Value: ERole);


    procedure SetVolumeScaleMode(Value: TMfVolumeScaleMode);
    procedure SetMode(Value: TMfAnalyzerMode);
    procedure SetView(Value: TMfVizMode);

    procedure SetBarCount(Value: Integer);
    procedure SetFftSize(Value: Integer);

    procedure SetAttackMs(Value: Integer);
    procedure SetReleaseMs(Value: Integer);

    procedure SetDispatchEveryMs(Value: Integer);
    procedure SetFps(Value: Integer);

    procedure SetBackColor(Value: TColor);

    procedure SetBarColor(Value: TColor);
    procedure SetPeakColor(Value: TColor);
    procedure SetRmsColor(Value: TColor);
    procedure SetBorderColor(Value: TColor);
    procedure SetShowDbScale(Value: Boolean);
    procedure SetShowMeters(Value: Boolean);
    procedure SetDbTop(Value: Single);
    procedure SetDbMin(Value: Single);
    procedure SetDbTickStep(Value: Integer);
    procedure SetDbLabelStep(Value: Integer);
    procedure SetDbScaleWidth(Value: Integer);
    procedure SetPeakThreshold(Value: Single);
    procedure SetPeakCapFrac(Value: Single);
    procedure SetInputTrimDb(Value: Single);

    procedure ClearVisualState();
    procedure EnsureSpectrumStorage();
    procedure StartCapture();
    procedure StopCapture();
    procedure RestartIfRunning();

    function GetLevels(): TMfLevels;
    function GetSpectrum(): TMfSpectrum;


    function GetVolumeVisualGain(): Single;
    function DbToDisplayFrac(const ADb,
                             ADbMin,
                             ADbTop: Single): Single;
    procedure BuildSpectrumBars(const MonoBlock: TArray<Single>;
                                out Bars: TArray<Single>);
    procedure DrawMeters(ACanvas: TCanvas; const R: TRect);
    procedure DrawSpectrum(ACanvas: TCanvas; const R: TRect);
    procedure DrawDesignTimePlaceholder(ACanvas: TCanvas; const R: TRect);

    // Set DeviceID
    procedure SetDeviceId(const Value: string);

  protected

    procedure Loaded(); override;
    procedure Paint(); override;

  public

    // External feed mode (InputSource = isExternalFeed).
    procedure BeginExternalFormat(ASampleRate, AChannels: Integer; AIsFloat: Boolean);
    procedure PushFloat32Interleaved(pData: PSingle; Frames: Integer);
    procedure PushInt16Interleaved(pData: PSmallInt; Frames: Integer);

    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;

    // Force visuals to zero (bars + peak hold + clip latches).
    procedure Reset();

    property Levels: TMfLevels read GetLevels;
    property Spectrum: TMfSpectrum read GetSpectrum;

    property SampleRate: Integer read FSampleRate;
    property Channels: Integer read FChannels;

  published

    property Align;
    property Anchors;

    property BackColor: TColor read FBackColor write SetBackColor default $00001A00;

    // Colors
    property BarColor: TColor read FBarColor write SetBarColor default $00F1BC8B;
    property PeakColor: TColor read FPeakColor write SetPeakColor default clRed;
    property RmsColor: TColor read FRmsColor write SetRmsColor default clYellow;
    property BorderColor: TColor read FBorderColor write SetBorderColor default clGray;

    // Peak highlight
    property PeakThreshold: Single read FPeakThreshold write SetPeakThreshold;
    property PeakCapFrac: Single read FPeakCapFrac write SetPeakCapFrac;

    // dBFS scale overlay
    property ShowDbScale: Boolean read FShowDbScale write SetShowDbScale default True;
    property ShowMeterInfo: Boolean read FShowMeters write SetShowMeters default True;
    property DbTop: Single read FDbTop write SetDbTop;
    property DbMin: Single read FDbMin write SetDbMin;
    property DbTickStep: Integer read FDbTickStep write SetDbTickStep default 6;
    property DbLabelStep: Integer read FDbLabelStep write SetDbLabelStep default 12;
    property DbScaleWidth: Integer read FDbScaleWidth write SetDbScaleWidth default 44;

    property AutoStart: Boolean read FAutoStart write SetAutoStart default False;
    property Active: Boolean read FActive write SetActive default False;

    property InputSource: TMfAudioInputSource read FInputSource write SetInputSource default isLoopback;

    property DeviceDataFlow: EDataFlow read FDataFlow write SetDeviceDataFlow default eRender;
    property DeviceRole: ERole read FRole write SetDeviceRole default eMultimedia;
    property DeviceId: string read FDeviceId write SetDeviceId;
    property InputTrimDb: Single read FInputTrimDb write SetInputTrimDb;

    property VolumeScaleMode: TMfVolumeScaleMode read FVolumeScaleMode write SetVolumeScaleMode default vsmPerceptual;
    property EnableVolumeSmoothing: Boolean read FEnableVolumeSmoothing write FEnableVolumeSmoothing default True;
    property VolumeSmoothingMs: Integer read FVolumeSmoothingMs write FVolumeSmoothingMs default 80;
    property Mode: TMfAnalyzerMode read FMode write SetMode default amLevelsAndSpectrum;
    property View: TMfVizMode read FView write SetView default vmSpectrum;

    property BarCount: Integer read FBarCount write SetBarCount default 48;
    property FftSize: Integer read FFftSize write SetFftSize default 2048;

    property AttackMs: Integer read FAttackMs write SetAttackMs default 15;
    property ReleaseMs: Integer read FReleaseMs write SetReleaseMs default 120;

    property DispatchEveryMs: Integer read FDispatchEveryMs write SetDispatchEveryMs default 33;
    property FPS: Integer read FFps write SetFps default 60;

    property ShowRms: Boolean read FShowRms write FShowRms default True;
    property ShowPeakHold: Boolean read FShowPeakHold write FShowPeakHold default True;
    property PeakHoldMs: Integer read FPeakHoldMs write FPeakHoldMs default 450;

    property OnClick;
    property OnDblClick;
  end;


procedure Register;


implementation

const
  EPS_AMP = 1.0e-12; // prevents Log10(0)



procedure Register;
begin

  RegisterComponents('MfPack Core Audio Samples',
                     [TMfAudioMixVisualizer]);
end;


function RectWidth(const R: TRect): Integer; inline;
begin

  Result := R.Right - R.Left;
end;


function RectHeight(const R: TRect): Integer; inline;
begin

  Result := R.Bottom - R.Top;
end;


function SingleToBits(const S: Single): Integer; inline;
begin

  Result := PInteger(@S)^;
end;


function BitsToSingle(const B: Integer): Single; inline;
begin

  Result := PSingle(@B)^;
end;


function DbToGain(const ADb: Single): Single; inline;
begin

  Result := Power(10.0,
                  ADb / 20.0);
end;


function ClampVizSample(const S: Single): Single; inline;
begin

  Result := EnsureRange(S,
                        -4.0,
                        4.0);
end;


// SimpleFFT

class procedure TSimpleFFT.HannWindow(var X: TArray<Single>);
var
  i, N: Integer;
  w: Double;

begin

  N := Length(X);
  if (N <= 1) then
    Exit;

  for i := 0 to N - 1 do
    begin

      w := 0.5 * (1.0 - Cos(2.0 * Pi * i / (N - 1)));
      X[i] := X[i] * (w * 1.0);
    end;
end;


class procedure TSimpleFFT.FFT(var Re, Im: TArray<Single>);

  procedure Swap(var A,
                 B: Single); inline;
  var
    T: Single;

  begin

    T := A;
    A := B;
    B := T;
  end;

var
  N,
  i,
  j,
  m,
  m2,
  step: Integer;
  wr,
  wi,
  ur,
  ui,
  tr,
  ti: Double;
  angle: Double;

begin

  N := Length(Re);
  if (N <= 1) or (Length(Im) <> N) then
    Exit;

  j := 0;
  for i := 1 to N - 2 do
    begin

      step := N shr 1;
      while (j >= step) do
        begin

          j := j - step;
          step := step shr 1;
        end;

    j := j + step;
    if (i < j) then
      begin

        Swap(Re[i],
             Re[j]);
        Swap(Im[i],
             Im[j]);
      end;
  end;

  m := 2;
  while (m <= N) do
    begin

      m2 := m shr 1;
      angle := -2.0 * Pi / m;

      for j := 0 to m2 - 1 do
        begin

          wr := Cos(angle * j);
          wi := Sin(angle * j);

          i := j;
          while (i < N) do
            begin

              tr := wr * Re[i + m2] - wi * Im[i + m2];
              ti := wr * Im[i + m2] + wi * Re[i + m2];

              ur := Re[i];
              ui := Im[i];

              Re[i] := (ur + tr)  * 1.0;
              Im[i] := (ui + ti) * 1.0;
              Re[i + m2] := (ur - tr) * 1.0;
              Im[i + m2] := (ui - ti) * 1.0;

              i := i + m;
            end;
        end;

      m := m shl 1;
    end;
end;


constructor TMfCaptureThread.Create(AOwner: TMfAudioMixVisualizer);
begin

  inherited Create(False);

  FreeOnTerminate := False;
  FOwner := AOwner;
end;



{ TMfDeviceNotificationClient }

constructor TMfDeviceNotificationClient.Create(AOwner: TMfAudioMixVisualizer);
begin

  inherited Create;
  FOwner := AOwner;
end;


function TMfDeviceNotificationClient.OnDeviceStateChanged(pwstrDeviceId: LPCWSTR;
                                                          dwNewState: DWord): HResult; stdcall;
begin

  Result := S_OK;
end;


function TMfDeviceNotificationClient.OnDeviceAdded(pwstrDeviceId: LPCWSTR): HResult; stdcall;
begin

  Result := S_OK;
end;


function TMfDeviceNotificationClient.OnDeviceRemoved(pwstrDeviceId: LPCWSTR): HResult; stdcall;
begin

  Result := S_OK;
end;


function TMfDeviceNotificationClient.OnDefaultDeviceChanged(flow: EDataFlow;
                                                            role: ERole;
                                                            pwstrDefaultDeviceId: PWideChar): HResult; stdcall;
begin

  Result := S_OK;

  if (FOwner = nil) then
    Exit;

  // Explicit device selected: do not follow Windows default changes.
  if (FOwner.FDeviceId <> '') then
    Exit;

  if (flow <> FOwner.FDataFlow) then
    Exit;

  if (role <> FOwner.FRole) then
    Exit;

  if (not FOwner.FActive) then
    Exit;

  if (FOwner.FInputSource <> isLoopback) then
    Exit;

  FOwner.QueueRestart();
end;


function TMfDeviceNotificationClient.OnPropertyValueChanged(pwstrDeviceId: LPCWSTR;
                                                            const key: PROPERTYKEY): HResult; stdcall;
begin

  Result := S_OK;
end;

// -----------------------------------------------------------------------------


procedure TMfCaptureThread.Execute();
const
  EPS = 1e-12;

var
  hr: HRESULT;
  Enumerator: IMMDeviceEnumerator;
  Device: IMMDevice;
  AudioClient: IAudioClient;
  NotifyClient: IMMNotificationClient;
  NotifyRegistered: Boolean;

  CaptureClient: IAudioCaptureClient;
  MixFmt: PWaveFormatEx;
  EventHandle: THandle;
  WaitHandles: array[0..1] of THandle;

  PacketFrames, NumFrames: UINT32;
  pData: PByte;
  Flags: DWORD;

  channels,
  sampleRate: Integer;
  isFloat: Boolean;

  peakL,
  peakR: Single;
  sumSqL,
  sumSqR: Double;
  nSamp: Integer;

  smPeakL,
  smPeakR,
  smRmsL,
  smRmsR: Single;
  attackA,
  releaseA: Single;

  monoBlock: TArray<Single>;
  re,
  im: TArray<Single>;
  mags: TArray<Single>;
  bars: TArray<Single>;

  fftN, barCount: Integer;
  i,
  k,
  bar: Integer;
  idxStart,
  idxEnd: Integer;
  maxMag,
  v: Single;

  lastDispatchTick: DWORD;

  tmpMono: TArray<Single>;

  pf: PSingle;
  ps: PSmallInt;
  a,
  b: Single;
  rmsL,
  rmsR: Single;
  aAtk,
  aRel: Single;

  t0,
  t1: Double;
  acc: Single;
  cnt: Integer;

  function NowTick: DWORD; inline;
    begin

      Result := GetTickCount();
    end;

  function ShouldDispatch(): Boolean;
  var
    t: DWORD;

  begin

    t := NowTick;
    if (t - lastDispatchTick) >= DWORD(Max(1,
                                           FOwner.FDispatchEveryMs)) then
      begin

        lastDispatchTick := t;
        Result := True;
      end
    else
      Result := False;
  end;

  procedure AtomicWriteSingle(var DestBits: Integer;
                              const Value: Single); inline;
  begin

    InterlockedExchange(DestBits,
                        SingleToBits(Value));
  end;

  function Smooth(prev,
                  target,
                  aAtkLocal,
                  aRelLocal: Single): Single; inline;
  begin

    if (target > prev) then
      Result := target + (prev - target) * aAtkLocal
    else
      Result := target + (prev - target) * aRelLocal;
  end;

  procedure ComputeAttackRelease();
  var
    dt,
    aa,
    rr: Double;

  begin

    dt := 1.0 / Max(1,
                    sampleRate);
    aa := Exp(-dt / (Max(1,
                         FOwner.FAttackMs) / 1000.0));
    rr := Exp(-dt / (Max(1,
                         FOwner.FReleaseMs) / 1000.0));
    attackA := (aa * 1.0);
    releaseA := (rr * 1.0);
  end;

  procedure PushMonoToRing(const X: TArray<Single>);
  var
    j,
    N: Integer;

  begin

    N := Length(X);
    for j := 0 to N - 1 do
    begin

      FOwner.FMonoRing[FOwner.FRingWrite] := X[j];
      Inc(FOwner.FRingWrite);
      if (FOwner.FRingWrite >= Length(FOwner.FMonoRing)) then
        FOwner.FRingWrite := 0;

      if (FOwner.FRingCount < Length(FOwner.FMonoRing)) then
        Inc(FOwner.FRingCount);
    end;
  end;

  procedure ReadRingLatest(var OutBlock: TArray<Single>);
  var
    j,
    N, start: Integer;

  begin

    N := Length(OutBlock);
    if (N = 0) then
      Exit;

    if (FOwner.FRingCount < N) then
      begin

        FillChar(OutBlock[0],
                 N * SizeOf(Single),
                 0);
        Exit;
      end;

    start := FOwner.FRingWrite - N;
    if (start < 0) then
      start := start + Length(FOwner.FMonoRing);

    for j := 0 to N - 1 do
      begin

        OutBlock[j] := FOwner.FMonoRing[start];
        Inc(start);
        if (start >= Length(FOwner.FMonoRing)) then
          start := 0;
      end;
  end;

  procedure PublishCaptureLevels(const pL,
                                 pR,
                                 rL, rR: Single);
  var
    peakDbL,
    peakDbR,
    rmsDbL,
    rmsDbR: Single;

  begin

    peakDbL := 20 * Log10(Max(EPS_AMP,
                              pL));
    peakDbR := 20 * Log10(Max(EPS_AMP,
                              pR));
    rmsDbL  := 20 * Log10(Max(EPS_AMP,
                              rL));
    rmsDbR  := 20 * Log10(Max(EPS_AMP,
                              rR));

    AtomicWriteSingle(FOwner.FLevelsBits[0],
                      pL);
    AtomicWriteSingle(FOwner.FLevelsBits[1],
                      pR);
    AtomicWriteSingle(FOwner.FLevelsBits[2],
                      rL);
    AtomicWriteSingle(FOwner.FLevelsBits[3],
                      rR);
    AtomicWriteSingle(FOwner.FLevelsBits[4],
                      peakDbL);
    AtomicWriteSingle(FOwner.FLevelsBits[5],
                      peakDbR);
    AtomicWriteSingle(FOwner.FLevelsBits[6],
                      rmsDbL);
    AtomicWriteSingle(FOwner.FLevelsBits[7],
                      rmsDbR);

    // Mark that we received fresh audio data (for auto-reset on stop).
    InterlockedExchange(FOwner.FLastDataTick, Integer(GetTickCount));

    // Clip latch (per channel + any).
    if (pL >= FOwner.FClipThreshold) then
      InterlockedExchange(FOwner.FClipUntilL,
                          Integer(GetTickCount) + FOwner.FClipHoldMs);

    if (pR >= FOwner.FClipThreshold) then
      InterlockedExchange(FOwner.FClipUntilR,
                          Integer(GetTickCount) + FOwner.FClipHoldMs);

    if (pL >= FOwner.FClipThreshold) or (pR >= FOwner.FClipThreshold) then
      InterlockedExchange(FOwner.FClipUntilAny,
                          Integer(GetTickCount) + FOwner.FClipHoldMs);
  end;

  procedure PublishCaptureSpectrum(const BarsLocal: TArray<Single>);
  const
    // How to tune:
    // More punch / faster bar rise:
    //   Increase SPEC_ATTACK_ALPHA to 0.65..0.75
    // Longer trailing decay:
    //   Reduce SPEC_RELEASE_ALPHA to 0.10..0.14
    // Quicker drop:
    //   Increase SPEC_RELEASE_ALPHA to 0.22..0.30
    // More aggressive snap-to-zero:
    //   Raise SPEC_ZERO_SNAP to 0.005..0.01
    //
    SPEC_ATTACK_ALPHA  = 0.70;  // higher = faster rise
    SPEC_RELEASE_ALPHA = 0.18;  // lower = slower fall
    SPEC_ZERO_SNAP     = 0.009; // tiny residuals -> hard zero

  var
    ii,
    fi: Integer;
    prev,
    target,
    outv: Single;

  begin

    fi := Min(Length(BarsLocal),
              Length(FOwner.FSpectrumBits));

    for ii := 0 to fi - 1 do
      begin

        prev := BitsToSingle(
                  InterlockedCompareExchange(FOwner.FSpectrumBits[ii],
                                             0,
                                             0));

        prev := EnsureRange(prev,
                            0.0,
                            1.0);

        target := EnsureRange(BarsLocal[ii],
                              0.0,
                              1.0);

        if (target > prev) then
          outv := prev + (target - prev) * SPEC_ATTACK_ALPHA
        else
          outv := prev + (target - prev) * SPEC_RELEASE_ALPHA;

        if (outv < SPEC_ZERO_SNAP) then
          outv := 0.0;

        InterlockedExchange(FOwner.FSpectrumBits[ii],
                            SingleToBits(outv));
      end;
  end;

begin

  // Never run in design-time (extra safety)
  if (csDesigning in FOwner.ComponentState) then
    Exit;

  CoInitializeEx(nil,
                 COINIT_MULTITHREADED);

  try

    lastDispatchTick := NowTick;

    hr := CoCreateInstance(CLSID_MMDeviceEnumerator,
                           nil,
                           CLSCTX_INPROC_SERVER,
                           IMMDeviceEnumerator,
                           Enumerator);
    if FAILED(hr) then
      Exit;


    // Register for default endpoint changes so we can auto-follow speaker switches.
    NotifyClient := TMfDeviceNotificationClient.Create(FOwner);
    hr := Enumerator.RegisterEndpointNotificationCallback(NotifyClient);
    NotifyRegistered := Succeeded(hr);

    if (FOwner.FDeviceId <> '') then
      hr := Enumerator.GetDevice(PWideChar(WideString(FOwner.FDeviceId)),
                                 Device)
    else
      hr := Enumerator.GetDefaultAudioEndpoint(FOwner.FDataFlow,
                                               FOwner.FRole,
                                               Device);
    if FAILED(hr) then
      Exit;

    hr := Device.Activate(IID_IAudioClient,
                          CLSCTX_INPROC_SERVER,
                          nil,
                          Pointer(AudioClient));
    if FAILED(hr) then
      Exit;

    MixFmt := nil;
    hr := AudioClient.GetMixFormat(MixFmt);
    if FAILED(hr) then
      Exit;

    channels := MixFmt.nChannels;
    sampleRate := MixFmt.nSamplesPerSec;

    isFloat := False;
    if (MixFmt.wFormatTag = WAVE_FORMAT_IEEE_FLOAT) then
      isFloat := True
    else
      if (MixFmt.wFormatTag = WAVE_FORMAT_EXTENSIBLE) then
        begin

          if (PWaveFormatExtensible(MixFmt).SubFormat = KSDATAFORMAT_SUBTYPE_IEEE_FLOAT) then
            isFloat := True;
        end;

    EventHandle := CreateEvent(nil,
                               False,
                               False,
                               nil);
    if (EventHandle = 0) then
      begin

        CoTaskMemFree(MixFmt);
        Exit;
      end;

    hr := AudioClient.Initialize(AUDCLNT_SHAREMODE_SHARED,
                                 AUDCLNT_STREAMFLAGS_LOOPBACK or AUDCLNT_STREAMFLAGS_EVENTCALLBACK,
                                 0,
                                 0,
                                 MixFmt,
                                 nil);
    if FAILED(hr) then
      begin

        CloseHandle(EventHandle);
        CoTaskMemFree(MixFmt);
        Exit;
      end;

    hr := AudioClient.SetEventHandle(EventHandle);
    if FAILED(hr) then
      begin

        CloseHandle(EventHandle);
        CoTaskMemFree(MixFmt);
        Exit;
      end;

    hr := AudioClient.GetService(IAudioCaptureClient,
                                 CaptureClient);
    if FAILED(hr) then
      begin

        CloseHandle(EventHandle);
        CoTaskMemFree(MixFmt);
        Exit;
      end;

    FOwner.FSampleRate := sampleRate;
    FOwner.FChannels := channels;
    FOwner.FIsFloat := isFloat;

    FOwner.EnsureSpectrumStorage;

    fftN := FOwner.FFftSize;
    barCount := FOwner.FBarCount;

    SetLength(monoBlock,
              fftN);
    SetLength(re,
              fftN);
    SetLength(im,
              fftN);
    SetLength(mags,
              fftN div 2);
    SetLength(bars,
              barCount);

    smPeakL := 0;
    smPeakR := 0;
    smRmsL := 0;
    smRmsR := 0;

    ComputeAttackRelease();

    hr := AudioClient.Start();
    if FAILED(hr) then
      begin

        CloseHandle(EventHandle);
        CoTaskMemFree(MixFmt);
        Exit;
      end;

    WaitHandles[0] := FOwner.FStopEvent;
    WaitHandles[1] := EventHandle;

    acc := 0;
    cnt := 0;

    // Do the loop.
    while not Terminated do
      begin

        case WaitForMultipleObjects(2,
                                    @WaitHandles[0],
                                    False,
                                    INFINITE) of
        WAIT_OBJECT_0: Break;

        WAIT_OBJECT_0 + 1:
          begin

            while True do
              begin

                hr := CaptureClient.GetNextPacketSize(PacketFrames);
                if FAILED(hr) or (PacketFrames = 0) then
                  Break;

                pData := nil;
                NumFrames := 0;
                Flags := 0;

                hr := CaptureClient.GetBuffer(pData,
                                              NumFrames,
                                              Flags,
                                              nil,
                                              nil);
                if FAILED(hr) then
                  Break;

                try

                  peakL := 0;
                  peakR := 0;
                  sumSqL := 0;
                  sumSqR := 0;
                  nSamp := Integer(NumFrames);

                  if ((Flags and AUDCLNT_BUFFERFLAGS_SILENT) <> 0) then
                  begin

                    if (FOwner.FMode = amLevelsAndSpectrum) and (Length(FOwner.FMonoRing) > 0) then
                      begin
                        SetLength(tmpMono,
                                  nSamp);
                        if (nSamp > 0) then
                          FillChar(tmpMono[0],
                                   nSamp * SizeOf(Single),
                                   0);
                        PushMonoToRing(tmpMono);
                      end;
                  end
                else
                  begin

                    if (FOwner.FMode = amLevelsAndSpectrum) and (Length(FOwner.FMonoRing) > 0) then
                      SetLength(tmpMono,
                                nSamp);

                    if isFloat then
                      begin

                        pf := PSingle(pData);
                        for i := 0 to nSamp - 1 do
                          begin

                            a := ClampVizSample(pf^ * DbToGain(FOwner.FInputTrimDb)); Inc(pf);
                            sumSqL := sumSqL + (a * a);

                            if (Abs(a) > peakL) then
                              peakL := Abs(a);

                            if (channels > 1) then
                              begin

                                b := ClampVizSample(pf^ * DbToGain(FOwner.FInputTrimDb)); Inc(pf);
                                sumSqR := sumSqR + (b * b);
                                if (Abs(b) > peakR) then
                                  peakR := Abs(b);
                              end
                            else
                              b := a;

                            if (FOwner.FMode = amLevelsAndSpectrum) and (Length(FOwner.FMonoRing) > 0) then
                              tmpMono[i] := 0.5 * (a + b);

                            if (channels > 2) then
                              Inc(pf,
                                  channels - 2);
                          end;

                        if (FOwner.FMode = amLevelsAndSpectrum) and (Length(FOwner.FMonoRing) > 0) then
                          PushMonoToRing(tmpMono);
                      end
                    else
                      begin

                        ps := PSmallInt(pData);
                        for i := 0 to nSamp - 1 do
                          begin

                            a := ClampVizSample((ps^ / 32768.0) * DbToGain(FOwner.FInputTrimDb)); Inc(ps);
                            sumSqL := sumSqL + (a * a);
                            if (Abs(a) > peakL) then
                              peakL := Abs(a);

                            if (channels > 1) then
                              begin

                                b := ClampVizSample((ps^ / 32768.0) * DbToGain(FOwner.FInputTrimDb));
                                Inc(ps);
                                sumSqR := sumSqR + (b * b);

                                if (Abs(b) > peakR) then
                                  peakR := Abs(b);
                              end
                            else
                              b := a;

                            if (FOwner.FMode = amLevelsAndSpectrum) and (Length(FOwner.FMonoRing) > 0) then
                              tmpMono[i] := 0.5 * (a + b);

                            if (channels > 2) then
                              Inc(ps,
                                  channels - 2);
                          end;

                        if (FOwner.FMode = amLevelsAndSpectrum) and (Length(FOwner.FMonoRing) > 0) then
                          PushMonoToRing(tmpMono);
                        end;
                  end;

                finally

                  CaptureClient.ReleaseBuffer(NumFrames);
                end;

                rmsL := (Sqrt(sumSqL / Max(1,
                                           nSamp))) * 1.0;
                rmsR := (Sqrt(sumSqR / Max(1,
                                           nSamp))) * 1.0;

                aAtk := Power(attackA,
                              NumFrames);
                aRel := Power(releaseA,
                              NumFrames);

                smPeakL := Smooth(smPeakL,
                                  peakL,
                                  aAtk,
                                  aRel);

                smPeakR := Smooth(smPeakR,
                                  peakR,
                                  aAtk,
                                  aRel);

                smRmsL  := Smooth(smRmsL,
                                  rmsL,
                                  aAtk,
                                  aRel);

                smRmsR  := Smooth(smRmsR,
                                  rmsR,
                                  aAtk,
                                  aRel);

                if ShouldDispatch() then
                  begin

                    PublishCaptureLevels(smPeakL,
                                         smPeakR,
                                         smRmsL,
                                         smRmsR);

                    if (FOwner.FMode = amLevelsAndSpectrum) and (Length(FOwner.FMonoRing) > 0) then
                      begin

                        try

                          ReadRingLatest(monoBlock);
                          FOwner.BuildSpectrumBars(monoBlock,
                                                   bars);
                          PublishCaptureSpectrum(bars);
                        except
                          on E: Exception do
                            OutputDebugString(PChar('MfAudioMixVisualizer capture spectrum exception: ' +
                                                    E.ClassName + ': ' + E.Message));
                        end;
                      end;
                  end;
              end;
          end;
        else  // case
          Break;
        end;

      end;

    if NotifyRegistered then
      Enumerator.UnregisterEndpointNotificationCallback(NotifyClient);

    AudioClient.Stop();
    CloseHandle(EventHandle);
    CoTaskMemFree(MixFmt);

  finally

    CoUninitialize();
  end;
end;


{ TMfAudioMixVisualizer }

constructor TMfAudioMixVisualizer.Create(AOwner: TComponent);
begin

  inherited Create(AOwner);

  // IMPORTANT: Never allocate timers/bitmaps or handles in the constructor,
  // because IDE palette may instantiate this at install time.
  FBackColor := clBlack;

  FAutoStart := False;
  FActive := False;

  FRestartQueued := 0;
  FInputSource := isLoopback;

  FDataFlow := eRender;
  FRole := eMultimedia;

  FVolumeScaleMode := vsmPerceptual;
  FEnableVolumeSmoothing := True;
  FVolumeSmoothingMs := 60;
  FEndpointVol := nil;
  FVolScalarBits := SingleToBits(1.0);
  FMode := amLevelsAndSpectrum;
  FView := vmSpectrum;

  FBarCount := 48;
  FFftSize := 2048;

  FAttackMs := 15;
  FReleaseMs := 120;
  FDispatchEveryMs := 33;

  FFps := 60;

  FShowRms := True;
  FShowPeakHold := True;
  FPeakHoldMs := 450;

  // Colors
  FBarColor := clAqua;
  FPeakColor := clRed;
  FRmsColor := clYellow;
  FBorderColor := clGray;

  // Peak cap
  FPeakThreshold := 0.98;
  FPeakCapFrac := 0.12;

  // Clip indicator defaults.
  FShowClipIndicator := True;
  FClipIndicatorMode := cimSingle;
  FClipThreshold := 0.999;
  FClipHoldMs := 800;
  FClipUntilL := 0;
  FClipUntilR := 0;
  FClipUntilAny := 0;


  // dB scale overlay
  FShowDbScale := True;
  FInputTrimDb := 12.0;
  FDbTop := 6.0;
  FDbMin := -45.0;
  FDbTickStep := 6;
  FDbLabelStep := 12;
  FDbScaleWidth := 44;

  FillChar(FLevelsBits,
           SizeOf(FLevelsBits),
           0);

  SetLength(FSpectrumBits,
            0);

  SetLength(FMonoRing,
            0);

  FRingWrite := 0;
  FRingCount := 0;

  FSampleRate := 0;
  FChannels := 0;
  FIsFloat := False;

  FSmPeakL := 0;
  FSmPeakR := 0;
  FSmRmsL := 0;
  FSmRmsR := 0;
  FExtAttackA := 0;
  FExtReleaseA := 0;
  FExtLastDispatchTick := GetTickCount();

  FHoldL := 0;
  FHoldR := 0;
  FHoldTickL := GetTickCount();
  FHoldTickR := GetTickCount();

  FStopEvent := 0;
  FBack := nil;
  FTimer := nil;
end;


destructor TMfAudioMixVisualizer.Destroy();
begin

  Active := False;

  if Assigned(FTimer) then
    begin

      FTimer.Enabled := False;
      FTimer.OnTimer := nil;
    end;

  FreeAndNil(FBack);

  if (FStopEvent <> 0) then
    CloseHandle(FStopEvent);

  inherited Destroy();
end;


procedure TMfAudioMixVisualizer.Reset();
begin

  FTimer.Enabled := False;
  Paint();
  ClearVisualState();  // Our existing internal reset logic.
  Invalidate();        // Force redraw immediately.
  FTimer.Enabled := True;
end;


procedure TMfAudioMixVisualizer.BeginExternalFormat(ASampleRate,
                                                    AChannels: Integer;
                                                    AIsFloat: Boolean);
begin

  if (csDesigning in ComponentState) then
    Exit;

  if not FActive then
    Exit;

  if (FInputSource <> isExternalFeed) then
    Exit;

  FSampleRate := ASampleRate;
  FChannels := AChannels;
  FIsFloat := AIsFloat;

  // Reset smoothing coeffs to force recompute on next push
  FExtAttackA := 0;
  FExtReleaseA := 0;
  FExtLastDispatchTick := GetTickCount();

  EnsureSpectrumStorage();
end;


procedure TMfAudioMixVisualizer.PushFloat32Interleaved(pData: PSingle;
                                                       Frames: Integer);
const
  EPS = 1e-12;

var
  i: Integer;
  a,
  b: Single;
  peakL,
  peakR: Single;
  sumSqL,
  sumSqR: Double;
  rmsL,
  rmsR: Single;
  aAtk,
  aRel: Single;
  nowTick: DWORD;

  // spectrum locals
  fftN,
  barCount: Integer;
  monoBlock: TArray<Single>;
  re,
  im: TArray<Single>;
  mags: TArray<Single>;
  bars: TArray<Single>;

  // mono push
  tmpMono: TArray<Single>;
  needSpectrum: Boolean;

  function Smooth(prev,
                  target,
                  aAtkLocal,
                  aRelLocal: Single): Single;
  begin

    if (target > prev) then
      Result := target + (prev - target) * aAtkLocal
    else
      Result := target + (prev - target) * aRelLocal;
  end;

  procedure ComputeAttackRelease();
  var
    dt: Double;
    aa,
    rr: Double;

  begin

    if (FSampleRate <= 0) then
      Exit;

    dt := 1.0 / Max(1,
                   FSampleRate);

    aa := Exp(-dt / (Max(1,
                         FAttackMs) / 1000.0));

    rr := Exp(-dt / (Max(1,
                         FReleaseMs) / 1000.0));
    FExtAttackA := (aa) * 1.0;
    FExtReleaseA := (rr) * 1.0;
  end;

  function ShouldDispatch(): Boolean;
  begin

    nowTick := GetTickCount();

    if (nowTick - FExtLastDispatchTick) >= DWORD(Max(1,
                                                     FDispatchEveryMs)) then
      begin

        FExtLastDispatchTick := nowTick;
        Result := True;
      end
    else
      Result := False;
  end;

  procedure PushMonoToRing(const X: TArray<Single>);
  var
    j,
    N: Integer;

  begin

    N := Length(X);
    for j := 0 to N - 1 do
      begin

        FMonoRing[FRingWrite] := X[j];
        Inc(FRingWrite);
        if (FRingWrite >= Length(FMonoRing)) then
          FRingWrite := 0;

        if (FRingCount < Length(FMonoRing)) then
          Inc(FRingCount);
    end;
  end;

  procedure ReadRingLatest(var OutBlock: TArray<Single>);
  var
    j,
    N,
    start: Integer;

  begin

    N := Length(OutBlock);
    if (N = 0) then
      Exit;

    if (FRingCount < N) then
      begin
        FillChar(OutBlock[0],
                 N * SizeOf(Single),
                 0);
        Exit;
      end;

    start := FRingWrite - N;
    if (start < 0) then
      start := start + Length(FMonoRing);

    for j := 0 to N - 1 do
      begin

        OutBlock[j] := FMonoRing[start];
        Inc(start);
        if (start >= Length(FMonoRing)) then
          start := 0;
      end;
  end;

  procedure PublishExternalLevels(const pL,
                                  pR,
                                  rL,
                                  rR: Single);
  var
    peakDbL,
    peakDbR,
    rmsDbL,
    rmsDbR: Single;

  begin

    peakDbL := 20 * Log10(Max(EPS_AMP,
                              pL));
    peakDbR := 20 * Log10(Max(EPS_AMP,
                              pR));
    rmsDbL  := 20 * Log10(Max(EPS_AMP,
                              rL));
    rmsDbR  := 20 * Log10(Max(EPS_AMP,
                              rR));

    InterlockedExchange(FLevelsBits[0],
                        SingleToBits(pL));
    InterlockedExchange(FLevelsBits[1],
                        SingleToBits(pR));
    InterlockedExchange(FLevelsBits[2],
                        SingleToBits(rL));
    InterlockedExchange(FLevelsBits[3],
                        SingleToBits(rR));
    InterlockedExchange(FLevelsBits[4],
                        SingleToBits(peakDbL));
    InterlockedExchange(FLevelsBits[5],
                        SingleToBits(peakDbR));
    InterlockedExchange(FLevelsBits[6],
                        SingleToBits(rmsDbL));
    InterlockedExchange(FLevelsBits[7],
                        SingleToBits(rmsDbR));

    // Mark fresh data for idle-reset logic.
    InterlockedExchange(FLastDataTick,
                        Integer(GetTickCount));

    // Clip latch (same behavior as loopback path).
    if (pL >= FClipThreshold) then
      InterlockedExchange(FClipUntilL,
                          Integer(GetTickCount) + FClipHoldMs);

    if (pR >= FClipThreshold) then
      InterlockedExchange(FClipUntilR,
                          Integer(GetTickCount) + FClipHoldMs);

    if (pL >= FClipThreshold) or (pR >= FClipThreshold) then
      InterlockedExchange(FClipUntilAny,
                          Integer(GetTickCount) + FClipHoldMs);
  end;

  procedure PublishExternalSpectrum(const BarsLocal: TArray<Single>);
  const
    // How to tune:
    // More punch / faster bar rise:
    //   Increase SPEC_ATTACK_ALPHA to 0.65..0.75
    // Longer trailing decay:
    //   Reduce SPEC_RELEASE_ALPHA to 0.10..0.14
    // More aggressive snap-to-zero:
    //   Raise SPEC_ZERO_SNAP to 0.005..0.01
    //
    SPEC_ATTACK_ALPHA  = 0.70;  // higher = faster rise
    SPEC_RELEASE_ALPHA = 0.18;  // lower = slower fall
    SPEC_ZERO_SNAP     = 0.009; // tiny residuals -> hard zero

  var
    ii,
    fi: Integer;
    prev,
    target,
    outv: Single;

  begin

    fi := Min(Length(BarsLocal),
              Length(FSpectrumBits));

    for ii := 0 to fi - 1 do
      begin

        prev := BitsToSingle(
                  InterlockedCompareExchange(FSpectrumBits[ii],
                                             0,
                                             0));

        prev := EnsureRange(prev,
                            0.0,
                            1.0);

        target := EnsureRange(BarsLocal[ii],
                              0.0,
                              1.0);

        if (target > prev) then
          outv := prev + (target - prev) * SPEC_ATTACK_ALPHA
        else
          outv := prev + (target - prev) * SPEC_RELEASE_ALPHA;

        if (outv < SPEC_ZERO_SNAP) then
          outv := 0.0;

        InterlockedExchange(FSpectrumBits[ii],
                            SingleToBits(outv));
      end;
  end;

begin

  if (csDesigning in ComponentState) then
    Exit;

  if (FInputSource <> isExternalFeed) then
    Exit;

  if (not FActive) then
    Exit;

  if (pData = nil) or (Frames <= 0) then
    Exit;

  if (FSampleRate <= 0) then
    Exit;

  needSpectrum := (FMode = amLevelsAndSpectrum) and (Length(FMonoRing) > 0);

  peakL := 0;
  peakR := 0;
  sumSqL := 0;
  sumSqR := 0;

  if needSpectrum then
    SetLength(tmpMono, Frames);

  for i := 0 to Frames - 1 do
    begin

      a := ClampVizSample(pData^ * DbToGain(FInputTrimDb));
      Inc(pData);

      if (FChannels > 1) then
        begin

          b := ClampVizSample(pData^ * DbToGain(FInputTrimDb));
          Inc(pData);
        end
      else
        b := a;

      sumSqL := sumSqL + (a * a);
      if (Abs(a) > peakL) then
        peakL := Abs(a);

      sumSqR := sumSqR + (b * b);
      if (Abs(b) > peakR) then
        peakR := Abs(b);

      if needSpectrum then
        tmpMono[i] := 0.5 * (a + b);

      if (FChannels > 2) then
        Inc(pData,
            FChannels - 2);
    end;

  rmsL := (Sqrt(sumSqL / Max(1,
                             Frames))) * 1.0;
  rmsR := (Sqrt(sumSqR / Max(1,
                             Frames))) * 1.0;

  if (FExtAttackA = 0) or (FExtReleaseA = 0) then
    ComputeAttackRelease();

  aAtk := Power(FExtAttackA,
                Frames);
  aRel := Power(FExtReleaseA,
                Frames);

  FSmPeakL := Smooth(FSmPeakL,
                     peakL,
                     aAtk,
                     aRel);

  FSmPeakR := Smooth(FSmPeakR,
                     peakR,
                     aAtk,
                     aRel);

  FSmRmsL  := Smooth(FSmRmsL,
                     rmsL,
                     aAtk,
                     aRel);

  FSmRmsR  := Smooth(FSmRmsR,
                     rmsR,
                     aAtk,
                     aRel);

  if needSpectrum then
    PushMonoToRing(tmpMono);

  if ShouldDispatch() then
    begin

      PublishExternalLevels(FSmPeakL,
                            FSmPeakR,
                            FSmRmsL,
                            FSmRmsR);

      if needSpectrum then
        begin

          fftN := FFftSize;
          barCount := FBarCount;

          SetLength(monoBlock, fftN);
          SetLength(re, fftN);
          SetLength(im, fftN);
          SetLength(mags, fftN div 2);
          SetLength(bars, barCount);

          try

            ReadRingLatest(monoBlock);
            BuildSpectrumBars(monoBlock,
                              bars);
            PublishExternalSpectrum(bars);
          except
            on E: Exception do
              OutputDebugString(PChar('MfAudioMixVisualizer external spectrum exception: ' +
                                      E.ClassName + ': ' + E.Message));
          end;
        end;
    end;
end;


procedure TMfAudioMixVisualizer.PushInt16Interleaved(pData: PSmallInt; Frames: Integer);
var
  i: Integer;
  a16,
  b16: SmallInt;
  a,
  b: Single;
  peakL,
  peakR: Single;
  sumSqL,
  sumSqR: Double;
  rmsL,
  rmsR: Single;
  aAtk,
  aRel: Single;
  nowTick: DWORD;

  // spectrum
  tmpMono: TArray<Single>;
  needSpectrum: Boolean;

  function Smooth(prev,
                  target,
                  aAtkLocal,
                  aRelLocal: Single): Single;
  begin

    if (target > prev) then
      Result := target + (prev - target) * aAtkLocal
    else
      Result := target + (prev - target) * aRelLocal;
  end;

  procedure ComputeAttackRelease();
  var
    dt: Double;
    aa,
    rr: Double;

  begin

    if FSampleRate <= 0 then
      Exit;

    dt := 1.0 / Max(1, FSampleRate);
    aa := Exp(-dt / (Max(1,
                         FAttackMs) / 1000.0));
    rr := Exp(-dt / (Max(1,
                         FReleaseMs) / 1000.0));

    FExtAttackA := (aa) * 1.0;
    FExtReleaseA := (rr) * 1.0;
  end;

  function ShouldDispatch(): Boolean;
  begin

    nowTick := GetTickCount();

    if (nowTick - FExtLastDispatchTick) >= DWORD(Max(1,
                                                     FDispatchEveryMs)) then
      begin

        FExtLastDispatchTick := nowTick;
        Result := True;
      end
    else
      Result := False;
  end;

  procedure PublishExternalLevelsOnly(const pL,
                                      pR,
                              rL,
                              rR: Single);
  var
    peakDbL,
    peakDbR,
    rmsDbL,
    rmsDbR: Single;

  begin

    peakDbL := 20 * Log10(Max(EPS_AMP,
                              pL));
    peakDbR := 20 * Log10(Max(EPS_AMP,
                              pR));
    rmsDbL  := 20 * Log10(Max(EPS_AMP,
                              rL));
    rmsDbR  := 20 * Log10(Max(EPS_AMP,
                              rR));

    InterlockedExchange(FLevelsBits[0],
                        SingleToBits(pL));
    InterlockedExchange(FLevelsBits[1],
                        SingleToBits(pR));
    InterlockedExchange(FLevelsBits[2],
                        SingleToBits(rL));
    InterlockedExchange(FLevelsBits[3],
                        SingleToBits(rR));
    InterlockedExchange(FLevelsBits[4],
                        SingleToBits(peakDbL));
    InterlockedExchange(FLevelsBits[5],
                        SingleToBits(peakDbR));
    InterlockedExchange(FLevelsBits[6],
                        SingleToBits(rmsDbL));
    InterlockedExchange(FLevelsBits[7],
                        SingleToBits(rmsDbR));

    // Mark fresh data for idle-reset logic.
    InterlockedExchange(FLastDataTick,
                        Integer(GetTickCount));

    // Clip latch.
    if (pL >= FClipThreshold) then
      InterlockedExchange(FClipUntilL,
                          Integer(GetTickCount) + FClipHoldMs);

    if (pR >= FClipThreshold) then
      InterlockedExchange(FClipUntilR,
                          Integer(GetTickCount) + FClipHoldMs);

    if (pL >= FClipThreshold) or (pR >= FClipThreshold) then
      InterlockedExchange(FClipUntilAny,
                          Integer(GetTickCount) + FClipHoldMs);
  end;

  procedure PushMonoToRing(const X: TArray<Single>);
  var
    j,
    N: Integer;

  begin

    N := Length(X);

    for j := 0 to N - 1 do
      begin

        FMonoRing[FRingWrite] := X[j];
        Inc(FRingWrite);
        if (FRingWrite >= Length(FMonoRing)) then
          FRingWrite := 0;

        if (FRingCount < Length(FMonoRing)) then
          Inc(FRingCount);
      end;
  end;

begin

  if (csDesigning in ComponentState) then
    Exit;

  if (FInputSource <> isExternalFeed) then
    Exit;

  if (not FActive) then
    Exit;

  if (pData = nil) or (Frames <= 0) then
    Exit;

  if (FSampleRate <= 0) then
    Exit;

  needSpectrum := (FMode = amLevelsAndSpectrum) and (Length(FMonoRing) > 0);
  if needSpectrum then
    SetLength(tmpMono,
              Frames);

  peakL := 0;
  peakR := 0;
  sumSqL := 0;
  sumSqR := 0;

  for i := 0 to Frames - 1 do
  begin

    a16 := pData^;
    Inc(pData);

    if FChannels > 1 then
      begin

        b16 := pData^;
        Inc(pData);
      end
    else
      b16 := a16;

    a := a16 / 32768.0;
    b := b16 / 32768.0;

    sumSqL := sumSqL + (a * a);
    if (Abs(a) > peakL) then
      peakL := Abs(a);

    sumSqR := sumSqR + (b * b);
    if (Abs(b) > peakR) then
      peakR := Abs(b);

    if needSpectrum then
      tmpMono[i] := 0.5 * (a + b);

    if FChannels > 2 then
      Inc(pData, FChannels - 2);
  end;

  rmsL := (Sqrt(sumSqL / Max(1,
                             Frames))) * 1.0;
  rmsR := (Sqrt(sumSqR / Max(1,
                             Frames))) * 1.0;

  if (FExtAttackA = 0) or (FExtReleaseA = 0) then
    ComputeAttackRelease();

  aAtk := Power(FExtAttackA,
                Frames);

  aRel := Power(FExtReleaseA,
                Frames);

  FSmPeakL := Smooth(FSmPeakL,
                     peakL,
                     aAtk,
                     aRel);

  FSmPeakR := Smooth(FSmPeakR,
                     peakR,
                     aAtk,
                     aRel);

  FSmRmsL  := Smooth(FSmRmsL,
                     rmsL,
                     aAtk,
                     aRel);

  FSmRmsR  := Smooth(FSmRmsR,
                     rmsR,
                     aAtk,
                     aRel);

  if needSpectrum then
    PushMonoToRing(tmpMono);

  if ShouldDispatch() then
    PublishExternalLevelsOnly(FSmPeakL, FSmPeakR, FSmRmsL, FSmRmsR);
end;


procedure TMfAudioMixVisualizer.Loaded();
var
  interval: Integer;

begin

  inherited Loaded();

  if (csDesigning in ComponentState) then
    Exit;

  EnsureSpectrumStorage();

  if not Assigned(FBack) then
    begin

      FBack := TBitmap.Create();
      FBack.PixelFormat := pf32bit;
      FBack.SetSize(Max(1,
                        Width),
                    Max(1,
                        Height));
    end;

  if not Assigned(FTimer) then
    begin

      FTimer := TTimer.Create(Self);
      interval := 1000 div Max(1,
                               FFps);
      if (interval < 1) then
        interval := 1;

      FTimer.Interval := interval;
      FTimer.OnTimer := TimerTick;
      FTimer.Enabled := True;
    end;

  if FAutoStart then
    Active := True;
end;


procedure TMfAudioMixVisualizer.TimerTick(Sender: TObject);
const
  VIS_IDLE_RESET_MS = 200;

var
  Muted: INT;
  MasterVol: Single;
  TargetVol: Single;
  CurVol: Single;
  Alpha: Single;
  LastTick: DWORD;
  NowTick: DWORD;

begin

  if (csDesigning in ComponentState) then
    Exit;

  // Read endpoint master volume scalar for visual scaling (UI thread).
  TargetVol := 1.0;

  if (FEndpointVol <> nil) then
    begin

      try

        Muted := 0;
        MasterVol := 1.0;

        if Succeeded(FEndpointVol.GetMute(Muted)) and
           Succeeded(FEndpointVol.GetMasterVolumeLevelScalar(MasterVol)) then
          begin

            if (Muted <> 0) then
              MasterVol := 0.0;

            TargetVol := EnsureRange(MasterVol,
                                     0.0,
                                     1.0);
          end;
      except

        TargetVol := 1.0;
      end;
    end;

  // Optional smoothing so dragging the Windows slider doesn't cause hard jumps.
  if FEnableVolumeSmoothing and (FVolumeSmoothingMs > 0) then
    begin

      CurVol := BitsToSingle(InterlockedCompareExchange(FVolScalarBits,
                                                        0,
                                                        0));
      CurVol := EnsureRange(CurVol,
                            0.0,
                            1.0);

      Alpha := EnsureRange(FTimer.Interval / Max(1,
                                                 FVolumeSmoothingMs),
                           0.0,
                           1.0);

      CurVol := CurVol + (TargetVol - CurVol) * Alpha;

      InterlockedExchange(FVolScalarBits,
                          SingleToBits(CurVol));
    end
  else
    InterlockedExchange(FVolScalarBits,
                        SingleToBits(TargetVol));

  // Auto-reset visual state if no fresh audio data arrived recently.
  LastTick := DWORD(InterlockedCompareExchange(FLastDataTick,
                                               0,
                                               0));
  NowTick := GetTickCount;

  if (LastTick <> 0) and (DWORD(NowTick - LastTick) > VIS_IDLE_RESET_MS) then
    ClearVisualState();

  Invalidate();
end;


procedure TMfAudioMixVisualizer.SetBackColor(Value: TColor);
begin

  if (FBackColor = Value) then
    Exit;
  FBackColor := Value;
  Invalidate;
end;


procedure TMfAudioMixVisualizer.SetBarColor(Value: TColor);
begin

  if (FBarColor = Value) then
    Exit;

  FBarColor := Value;
  Invalidate;
end;


procedure TMfAudioMixVisualizer.SetPeakColor(Value: TColor);
begin

  if (FPeakColor = Value) then
    Exit;

  FPeakColor := Value;
  Invalidate;
end;


procedure TMfAudioMixVisualizer.SetRmsColor(Value: TColor);
begin

  if (FRmsColor = Value) then
    Exit;

  FRmsColor := Value;
  Invalidate;
end;


procedure TMfAudioMixVisualizer.SetBorderColor(Value: TColor);
begin

  if (FBorderColor = Value) then
    Exit;

  FBorderColor := Value;
  Invalidate;
end;




function TMfAudioMixVisualizer.DbToDisplayFrac(const ADb,
                                               ADbMin,
                                               ADbTop: Single): Single;
var
  DbClamped: Single;

begin

  if (ADbTop <= ADbMin + 0.001) then
    Exit(0.0);

  DbClamped := EnsureRange(ADb,
                           ADbMin,
                           ADbTop);

  Result := (DbClamped - ADbMin) / (ADbTop - ADbMin);
end;


procedure TMfAudioMixVisualizer.BuildSpectrumBars(const MonoBlock: TArray<Single>;
                                                  out Bars: TArray<Single>);
const
  EPS_AMP = 1.0e-12;
  HANN_COHERENT_GAIN = 0.5;

var
  fftN: Integer;
  barCount: Integer;
  re: TArray<Single>;
  im: TArray<Single>;
  magsDb: TArray<Single>;
  i: Integer;
  k: Integer;
  bar: Integer;
  t0: Double;
  t1: Double;
  idxStart: Integer;
  idxEnd: Integer;
  v: Double;
  PeakDb: Single;

begin

  fftN := FFftSize;
  barCount := FBarCount;

  SetLength(Bars, barCount);
  if (fftN <= 0) or
     (barCount <= 0) or
     (Length(MonoBlock) < fftN) then
    Exit;

  SetLength(re,
            fftN);
  SetLength(im,
            fftN);
  SetLength(magsDb,
            fftN div 2);

  for i := 0 to fftN - 1 do
    begin

      re[i] := MonoBlock[i];
      im[i] := 0.0;
    end;

  TSimpleFFT.HannWindow(re);
  TSimpleFFT.FFT(re,
                 im);

  magsDb[0] := FDbMin;

  for k := 1 to (fftN div 2) - 1 do
    begin

      v := Sqrt((re[k] * re[k]) + (im[k] * im[k]));

      v := (2.0 * v) / fftN;
      v := v / HANN_COHERENT_GAIN;

      if (v > EPS_AMP) then
        magsDb[k] := 20.0 * Log10(v)
      else
        magsDb[k] := FDbMin;
    end;

  for bar := 0 to barCount - 1 do
    begin

      t0 := bar / barCount;
      t1 := (bar + 1) / barCount;

      idxStart := 1 + Trunc((Power(fftN div 2, t0) - 1));
      idxEnd   := 1 + Trunc((Power(fftN div 2, t1) - 1));

      if (idxEnd <= idxStart) then
        idxEnd := idxStart + 1;

      if (idxEnd >= (fftN div 2)) then
        idxEnd := (fftN div 2) - 1;

      PeakDb := FDbMin;

      for k := idxStart to idxEnd do
        if (magsDb[k] > PeakDb) then
          PeakDb := magsDb[k];

      Bars[bar] := DbToDisplayFrac(PeakDb,
                                   FDbMin,
                                   FDbTop);
    end;
end;


procedure TMfAudioMixVisualizer.SetShowDbScale(Value: Boolean);
begin

  if (FShowDbScale <> Value) then
    begin

      FShowDbScale := Value;
      Invalidate();
    end;
end;


procedure TMfAudioMixVisualizer.SetShowMeters(Value: Boolean);
begin

  if (FShowMeters <> Value) then
    begin

      FShowMeters := Value;
      Invalidate();
    end;
  
end;


procedure TMfAudioMixVisualizer.SetDbTop(Value: Single);
begin

  Value := EnsureRange(Value,
                       -12.0,
                       24.0);

  if (Value <= FDbMin + 0.1) then
    Value := FDbMin + 0.1;

  if not SameValue(FDbTop,
                   Value,
                   1E-6) then
    begin

      FDbTop := Value;
      Invalidate();
    end;
end;


procedure TMfAudioMixVisualizer.SetDbMin(Value: Single);
begin

  // Must be negative.
  if (Value > -1.0) then
    Value := -1.0;

  if (FDbMin <> Value) then
    begin

      FDbMin := Value;
      Invalidate();
    end;
end;


procedure TMfAudioMixVisualizer.SetDbTickStep(Value: Integer);
begin

  if (Value < 1) then
    Value := 1;

  if (FDbTickStep <> Value) then
    begin

      FDbTickStep := Value;
      Invalidate();
    end;
end;


procedure TMfAudioMixVisualizer.SetDbLabelStep(Value: Integer);
begin

  if (Value < 1) then
    Value := 1;

  if (FDbLabelStep <> Value) then
    begin

      FDbLabelStep := Value;
      Invalidate();
    end;
end;


procedure TMfAudioMixVisualizer.SetDbScaleWidth(Value: Integer);
begin

  if (Value < 0) then
    Value := 0;

  if (FDbScaleWidth <> Value) then
    begin

      FDbScaleWidth := Value;
      Invalidate();
    end;
end;


procedure TMfAudioMixVisualizer.SetPeakThreshold(Value: Single);
begin

  Value := EnsureRange(Value,
                       0.0,
                       1.0);

  if SameValue(FPeakThreshold,
               Value,
               1e-6) then
    Exit;
  FPeakThreshold := Value;
end;


procedure TMfAudioMixVisualizer.SetPeakCapFrac(Value: Single);
begin

  Value := EnsureRange(Value,
                       0.0,
                       1.0);

  if SameValue(FPeakCapFrac,
               Value,
               1e-6) then
    Exit;
  FPeakCapFrac := Value;
end;


procedure TMfAudioMixVisualizer.DrawDesignTimePlaceholder(ACanvas: TCanvas;
                                                       const R: TRect);
begin

  ACanvas.Brush.Style := bsSolid;
  ACanvas.Brush.Color := FBackColor;
  ACanvas.FillRect(R);

  ACanvas.Brush.Style := bsClear;
  ACanvas.Font.Color := clGrayText;

  ACanvas.TextOut(R.Left + 8,
                  R.Top + 8,
                  'TMfAudioMixVisualizer');

  ACanvas.TextOut(R.Left + 8,
                  R.Top + 28,
                  'Design-time: no capture/timers');
end;


procedure TMfAudioMixVisualizer.SetDeviceId(const Value: string);
begin

  if SameText(FDeviceId,
              Value) then
    Exit;

  FDeviceId := Trim(Value);
  RestartIfRunning();
end;


procedure TMfAudioMixVisualizer.Paint();
var
  R: TRect;

begin

  // Absolutely no runtime allocations/threads/timers here.
  // Also, palette-instantiation may have Parent=nil.
  if (Parent = nil) then
    Exit;

  R := Rect(0,
            0,
            Width,
            Height);

  if (csDesigning in ComponentState) then
    begin

      DrawDesignTimePlaceholder(Canvas,
                                R);
      Exit;
    end;

  if (Width <= 0) or (Height <= 0) then
    Exit;

  if not Assigned(FBack) then
    begin

      // runtime-only lazy create.
      FBack := TBitmap.Create();
      FBack.PixelFormat := pf32bit;

      FBack.SetSize(Max(1,
                        Width),
                    Max(1,
                        Height));
    end;

  if (FBack.Width <> Width) or (FBack.Height <> Height) then
    FBack.SetSize(Width,
                  Height);

  FBack.Canvas.Brush.Style := bsSolid;
  FBack.Canvas.Brush.Color := FBackColor;
  FBack.Canvas.FillRect(R);

  if (FView = vmMeters) then
    DrawMeters(FBack.Canvas,
               R)
  else
    DrawSpectrum(FBack.Canvas,
                 R);

  Canvas.Draw(0,
              0,
              FBack);
end;


procedure TMfAudioMixVisualizer.SetInputTrimDb(Value: Single);
begin

  if SameValue(FInputTrimDb,
               Value,
               1.0e-4) then
    Exit;

  FInputTrimDb := EnsureRange(Value,
                              -24.0,
                              24.0);
  Invalidate;
end;


procedure TMfAudioMixVisualizer.ClearVisualState();
var
  i: Integer;

begin

  // Zero published levels/spectrum so the UI drops to silence immediately.
  for i := Low(FLevelsBits) to High(FLevelsBits) do
    InterlockedExchange(FLevelsBits[i],
                        0);

  for i := 0 to Length(FSpectrumBits) - 1 do
    InterlockedExchange(FSpectrumBits[i],
                        0);

  // Reset peak-hold state.
  FHoldL := 0;
  FHoldR := 0;
  FHoldTickL := GetTickCount();
  FHoldTickR := GetTickCount();

  // Reset smoothing/ring state (so external-feed restarts clean).
  FSmPeakL := 0;
  FSmPeakR := 0;
  FSmRmsL := 0;
  FSmRmsR := 0;
  FRingWrite := 0;
  FRingCount := 0;

  InterlockedExchange(FLastDataTick,
                      0);

  InterlockedExchange(FClipUntilL,
                      0);
  InterlockedExchange(FClipUntilR,
                      0);
  InterlockedExchange(FClipUntilAny,
                      0);
end;


procedure TMfAudioMixVisualizer.EnsureSpectrumStorage();
var
  ringSize: Integer;
  p: Integer;

begin

  if (csDesigning in ComponentState) then
    Exit;

  if (FMode <> amLevelsAndSpectrum) then
    begin

      SetLength(FSpectrumBits,
                0);

      SetLength(FMonoRing,
                0);
      FRingWrite := 0;
      FRingCount := 0;
      Exit;
    end;

  if (FBarCount < 8) then
    FBarCount := 8;

  if (FFftSize < 256) then
    FFftSize := 256;

  p := 1;
  while (p < FFftSize) do
    p := p shl 1;

  FFftSize := p;

  if (Length(FSpectrumBits) <> FBarCount) then
    SetLength(FSpectrumBits,
              FBarCount);

  ringSize := FFftSize * 4;
  if (Length(FMonoRing) <> ringSize) then
    begin

      SetLength(FMonoRing,
                ringSize);
      FRingWrite := 0;
      FRingCount := 0;
    end;
end;


procedure TMfAudioMixVisualizer.StartCapture();
var
  hr: HRESULT;
  DevEnum: IMMDeviceEnumerator;
  Dev: IMMDevice;

begin

  if (csDesigning in ComponentState) then
    Exit;

  if (FInputSource <> isLoopback) then
    Exit;

  if Assigned(FCaptureThread) then
    Exit;

  if (FStopEvent = 0) then
    FStopEvent := CreateEvent(nil,
                              True,
                              False,
                              nil)
  else
    ResetEvent(FStopEvent);

  EnsureSpectrumStorage();

  // Acquire endpoint master volume interface (for visual scaling only).
  // This does NOT affect the audio data used for FFT; it only scales what we display.
  if (FEndpointVol = nil) then
    begin

      DevEnum := nil;
      Dev := nil;

      // Try to ensure COM on this thread; ignore mode mismatch.
      hr := CoInitializeEx(nil,
                           COINIT_APARTMENTTHREADED);
      if (hr <> S_OK) and (hr <> S_FALSE) and (hr <> RPC_E_CHANGED_MODE) then
        ; // ignore

      hr := CoCreateInstance(CLSID_MMDeviceEnumerator,
                             nil,
                             CLSCTX_INPROC_SERVER,
                             IID_IMMDeviceEnumerator,
                             DevEnum);

      if Succeeded(hr) then
        begin

          if (FDeviceId <> '') then
            hr := DevEnum.GetDevice(PWideChar(WideString(FDeviceId)),
                                    Dev)
          else
            hr := DevEnum.GetDefaultAudioEndpoint(FDataFlow,
                                                  FRole,
                                                  Dev);

          if Succeeded(hr) then
            begin
              Dev.Activate(IID_IAudioEndpointVolume,
                           CLSCTX_INPROC_SERVER,
                           nil,
                           Pointer(FEndpointVol));
            end;
        end;
    end;

  FCaptureThread := TMfCaptureThread.Create(Self);
  FCaptureThread.Priority := tpHighest;
end;


procedure TMfAudioMixVisualizer.StopCapture();
begin

  if Assigned(FCaptureThread) then
    begin

      if (FStopEvent <> 0) then
        SetEvent(FStopEvent);

      FCaptureThread.Terminate();
      FCaptureThread.WaitFor();
      FreeAndNil(FCaptureThread);
    end;

  FEndpointVol := nil;

  ClearVisualState();
  Invalidate;
end;


procedure TMfAudioMixVisualizer.QueueRestart();
begin

  if (csDesigning in ComponentState) then
    Exit;

  if not FActive then
    Exit;

  if (FInputSource <> isLoopback) then
    Exit;

  // Prevent restart storms when the system fires multiple notifications.
  if (InterlockedExchange(FRestartQueued, 1) <> 0) then
    Exit;

  // Queue onto main thread without anonymous methods (package-safe).
  TThread.Queue(nil,
                DoQueuedRestart);
end;


procedure TMfAudioMixVisualizer.DoQueuedRestart();
begin

  try

    RestartIfRunning();
  finally

    InterlockedExchange(FRestartQueued,
                        0);
  end;
end;


procedure TMfAudioMixVisualizer.RestartIfRunning();
begin

  if (csDesigning in ComponentState) then
    Exit;

  if not FActive then
    Exit;

  if (FInputSource = isLoopback) then
    begin

      Active := False;
      Active := True;
    end
  else
    begin

      // External feed: never start loopback thread, but ensure any previous thread is stopped.
      StopCapture();
    end;
end;


procedure TMfAudioMixVisualizer.SetActive(Value: Boolean);
begin

  if (FActive = Value) then
    Exit;

  if (csDesigning in ComponentState) then
    begin

      FActive := False;
      Exit;
    end;

  FActive := Value;

  // Stop loopback thread when deactivating or when not in loopback mode.
  if (not FActive) then
    begin

      StopCapture();
      ClearVisualState();
      Invalidate;
      Exit;
    end;

  if (FInputSource = isLoopback) then
    StartCapture
  else
    StopCapture; // ensure any previous loopback thread is stopped
end;


procedure TMfAudioMixVisualizer.SetAutoStart(Value: Boolean);
begin

  if (FAutoStart = Value) then
    Exit;
  FAutoStart := Value;
end;


procedure TMfAudioMixVisualizer.SetInputSource(Value: TMfAudioInputSource);
begin

  if (FInputSource = Value) then
    Exit;

  // Switching source may require stopping loopback thread.
  FInputSource := Value;

  // If running, restart to apply new source selection.
  RestartIfRunning;
end;


procedure TMfAudioMixVisualizer.SetDeviceDataFlow(Value: EDataFlow);
begin

  if (FDataFlow = Value) then
    Exit;

  FDataFlow := Value;
  RestartIfRunning;
end;


procedure TMfAudioMixVisualizer.SetDeviceRole(Value: ERole);
begin

  if (FRole = Value) then
    Exit;

  FRole := Value;
  RestartIfRunning;
end;


procedure TMfAudioMixVisualizer.SetVolumeScaleMode(Value: TMfVolumeScaleMode);
begin

  if (FVolumeScaleMode = Value) then
    Exit;
  FVolumeScaleMode := Value;
  Invalidate;
end;


function TMfAudioMixVisualizer.GetVolumeVisualGain(): Single;
var
  s: Single;

begin

  // Atomic endpoint scalar (0..1). Defaults to 1.0 if endpoint volume is unavailable.
  s := BitsToSingle(InterlockedCompareExchange(FVolScalarBits,
                                               0,
                                               0));

  if (s <= 0.000001) then
    Exit(0.0);

  case FVolumeScaleMode of

    vsmLinear: Result := s;

    vsmPerceptual: Result := Sqrt(s);

    vsmDbPerceptual:
      begin

        // dB-ish perceptual mapping: 0..1 -> (-60..0 dB) -> 0..1
        if (s <= 0.000001) then
          Exit(0.0);

        Result := 20.0 * Log10(s); // negative
        if (Result < -60.0) then
          Result := -60.0;

        Result := (Result + 60.0) / 60.0;
      end;
  else
    Result := s;
  end;
end;


procedure TMfAudioMixVisualizer.SetMode(Value: TMfAnalyzerMode);
begin

  if (FMode = Value) then
    Exit;

  FMode := Value;
  if not (csDesigning in ComponentState) then
    EnsureSpectrumStorage;

  RestartIfRunning();
  Invalidate();
end;


procedure TMfAudioMixVisualizer.SetView(Value: TMfVizMode);
begin

  if (FView = Value) then
    Exit;

  FView := Value;
  Invalidate;
end;


procedure TMfAudioMixVisualizer.SetBarCount(Value: Integer);
begin

  Value := EnsureRange(Value,
                       8,
                       256);

  if FBarCount = Value then
    Exit;

  FBarCount := Value;
  if not (csDesigning in ComponentState) then
    EnsureSpectrumStorage();

  Invalidate();
end;


procedure TMfAudioMixVisualizer.SetFftSize(Value: Integer);
begin

  Value := EnsureRange(Value,
                       256,
                       16384);

  if (FFftSize = Value) then
    Exit;

  FFftSize := Value;
  if not (csDesigning in ComponentState) then
    EnsureSpectrumStorage();

  Invalidate();
end;


procedure TMfAudioMixVisualizer.SetAttackMs(Value: Integer);
begin

  Value := EnsureRange(Value,
                       1,
                       500);
  if (FAttackMs = Value) then
    Exit;
  FAttackMs := Value;

  FExtAttackA := 0;
  FExtReleaseA := 0;
end;


procedure TMfAudioMixVisualizer.SetReleaseMs(Value: Integer);
begin

  Value := EnsureRange(Value,
                       1,
                       2000);

  if (FReleaseMs = Value) then
    Exit;
  FReleaseMs := Value;

  FExtAttackA := 0;
  FExtReleaseA := 0;
end;


procedure TMfAudioMixVisualizer.SetDispatchEveryMs(Value: Integer);
begin

  Value := EnsureRange(Value,
                       10,
                       200);

  if (FDispatchEveryMs = Value) then
    Exit;
  FDispatchEveryMs := Value;
end;


procedure TMfAudioMixVisualizer.SetFps(Value: Integer);
var
  newInterval: Integer;

begin

  Value := EnsureRange(Value,
                       10,
                       120);

  if (FFps = Value) then
    Exit;

  FFps := Value;

  if (csDesigning in ComponentState) then
    Exit;

  if Assigned(FTimer) then
    begin

      newInterval := 1000 div Max(1,
                                  FFps);
      if (newInterval < 1) then
        newInterval := 1;

      FTimer.Interval := newInterval;
    end;
end;


function TMfAudioMixVisualizer.GetLevels(): TMfLevels;
var
  g: Single;

begin

  Result.PeakL := BitsToSingle(InterlockedCompareExchange(FLevelsBits[0],
                               0,
                               0));

  Result.PeakR := BitsToSingle(InterlockedCompareExchange(FLevelsBits[1],
                               0,
                               0));

  Result.RmsL  := BitsToSingle(InterlockedCompareExchange(FLevelsBits[2],
                               0,
                               0));

  Result.RmsR  := BitsToSingle(InterlockedCompareExchange(FLevelsBits[3],
                               0,
                               0));

  Result.PeakDbL := BitsToSingle(InterlockedCompareExchange(FLevelsBits[4],
                                 0,
                                 0));

  Result.PeakDbR := BitsToSingle(InterlockedCompareExchange(FLevelsBits[5],
                                 0,
                                 0));

  Result.RmsDbL  := BitsToSingle(InterlockedCompareExchange(FLevelsBits[6],
                                 0,
                                 0));

  Result.RmsDbR  := BitsToSingle(InterlockedCompareExchange(FLevelsBits[7],
                                 0,
                                 0));

  // Apply endpoint master-volume visual scaling (optional).
  g := GetVolumeVisualGain();
  if (g <> 1.0) then
    begin

      Result.PeakL := Result.PeakL * g;
      Result.PeakR := Result.PeakR * g;
      Result.RmsL  := Result.RmsL  * g;
      Result.RmsR  := Result.RmsR  * g;
    end;
end;


function TMfAudioMixVisualizer.GetSpectrum(): TMfSpectrum;
var
  i: Integer;
  g: Single;

begin

  SetLength(Result,
            Length(FSpectrumBits));

  for i := 0 to High(FSpectrumBits) do
    Result[i] := BitsToSingle(InterlockedCompareExchange(FSpectrumBits[i],
                              0,
                              0));

  // Apply endpoint master-volume visual scaling (optional).
  g := GetVolumeVisualGain();
  if (g <> 1.0) then
    for i := 0 to High(Result) do
      Result[i] := Result[i] * g;

end;


procedure TMfAudioMixVisualizer.DrawMeters(ACanvas: TCanvas;
                                           const R: TRect);

const
  TOP_M = 8;
  BOT_M = 8;
  EPS_AMP = 1.0e-12;

var
  PlotR,
  ScaleR,
  PlotArea: TRect;
  DbMinLocal: Single;

  function AmpToDb(const A: Single): Single; inline;
  begin

    Result := 20.0 * Log10(Max(A,
                           EPS_AMP));
  end;

  function DbToFrac(const Db: Single): Double;
  var
    dbClamped: Double;

  begin

    dbClamped := EnsureRange(Db, DbMinLocal, 0.0);
    Result := (dbClamped - DbMinLocal) / (0.0 - DbMinLocal); // 0..1
  end;

  function AmpToFrac(const A: Single): Double; inline;
  begin

    Result := DbToFrac(AmpToDb(A));
  end;

  procedure DrawDbScale(const ScaleR,
                        PlotArea: TRect);
  var
    db: Integer;
    axisX: Integer;
    y: Integer;
    tickLen: Integer;
    lbl: string;
    dbMinInt: Integer;

  begin

    ACanvas.Brush.Style := bsClear;
    ACanvas.Font.Color := clSilver;
    ACanvas.Pen.Style := psSolid;
    ACanvas.Pen.Color := clGray;

    axisX := PlotArea.Left - 4;

    // Axis line
    ACanvas.MoveTo(axisX,
                   PlotArea.Top);

    ACanvas.LineTo(axisX,
                   PlotArea.Bottom);

    dbMinInt := Trunc(DbMinLocal);

    for db := 0 downto dbMinInt do
      if ((Abs(db) mod FDbTickStep) = 0) then
        begin

          y := PlotArea.Bottom - Round(DbToFrac(db) * RectHeight(PlotArea));

          if ((Abs(db) mod FDbLabelStep) = 0) or (db = 0) then
            tickLen := 8
          else
            tickLen := 5;

          ACanvas.MoveTo(axisX - tickLen, y);
          ACanvas.LineTo(axisX, y);

          if ((Abs(db) mod FDbLabelStep) = 0) or (db = 0) then
            begin

              lbl := IntToStr(db);
              ACanvas.TextOut(ScaleR.Left + 2,
                              y - (ACanvas.TextHeight('0') div 2),
                              lbl);
            end;
        end;
  end;

  function BarRect(const idx,
                   total: Integer): TRect;
  var
    w,
    gap,
    bw,
    x0: Integer;

  begin

    w := RectWidth(PlotR);
    gap := Max(4, w div 80);

    bw := (w - gap * (total + 1)) div total;
    if (bw < 10) then
      bw := 10;

    x0 := PlotR.Left + gap + idx * (bw + gap);

    Result := Rect(x0,
                   PlotR.Top + TOP_M,
                   x0 + bw,
                   PlotR.Bottom - BOT_M);
  end;

  procedure DrawBar(const BR: TRect;
                    const level,
                    rms,
                    hold: Single;
                    showRms,
                    showHold: Boolean);
  var
    h: Integer;
    fillR: TRect;
    yLevel,
    yRms,
    yHold: Integer;
    varCapH: Integer;
    capR: TRect;

  begin

    ACanvas.Pen.Color := FBorderColor;
    ACanvas.Brush.Style := bsClear;
    ACanvas.Rectangle(BR);

    h := RectHeight(BR) - 2;

    yLevel := BR.Bottom - 1 - Round(h * EnsureRange(level,
                                                    0,
                                                    1));

    fillR := Rect(BR.Left + 1,
                  yLevel,
                  BR.Right - 1,
                  BR.Bottom - 1);

    // Main bar body
    ACanvas.Brush.Style := bsSolid;
    ACanvas.Brush.Color := FBarColor;
    ACanvas.Pen.Style := psClear;
    ACanvas.Rectangle(fillR);

    // Peak cap (red top segment) when level is near full-scale
    if (level >= FPeakThreshold) then
      begin

        varCapH := Round(h * EnsureRange(FPeakCapFrac,
                                         0,
                                         1));
        if (varCapH < 2) then
          varCapH := 2;

        capR := fillR;
        capR.Bottom := Min(fillR.Bottom,
                           fillR.Top + varCapH);

        ACanvas.Brush.Color := FPeakColor;
        ACanvas.Rectangle(capR);

        ACanvas.Brush.Color := FBarColor;
      end;

    ACanvas.Pen.Style := psSolid;

    if showRms then
      begin

        yRms := BR.Bottom - 1 - Round(h * EnsureRange(rms,
                                                      0,
                                                      1));
        ACanvas.Pen.Color := FRmsColor;

        ACanvas.MoveTo(BR.Left + 1,
                       yRms);

        ACanvas.LineTo(BR.Right - 1,
                       yRms);
      end;

    if showHold then
      begin

       yHold := BR.Bottom - 1 - Round(h * EnsureRange(hold,
                                                      0,
                                                      1));
        ACanvas.Pen.Color := clWhite;

        ACanvas.MoveTo(BR.Left + 1,
                       yHold);

        ACanvas.LineTo(BR.Right - 1,
                       yHold);
      end;
  end;

  procedure DrawClipLedSquare(const LedR: TRect;
                              const Active: Boolean;
                              const AlwaysLabel: Boolean);
  begin

    ACanvas.Pen.Style := psClear;
    ACanvas.Brush.Style := bsSolid;

    if Active then
      ACanvas.Brush.Color := clRed
    else
      ACanvas.Brush.Color := clMaroon;

    ACanvas.Rectangle(LedR);

    if AlwaysLabel then
      begin

        ACanvas.Brush.Style := bsClear;

        if Active then
          ACanvas.Font.Color := clWhite
        else
          ACanvas.Font.Color := clSilver;

        ACanvas.TextOut(LedR.Right + 4,
                        LedR.Top - 1, 'CLIP');
      end;
  end;

var
  L: TMfLevels;
  brL,
  brR: TRect;
  levL,
  levR,
  rmsL,
  rmsR,
  holdL,
  holdR: Single;

begin

  DbMinLocal := FDbMin;

  PlotR := R;

  if FShowDbScale then
    PlotR.Left := PlotR.Left + FDbScaleWidth;

  ScaleR := Rect(R.Left,
                 R.Top,
                 PlotR.Left,
                 R.Bottom);

  PlotArea := Rect(PlotR.Left,
                   R.Top + TOP_M,
                   PlotR.Right,
                   R.Bottom - BOT_M);

  if FShowDbScale then
    DrawDbScale(ScaleR,
                PlotArea);

  L := Levels;

  levL := AmpToFrac(EnsureRange(L.PeakL,
                                0,
                                1));
  levR := AmpToFrac(EnsureRange(L.PeakR,
                                0,
                                1));
  rmsL := AmpToFrac(EnsureRange(L.RmsL,
                                0,
                                1));
  rmsR := AmpToFrac(EnsureRange(L.RmsR,
                                0,
                                1));
  holdL := AmpToFrac(EnsureRange(FHoldL,
                                 0,
                                 1));
  holdR := AmpToFrac(EnsureRange(FHoldR,
                                 0,
                                 1));

  brL := BarRect(0,
                 2);

  brR := BarRect(1,
                 2);

  DrawBar(brL,
          levL,
          rmsL,
          holdL,
          FShowRms,
          FShowPeakHold);

  DrawBar(brR,
          levR,
          rmsR,
          holdR,
          FShowRms,
          FShowPeakHold);

  if FShowClipIndicator then
    begin

      // Latch deadlines are stored as tick counts (GetTickCount + hold).
      if (FClipIndicatorMode = cimPerChannel) then
        begin

          DrawClipLedSquare(Rect(brL.Right - 14,
                                 brL.Top + 2,
                                 brL.Right - 2,
                                 brL.Top + 14),
                                 GetTickCount < Cardinal(InterlockedCompareExchange(FClipUntilL,
                                                                                    0,
                                                                                    0)),
                                                                                    False);

          DrawClipLedSquare(Rect(brR.Right - 14,
                                 brR.Top + 2,
                                 brR.Right - 2,
                                 brR.Top + 14),
                                 GetTickCount < Cardinal(InterlockedCompareExchange(FClipUntilR,
                                                                                    0,
                                                                                    0)),
                                                                                    False);
        end
      else
        begin

          // Single global CLIP LED (always shows label).
          DrawClipLedSquare(Rect(PlotR.Right - 14,
                                 PlotR.Top + 2,
                                 PlotR.Right - 2,
                                 PlotR.Top + 14),
                                 GetTickCount < Cardinal(InterlockedCompareExchange(FClipUntilAny,
                                                                                    0,
                                                                                    0)),
                                                                                    True);
        end;
    end;

  // Header
  if FShowMeters then
    begin

      ACanvas.Brush.Style := bsClear;
      ACanvas.Font.Color := clSilver;
      ACanvas.TextOut(PlotR.Left + 8,
                      R.Top + 8,
                      Format('Meters  SR=%d  Ch=%d',
                             [FSampleRate,
                             FChannels]));
    end;

end;



procedure TMfAudioMixVisualizer.DrawSpectrum(ACanvas: TCanvas;
                                             const R: TRect);

const
  TOP_M = 8;
  BOT_M = 8;

var
  PlotR,
  ScaleR,
  PlotArea: TRect;
  DbMinLocal: Single;
  DbTopLocal: Single;

  procedure DrawDbScale(const ScaleR,
                        PlotArea: TRect);
  var
    db: Integer;
    axisX: Integer;
    y: Integer;
    tickLen: Integer;
    lbl: string;
    dbMinInt: Integer;
    dbTopInt: Integer;

  begin

    ACanvas.Brush.Style := bsClear;
    ACanvas.Font.Color := clSilver;
    ACanvas.Pen.Style := psSolid;
    ACanvas.Pen.Color := clGray;

    axisX := PlotArea.Left - 4;
    ACanvas.MoveTo(axisX,
                   PlotArea.Top);
    ACanvas.LineTo(axisX,
                   PlotArea.Bottom);

    dbMinInt := Floor(DbMinLocal);
    dbTopInt := Ceil(DbTopLocal);

    for db := dbTopInt downto dbMinInt do
      if (((Abs(db) mod FDbTickStep) = 0) or (db = dbTopInt)) then
        begin

          y := PlotArea.Bottom - Round(DbToDisplayFrac(db,
                                                       DbMinLocal,
                                                       DbTopLocal) * RectHeight(PlotArea));

          if (((Abs(db) mod FDbLabelStep) = 0) or (db = 0) or (db = dbTopInt)) then
            tickLen := 8
          else
            tickLen := 5;

          ACanvas.MoveTo(axisX - tickLen,
                         y);
          ACanvas.LineTo(axisX,
                         y);

          if (((Abs(db) mod FDbLabelStep) = 0) or (db = 0) or (db = dbTopInt)) then
            begin

              if (db > 0) then
                lbl := '+' + IntToStr(db)
              else
                lbl := IntToStr(db);

              ACanvas.TextOut(ScaleR.Left + 2,
                              y - (ACanvas.TextHeight('0') div 2),
                              lbl);
            end;
        end;
  end;

var
  S: TMfSpectrum;
  i,
  n: Integer;
  gap,
  bw,
  x,
  h,
  yTop: Integer;
  level: Single;
  BR: TRect;
  capR: TRect;
  capH: Integer;
  PeakThresholdFrac: Single;

begin

  DbMinLocal := FDbMin;
  DbTopLocal := FDbTop;

  S := Spectrum;
  n := Length(S);

  PlotR := R;

  if FShowDbScale then
    PlotR.Left := PlotR.Left + FDbScaleWidth;

  ScaleR := Rect(R.Left,
                 R.Top,
                 PlotR.Left,
                 R.Bottom);

  PlotArea := Rect(PlotR.Left,
                   R.Top + TOP_M,
                   PlotR.Right,
                   R.Bottom - BOT_M);

  if (n = 0) then
    begin
      ACanvas.Brush.Style := bsClear;
      ACanvas.Font.Color := clRed;
      ACanvas.TextOut(PlotR.Left + 8,
                      R.Top + 8,
                      'Spectrum: No data (Active = False?)');
      Exit;
    end;

  if FShowDbScale then
    DrawDbScale(ScaleR,
                PlotArea);

  gap := Max(1,
             RectWidth(PlotArea) div (n * 8));
  bw := (RectWidth(PlotArea) - gap * (n + 1)) div n;
  if (bw < 1) then
    bw := 1;

  h := RectHeight(PlotArea);
  x := PlotArea.Left + gap;

  PeakThresholdFrac := DbToDisplayFrac(20.0 * Log10(Max(FPeakThreshold,
                                                        1.0e-12)),
                                       DbMinLocal,
                                       DbTopLocal);

  ACanvas.Pen.Style := psClear;
  ACanvas.Brush.Style := bsSolid;
  ACanvas.Brush.Color := FBarColor;

  for i := 0 to n - 1 do
    begin
      level := EnsureRange(S[i],
                           0,
                           1);

      yTop := PlotArea.Bottom - Round(h * level);
      BR := Rect(x,
                 yTop,
                 x + bw,
                 PlotArea.Bottom);

      ACanvas.Rectangle(BR);

      if (level >= PeakThresholdFrac) then
        begin
          capH := Round(h * EnsureRange(FPeakCapFrac,
                                        0,
                                        1));
          if (capH < 2) then
            capH := 2;

          capR := BR;
          capR.Bottom := Min(BR.Bottom,
                             BR.Top + capH);

          ACanvas.Brush.Color := FPeakColor;
          ACanvas.Rectangle(capR);
          ACanvas.Brush.Color := FBarColor;
        end;

      x := x + bw + gap;
    end;

  if FShowMeters then
    begin

      ACanvas.Pen.Style := psSolid;
      ACanvas.Brush.Style := bsClear;
      ACanvas.Font.Color := clSilver;
      ACanvas.TextOut(PlotR.Left + 8,
                      R.Top + 8,
                      Format('Spectrum  Sample Rate=%d  FFT=%d',
                             [FSampleRate,
                              FFftSize]));
    end;
end;

end.