// FactoryX
//
// Copyright:  FactoryX. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: MfAudioVisualizer.pas
// Kind: Pascal Unit Component
// Release date: 24-01-2019
// Language: ENU
//
// Version: 3.2.0
// Description: Single visual component: WASAPI loopback + Peak/RMS + optional Spectrum bars.
//
// Company: FactoryX
// Intiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX), Peter Larson (ozships)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 01/04/2026 All                 Sineead O'Connor release  SDK 10.0.26100.4654 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: To install the visual components, choose Install in the Project Manager.
//          Requires Windows 7 or later.
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
unit MfAudioVisualizerEx;

interface

uses

  {WinApi}
  WinApi.Windows,
  WinApi.ActiveX,
  {System}
  System.SysUtils,
  System.Classes,
  System.Math,
  {CoreAudioApi}
  WinApi.CoreAudioApi.MMDeviceApi,
  Winapi.CoreAudioApi.AudioClient,
  WinApi.CoreAudioApi.AudioSessionTypes,
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
  TMfAudioVisualizer = class;

  TMfCaptureThread = class(TThread)
  private

    FOwner: TMfAudioVisualizer;
  protected

    procedure Execute; override;
  public

    // External feed mode (InputSource = isExternalFeed).
    procedure BeginExternalFormat(ASampleRate, AChannels: Integer; AIsFloat: Boolean);
    procedure PushFloat32Interleaved(pData: PSingle; Frames: Integer);
    procedure PushInt16Interleaved(pData: PSmallInt; Frames: Integer);

    constructor Create(AOwner: TMfAudioVisualizer);
  end;

  TMfSpectrum = TArray<Single>; // 0..1 per bar

  TMfAudioVisualizer = class(TGraphicControl)
  private
    // Published settings.
    FActive: Boolean;
    FAutoStart: Boolean;

    FInputSource: TMfAudioInputSource;

    FDataFlow: EDataFlow;
    FRole: ERole;

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

    // Peak cap settings.
    FPeakThreshold: Single; // 0..1 (e.g. 0.98)
    FPeakCapFrac: Single;   // 0..1 fraction of bar height (e.g. 0.12)

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

    procedure TimerTick(Sender: TObject);

    procedure SetActive(Value: Boolean);
    procedure SetAutoStart(Value: Boolean);
    procedure SetInputSource(Value: TMfAudioInputSource);

    procedure SetDeviceDataFlow(Value: EDataFlow);
    procedure SetDeviceRole(Value: ERole);

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
    procedure SetPeakThreshold(Value: Single);
    procedure SetPeakCapFrac(Value: Single);

    procedure EnsureSpectrumStorage();
    procedure StartCapture();
    procedure StopCapture();
    procedure RestartIfRunning();

    function GetLevels(): TMfLevels;
    function GetSpectrum(): TMfSpectrum;

    procedure DrawMeters(ACanvas: TCanvas; const R: TRect);
    procedure DrawSpectrum(ACanvas: TCanvas; const R: TRect);
    procedure DrawDesignTimePlaceholder(ACanvas: TCanvas; const R: TRect);

  protected

    procedure Loaded(); override;
    procedure Paint(); override;

  public

    // External feed mode (InputSource = isExternalFeed).
    procedure BeginExternalFormat(ASampleRate, AChannels: Integer; AIsFloat: Boolean);
    procedure PushFloat32Interleaved(pData: PSingle; Frames: Integer);
    procedure PushInt16Interleaved(pData: PSmallInt; Frames: Integer);

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property Levels: TMfLevels read GetLevels;
    property Spectrum: TMfSpectrum read GetSpectrum;

    property SampleRate: Integer read FSampleRate;
    property Channels: Integer read FChannels;

  published

    property Align;
    property Anchors;

    property BackColor: TColor read FBackColor write SetBackColor default clBlack;

    // Colors
    property BarColor: TColor read FBarColor write SetBarColor default clAqua;
    property PeakColor: TColor read FPeakColor write SetPeakColor default clRed;
    property RmsColor: TColor read FRmsColor write SetRmsColor default clYellow;
    property BorderColor: TColor read FBorderColor write SetBorderColor default clGray;

    // Peak highlight
    property PeakThreshold: Single read FPeakThreshold write SetPeakThreshold;
    property PeakCapFrac: Single read FPeakCapFrac write SetPeakCapFrac;

    property AutoStart: Boolean read FAutoStart write SetAutoStart default False;
    property Active: Boolean read FActive write SetActive default False;

    property InputSource: TMfAudioInputSource read FInputSource write SetInputSource default isLoopback;

    property DeviceDataFlow: EDataFlow read FDataFlow write SetDeviceDataFlow default eRender;
    property DeviceRole: ERole read FRole write SetDeviceRole default eMultimedia;

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


procedure Register;
begin

  RegisterComponents('MfPack Core Audio Samples',
                     [TMfAudioVisualizer]);
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
      X[i] := X[i] * Single(w);
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

        Swap(Re[i], Re[j]);
        Swap(Im[i], Im[j]);
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

              Re[i] := Single(ur + tr);
              Im[i] := Single(ui + ti);
              Re[i + m2] := Single(ur - tr);
              Im[i + m2] := Single(ui - ti);

              i := i + m;
            end;
        end;

      m := m shl 1;
    end;
end;


constructor TMfCaptureThread.Create(AOwner: TMfAudioVisualizer);
begin

  inherited Create(False);

  FreeOnTerminate := False;
  FOwner := AOwner;
end;


procedure TMfCaptureThread.Execute();
const
  EPS = 1e-12;

var
  hr: HRESULT;
  Enumerator: IMMDeviceEnumerator;
  Device: IMMDevice;
  AudioClient: IAudioClient;
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

    dt := 1.0 / Max(1, sampleRate);
    aa := Exp(-dt / (Max(1,
                         FOwner.FAttackMs) / 1000.0));
    rr := Exp(-dt / (Max(1,
                         FOwner.FReleaseMs) / 1000.0));
    attackA := Single(aa);
    releaseA := Single(rr);
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

  procedure PublishLevels(const pL,
                          pR,
                          rL, rR: Single);
  var
    peakDbL,
    peakDbR,
    rmsDbL,
    rmsDbR: Single;

  begin

    peakDbL := 20 * Log10(Max(EPS,
                              pL));
    peakDbR := 20 * Log10(Max(EPS,
                              pR));
    rmsDbL  := 20 * Log10(Max(EPS,
                              rL));
    rmsDbR  := 20 * Log10(Max(EPS,
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
  end;

  procedure PublishSpectrum(const BarsLocal: TArray<Single>);
  var
    ii,
    fi: Integer;
  begin

    fi := Min(Length(BarsLocal),
              Length(FOwner.FSpectrumBits));
    for ii := 0 to fi - 1 do
      InterlockedExchange(FOwner.FSpectrumBits[ii],
                          SingleToBits(BarsLocal[ii]));
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

                            a := pf^; Inc(pf);
                            sumSqL := sumSqL + (a * a);

                            if (Abs(a) > peakL) then
                              peakL := Abs(a);

                            if (channels > 1) then
                              begin

                                b := pf^; Inc(pf);
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

                            a := ps^ / 32768.0; Inc(ps);
                            sumSqL := sumSqL + (a * a);
                            if (Abs(a) > peakL) then
                              peakL := Abs(a);

                            if (channels > 1) then
                              begin

                                b := ps^ / 32768.0;
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

                CaptureClient.ReleaseBuffer(NumFrames);

                rmsL := Single(Sqrt(sumSqL / Max(1,
                                                 nSamp)));
                rmsR := Single(Sqrt(sumSqR / Max(1,
                                                 nSamp)));

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

                    PublishLevels(smPeakL,
                                  smPeakR,
                                  smRmsL,
                                  smRmsR);

                    if (FOwner.FMode = amLevelsAndSpectrum) and (Length(FOwner.FMonoRing) > 0) then
                      begin

                        ReadRingLatest(monoBlock);

                        for i := 0 to fftN - 1 do
                          begin

                            re[i] := monoBlock[i];
                            im[i] := 0;
                          end;

                        TSimpleFFT.HannWindow(re);
                        TSimpleFFT.FFT(re,
                                       im);

                        maxMag := 1e-9;
                        for k := 1 to (fftN div 2) - 1 do
                          begin

                            v := Sqrt(re[k]*re[k] + im[k]*im[k]);
                            mags[k] := v;
                            if (v > maxMag) then
                              maxMag := v;
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

                            acc := 0;
                            cnt := 0;

                            for k := idxStart to idxEnd do
                              begin

                                acc := acc + mags[k];
                                Inc(cnt);
                              end;

                            if (cnt > 0) then
                              acc := acc / cnt;

                            acc := acc / maxMag;
                            acc := Sqrt(Max(0,
                                            Min(1,
                                                acc)));
                            bars[bar] := acc;
                          end;

                        PublishSpectrum(bars);
                      end;
                  end;
              end;
          end;
        else  // case
          Break;
        end;
      end;

    AudioClient.Stop();
    CloseHandle(EventHandle);
    CoTaskMemFree(MixFmt);
  finally
    CoUninitialize;
  end;
end;


{ TMfAudioVisualizer }

constructor TMfAudioVisualizer.Create(AOwner: TComponent);
begin

  inherited Create(AOwner);

  // IMPORTANT: never allocate timers/bitmaps or handles in the constructor,
  // because IDE palette may instantiate this at install time.
  FBackColor := clBlack;

  FAutoStart := False;
  FActive := False;

  FInputSource := isLoopback;

  FDataFlow := eRender;
  FRole := eMultimedia;

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


destructor TMfAudioVisualizer.Destroy();
begin

  Active := False;

  if Assigned(FTimer) then
    begi
procedure TMfAudioVisualizer.BeginExternalFormat(ASampleRate, AChannels: Integer; AIsFloat: Boolean);
begin
  if (csDesigning in ComponentState) then
    Exit;

  if (FInputSource <> isExternalFeed) then
    Exit;

  FSampleRate := ASampleRate;
  FChannels := AChannels;
  FIsFloat := AIsFloat;

  // reset smoothing coeffs to force recompute on next push
  FExtAttackA := 0;
  FExtReleaseA := 0;
  FExtLastDispatchTick := GetTickCount();

  EnsureSpectrumStorage();
end;


procedure TMfAudioVisualizer.PushFloat32Interleaved(pData: PSingle; Frames: Integer);
const
  EPS = 1e-12;
var
  i: Integer;
  a, b: Single;
  peakL, peakR: Single;
  sumSqL, sumSqR: Double;
  rmsL, rmsR: Single;
  aAtk, aRel: Single;
  nowTick: DWORD;

  // spectrum locals
  fftN, barCount: Integer;
  monoBlock: TArray<Single>;
  re, im: TArray<Single>;
  mags: TArray<Single>;
  bars: TArray<Single>;
  k, bar: Integer;
  idxStart, idxEnd: Integer;
  maxMag, v: Single;
  t0, t1: Double;
  acc: Single;
  cnt: Integer;

  // mono push
  tmpMono: TArray<Single>;
  needSpectrum: Boolean;

  function Smooth(prev, target, aAtkLocal, aRelLocal: Single): Single;
  begin
    if target > prev then
      Result := target + (prev - target) * aAtkLocal
    else
      Result := target + (prev - target) * aRelLocal;
  end;

  procedure ComputeAttackRelease();
  var
    dt: Double;
    aa, rr: Double;
  begin
    if FSampleRate <= 0 then
      Exit;

    dt := 1.0 / Max(1, FSampleRate);
    aa := Exp(-dt / (Max(1, FAttackMs) / 1000.0));
    rr := Exp(-dt / (Max(1, FReleaseMs) / 1000.0));
    FExtAttackA := Single(aa);
    FExtReleaseA := Single(rr);
  end;

  function ShouldDispatch(): Boolean;
  begin
    nowTick := GetTickCount();
    if (nowTick - FExtLastDispatchTick) >= DWORD(Max(1, FDispatchEveryMs)) then
    begin
      FExtLastDispatchTick := nowTick;
      Result := True;
    end
    else
      Result := False;
  end;

  procedure PushMonoToRing(const X: TArray<Single>);
  var
    j, N: Integer;
  begin
    N := Length(X);
    for j := 0 to N - 1 do
    begin
      FMonoRing[FRingWrite] := X[j];
      Inc(FRingWrite);
      if FRingWrite >= Length(FMonoRing) then
        FRingWrite := 0;

      if FRingCount < Length(FMonoRing) then
        Inc(FRingCount);
    end;
  end;

  procedure ReadRingLatest(var OutBlock: TArray<Single>);
  var
    j, N, start: Integer;
  begin
    N := Length(OutBlock);
    if N = 0 then Exit;

    if FRingCount < N then
    begin
      FillChar(OutBlock[0], N * SizeOf(Single), 0);
      Exit;
    end;

    start := FRingWrite - N;
    if start < 0 then
      start := start + Length(FMonoRing);

    for j := 0 to N - 1 do
    begin
      OutBlock[j] := FMonoRing[start];
      Inc(start);
      if start >= Length(FMonoRing) then
        start := 0;
    end;
  end;

  procedure PublishLevels(const pL, pR, rL, rR: Single);
  var
    peakDbL, peakDbR, rmsDbL, rmsDbR: Single;
  begin
    peakDbL := 20 * Log10(Max(EPS, pL));
    peakDbR := 20 * Log10(Max(EPS, pR));
    rmsDbL  := 20 * Log10(Max(EPS, rL));
    rmsDbR  := 20 * Log10(Max(EPS, rR));

    InterlockedExchange(FLevelsBits[0], SingleToBits(pL));
    InterlockedExchange(FLevelsBits[1], SingleToBits(pR));
    InterlockedExchange(FLevelsBits[2], SingleToBits(rL));
    InterlockedExchange(FLevelsBits[3], SingleToBits(rR));
    InterlockedExchange(FLevelsBits[4], SingleToBits(peakDbL));
    InterlockedExchange(FLevelsBits[5], SingleToBits(peakDbR));
    InterlockedExchange(FLevelsBits[6], SingleToBits(rmsDbL));
    InterlockedExchange(FLevelsBits[7], SingleToBits(rmsDbR));
  end;

  procedure PublishSpectrum(const BarsLocal: TArray<Single>);
  var
    ii, fi: Integer;
  begin
    fi := Min(Length(BarsLocal), Length(FSpectrumBits));
    for ii := 0 to fi - 1 do
      InterlockedExchange(FSpectrumBits[ii], SingleToBits(BarsLocal[ii]));
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

  if FSampleRate <= 0 then
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
    a := pData^;
    Inc(pData);

    if FChannels > 1 then
    begin
      b := pData^;
      Inc(pData);
    end
    else
      b := a;

    sumSqL := sumSqL + (a * a);
    if Abs(a) > peakL then peakL := Abs(a);

    sumSqR := sumSqR + (b * b);
    if Abs(b) > peakR then peakR := Abs(b);

    if needSpectrum then
      tmpMono[i] := 0.5 * (a + b);

    if FChannels > 2 then
      Inc(pData, FChannels - 2);
  end;

  rmsL := Single(Sqrt(sumSqL / Max(1, Frames)));
  rmsR := Single(Sqrt(sumSqR / Max(1, Frames)));

  if (FExtAttackA = 0) or (FExtReleaseA = 0) then
    ComputeAttackRelease();

  aAtk := Power(FExtAttackA, Frames);
  aRel := Power(FExtReleaseA, Frames);

  FSmPeakL := Smooth(FSmPeakL, peakL, aAtk, aRel);
  FSmPeakR := Smooth(FSmPeakR, peakR, aAtk, aRel);
  FSmRmsL  := Smooth(FSmRmsL,  rmsL,  aAtk, aRel);
  FSmRmsR  := Smooth(FSmRmsR,  rmsR,  aAtk, aRel);

  if needSpectrum then
    PushMonoToRing(tmpMono);

  if ShouldDispatch() then
  begin
    PublishLevels(FSmPeakL, FSmPeakR, FSmRmsL, FSmRmsR);

    if needSpectrum then
    begin
      fftN := FFftSize;
      barCount := FBarCount;

      SetLength(monoBlock, fftN);
      SetLength(re, fftN);
      SetLength(im, fftN);
      SetLength(mags, fftN div 2);
      SetLength(bars, barCount);

      ReadRingLatest(monoBlock);

      for i := 0 to fftN - 1 do
      begin
        re[i] := monoBlock[i];
        im[i] := 0;
      end;

      TSimpleFFT.HannWindow(re);
      TSimpleFFT.FFT(re, im);

      maxMag := 1e-9;
      for k := 1 to (fftN div 2) - 1 do
      begin
        v := Sqrt(re[k] * re[k] + im[k] * im[k]);
        mags[k] := v;
        if v > maxMag then maxMag := v;
      end;

      for bar := 0 to barCount - 1 do
      begin
        t0 := bar / barCount;
        t1 := (bar + 1) / barCount;

        idxStart := 1 + Trunc((Power(fftN div 2, t0) - 1));
        idxEnd   := 1 + Trunc((Power(fftN div 2, t1) - 1));

        if idxEnd <= idxStart then
          idxEnd := idxStart + 1;

        if idxEnd >= (fftN div 2) then
          idxEnd := (fftN div 2) - 1;

        acc := 0;
        cnt := 0;

        for k := idxStart to idxEnd do
        begin
          acc := acc + mags[k];
          Inc(cnt);
        end;

        if cnt > 0 then
          acc := acc / cnt;

        acc := acc / maxMag;
        acc := Sqrt(Max(0, Min(1, acc)));

        bars[bar] := acc;
      end;

      PublishSpectrum(bars);
    end;
  end;
end;


procedure TMfAudioVisualizer.PushInt16Interleaved(pData: PSmallInt; Frames: Integer);
var
  i: Integer;
  a16, b16: SmallInt;
  a, b: Single;
  peakL, peakR: Single;
  sumSqL, sumSqR: Double;
  rmsL, rmsR: Single;
  aAtk, aRel: Single;
  nowTick: DWORD;

  // spectrum
  tmpMono: TArray<Single>;
  needSpectrum: Boolean;

  function Smooth(prev, target, aAtkLocal, aRelLocal: Single): Single;
  begin
    if target > prev then
      Result := target + (prev - target) * aAtkLocal
    else
      Result := target + (prev - target) * aRelLocal;
  end;

  procedure ComputeAttackRelease();
  var
    dt: Double;
    aa, rr: Double;
  begin
    if FSampleRate <= 0 then
      Exit;

    dt := 1.0 / Max(1, FSampleRate);
    aa := Exp(-dt / (Max(1, FAttackMs) / 1000.0));
    rr := Exp(-dt / (Max(1, FReleaseMs) / 1000.0));
    FExtAttackA := Single(aa);
    FExtReleaseA := Single(rr);
  end;

  function ShouldDispatch(): Boolean;
  begin
    nowTick := GetTickCount();
    if (nowTick - FExtLastDispatchTick) >= DWORD(Max(1, FDispatchEveryMs)) then
    begin
      FExtLastDispatchTick := nowTick;
      Result := True;
    end
    else
      Result := False;
  end;

  procedure PublishLevelsOnly(const pL, pR, rL, rR: Single);
  const
    EPS = 1e-12;
  var
    peakDbL, peakDbR, rmsDbL, rmsDbR: Single;
  begin
    peakDbL := 20 * Log10(Max(EPS, pL));
    peakDbR := 20 * Log10(Max(EPS, pR));
    rmsDbL  := 20 * Log10(Max(EPS, rL));
    rmsDbR  := 20 * Log10(Max(EPS, rR));

    InterlockedExchange(FLevelsBits[0], SingleToBits(pL));
    InterlockedExchange(FLevelsBits[1], SingleToBits(pR));
    InterlockedExchange(FLevelsBits[2], SingleToBits(rL));
    InterlockedExchange(FLevelsBits[3], SingleToBits(rR));
    InterlockedExchange(FLevelsBits[4], SingleToBits(peakDbL));
    InterlockedExchange(FLevelsBits[5], SingleToBits(peakDbR));
    InterlockedExchange(FLevelsBits[6], SingleToBits(rmsDbL));
    InterlockedExchange(FLevelsBits[7], SingleToBits(rmsDbR));
  end;

  procedure PushMonoToRing(const X: TArray<Single>);
  var
    j, N: Integer;
  begin
    N := Length(X);
    for j := 0 to N - 1 do
    begin
      FMonoRing[FRingWrite] := X[j];
      Inc(FRingWrite);
      if FRingWrite >= Length(FMonoRing) then
        FRingWrite := 0;

      if FRingCount < Length(FMonoRing) then
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

  if FSampleRate <= 0 then
    Exit;

  needSpectrum := (FMode = amLevelsAndSpectrum) and (Length(FMonoRing) > 0);
  if needSpectrum then
    SetLength(tmpMono, Frames);

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
    if Abs(a) > peakL then peakL := Abs(a);

    sumSqR := sumSqR + (b * b);
    if Abs(b) > peakR then peakR := Abs(b);

    if needSpectrum then
      tmpMono[i] := 0.5 * (a + b);

    if FChannels > 2 then
      Inc(pData, FChannels - 2);
  end;

  rmsL := Single(Sqrt(sumSqL / Max(1, Frames)));
  rmsR := Single(Sqrt(sumSqR / Max(1, Frames)));

  if (FExtAttackA = 0) or (FExtReleaseA = 0) then
    ComputeAttackRelease();

  aAtk := Power(FExtAttackA, Frames);
  aRel := Power(FExtReleaseA, Frames);

  FSmPeakL := Smooth(FSmPeakL, peakL, aAtk, aRel);
  FSmPeakR := Smooth(FSmPeakR, peakR, aAtk, aRel);
  FSmRmsL  := Smooth(FSmRmsL,  rmsL,  aAtk, aRel);
  FSmRmsR  := Smooth(FSmRmsR,  rmsR,  aAtk, aRel);

  if needSpectrum then
    PushMonoToRing(tmpMono);

  if ShouldDispatch() then
    PublishLevelsOnly(FSmPeakL, FSmPeakR, FSmRmsL, FSmRmsR);
end;

n

      FTimer.Enabled := False;
      FTimer.OnTimer := nil;
    end;

  FreeAndNil(FBack);

  if (FStopEvent <> 0) then
    CloseHandle(FStopEvent);

  inherited Destroy;
end;


procedure TMfAudioVisualizer.Loaded();
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


procedure TMfAudioVisualizer.TimerTick(Sender: TObject);
begin

  if (csDesigning in ComponentState) then
    Exit;
  Invalidate();
end;


procedure TMfAudioVisualizer.SetBackColor(Value: TColor);
begin

  if (FBackColor = Value) then
    Exit;
  FBackColor := Value;
  Invalidate;
end;


procedure TMfAudioVisualizer.SetBarColor(Value: TColor);
begin

  if (FBarColor = Value) then
    Exit;

  FBarColor := Value;
  Invalidate;
end;


procedure TMfAudioVisualizer.SetPeakColor(Value: TColor);
begin

  if (FPeakColor = Value) then
    Exit;

  FPeakColor := Value;
  Invalidate;
end;


procedure TMfAudioVisualizer.SetRmsColor(Value: TColor);
begin

  if (FRmsColor = Value) then
    Exit;

  FRmsColor := Value;
  Invalidate;
end;


procedure TMfAudioVisualizer.SetBorderColor(Value: TColor);
begin

  if (FBorderColor = Value) then
    Exit;

  FBorderColor := Value;
  Invalidate;
end;


procedure TMfAudioVisualizer.SetPeakThreshold(Value: Single);
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


procedure TMfAudioVisualizer.SetPeakCapFrac(Value: Single);
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


procedure TMfAudioVisualizer.DrawDesignTimePlaceholder(ACanvas: TCanvas;
                                                       const R: TRect);
begin

  ACanvas.Brush.Style := bsSolid;
  ACanvas.Brush.Color := FBackColor;
  ACanvas.FillRect(R);

  ACanvas.Brush.Style := bsClear;
  ACanvas.Font.Color := clGrayText;
  ACanvas.TextOut(R.Left + 8,
                  R.Top + 8, 'TMfAudioVisualizer');
  ACanvas.TextOut(R.Left + 8,
                  R.Top + 28, 'Design-time: no capture/timers');
end;


procedure TMfAudioVisualizer.Paint();
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


procedure TMfAudioVisualizer.EnsureSpectrumStorage();
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


procedure TMfAudioVisualizer.StartCapture();
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

  FCaptureThread := TMfCaptureThread.Create(Self);
  FCaptureThread.Priority := tpHighest;
end;


procedure TMfAudioVisualizer.StopCapture();
begin

  if Assigned(FCaptureThread) then
    begin

      if (FStopEvent <> 0) then
        SetEvent(FStopEvent);

      FCaptureThread.Terminate();
      FCaptureThread.WaitFor();
      FreeAndNil(FCaptureThread);
  end;
end;


procedure TMfAudioVisualizer.RestartIfRunning();
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
    StopCapture;
  end;
end;


procedure TMfAudioVisualizer.SetActive(Value: Boolean);
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
    StopCapture;
    Exit;
  end;

  if (FInputSource = isLoopback) then
    StartCapture
  else
    StopCapture; // ensure any previous loopback thread is stopped
end;


procedure TMfAudioVisualizer.SetAutoStart(Value: Boolean);
begin

  if (FAutoStart = Value) then
    Exit;
  FAutoStart := Value;
end;


procedure TMfAudioVisualizer.SetInputSource(Value: TMfAudioInputSource);
begin

  if (FInputSource = Value) then
    Exit;

  // Switching source may require stopping loopback thread.
  FInputSource := Value;

  // If running, restart to apply new source selection.
  RestartIfRunning;
end;


procedure TMfAudioVisualizer.SetDeviceDataFlow(Value: EDataFlow);
begin

  if (FDataFlow = Value) then
    Exit;

  FDataFlow := Value;
  RestartIfRunning;
end;


procedure TMfAudioVisualizer.SetDeviceRole(Value: ERole);
begin

  if (FRole = Value) then
    Exit;

  FRole := Value;
  RestartIfRunning;
end;


procedure TMfAudioVisualizer.SetMode(Value: TMfAnalyzerMode);
begin

  if (FMode = Value) then
    Exit;

  FMode := Value;
  if not (csDesigning in ComponentState) then
    EnsureSpectrumStorage;

  RestartIfRunning();
  Invalidate();
end;


procedure TMfAudioVisualizer.SetView(Value: TMfVizMode);
begin

  if (FView = Value) then
    Exit;

  FView := Value;
  Invalidate;
end;


procedure TMfAudioVisualizer.SetBarCount(Value: Integer);
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


procedure TMfAudioVisualizer.SetFftSize(Value: Integer);
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


procedure TMfAudioVisualizer.SetAttackMs(Value: Integer);
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


procedure TMfAudioVisualizer.SetReleaseMs(Value: Integer);
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


procedure TMfAudioVisualizer.SetDispatchEveryMs(Value: Integer);
begin

  Value := EnsureRange(Value,
                       10,
                       200);

  if (FDispatchEveryMs = Value) then
    Exit;
  FDispatchEveryMs := Value;
end;


procedure TMfAudioVisualizer.SetFps(Value: Integer);
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


function TMfAudioVisualizer.GetLevels(): TMfLevels;
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
end;


function TMfAudioVisualizer.GetSpectrum(): TMfSpectrum;
var
  i: Integer;

begin

  SetLength(Result,
            Length(FSpectrumBits));

  for i := 0 to High(FSpectrumBits) do
    Result[i] := BitsToSingle(InterlockedCompareExchange(FSpectrumBits[i],
                              0,
                              0));
end;


procedure TMfAudioVisualizer.DrawMeters(ACanvas: TCanvas;
                                        const R: TRect);

  function BarRect(const idx,
                   total: Integer): TRect;
  var
    w,
    gap,
    bw,
    x0: Integer;

  begin

    w := RectWidth(R);
    gap := Max(4,
               w div 80);

    bw := (w - gap * (total + 1)) div total;

    if (bw < 10) then
      bw := 10;
    x0 := R.Left + gap + idx * (bw + gap);
    Result := Rect(x0,
                   R.Top + 8,
                   x0 + bw,
                   R.Bottom - 8);
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
        varCapH := Round(h * EnsureRange(FPeakCapFrac, 0, 1));
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
      ACanvas.MoveTo(BR.Left + 2,
                     yRms);
      ACanvas.LineTo(BR.Right - 2,
                     yRms);
      end;

    if showHold then
      begin

        yHold := BR.Bottom - 1 - Round(h * EnsureRange(hold,
                                                       0,
                                                       1));
        ACanvas.Pen.Color := clWhite;

        ACanvas.MoveTo(BR.Left + 2,
                       yHold);

        ACanvas.LineTo(BR.Right - 2,
                       yHold);
      end;
  end;

var
  L: TMfLevels;
  nowTick: DWORD;
  holdAge: Integer;
  barL,
  barR: TRect;

begin

  L := Levels;
  nowTick := GetTickCount();

  if (L.PeakL >= FHoldL) then
    begin

      FHoldL := L.PeakL;
      FHoldTickL := nowTick;
    end
  else
    begin

      holdAge := Integer(nowTick - FHoldTickL);
      if (holdAge > FPeakHoldMs) then
        FHoldL := L.PeakL;
  end;

  if (L.PeakR >= FHoldR) then
    begin

      FHoldR := L.PeakR;
      FHoldTickR := nowTick;
    end
  else
    begin

      holdAge := Integer(nowTick - FHoldTickR);
      if (holdAge > FPeakHoldMs) then
        FHoldR := L.PeakR;
    end;

  barL := BarRect(0,
                  2);

  barR := BarRect(1,
                  2);

  DrawBar(barL,
          L.PeakL,
          L.RmsL,
          FHoldL,
          FShowRms,
          FShowPeakHold);

  if (FChannels > 1) then
    DrawBar(barR,
            L.PeakR,
            L.RmsR,
            FHoldR,
            FShowRms,
            FShowPeakHold)
  else
    DrawBar(barR,
            L.PeakL,
            L.RmsL,
            FHoldL,
            FShowRms,
            FShowPeakHold);

  ACanvas.Brush.Style := bsClear;
  ACanvas.Font.Color := clSilver;

  ACanvas.TextOut(R.Left + 8,
                  R.Top + 8,
    Format('Peak %.1f dB  RMS %.1f dB',
           [L.PeakDbL, L.RmsDbL]));
end;


procedure TMfAudioVisualizer.DrawSpectrum(ACanvas: TCanvas;
                                          const R: TRect);
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

begin

  S := Spectrum;
  n := Length(S);

  if (n = 0) then
    begin
      ACanvas.Brush.Style := bsClear;
      ACanvas.Font.Color := clGrayText;
      ACanvas.TextOut(R.Left + 8,
                      R.Top + 8, 'Spectrum: no data (Active = False?)');
      Exit;
    end;

  gap := Max(1,
             RectWidth(R) div (n * 8));
  bw := (RectWidth(R) - gap * (n + 1)) div n;
  if (bw < 1) then
    bw := 1;

  h := RectHeight(R) - 16;
  x := R.Left + gap;

  ACanvas.Pen.Style := psClear;
  ACanvas.Brush.Style := bsSolid;
  ACanvas.Brush.Color := FBarColor;

  for i := 0 to n - 1 do
    begin

      level := EnsureRange(S[i],
                           0,
                           1);

    yTop := (R.Bottom - 8) - Round(h * level);
    BR := Rect(x,
               yTop,
               x + bw,
               R.Bottom - 8);

    ACanvas.Rectangle(BR);

      // Peak cap.
      if (level >= FPeakThreshold) then
        begin

          capH := Round(h * EnsureRange(FPeakCapFrac, 0, 1));
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

  ACanvas.Pen.Style := psSolid;
  ACanvas.Brush.Style := bsClear;
  ACanvas.Font.Color := clSilver;
  ACanvas.TextOut(R.Left + 8,
                  R.Top + 8,
  Format('Spectrum  SR=%d  FFT=%d  Bars=%d',
         [FSampleRate, FFftSize, n]));
end;


end.

