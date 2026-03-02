// FactoryX
//
// Copyright:  FactoryX. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: MfPeakMeterMmcs.pas
// Kind: Pascal Unit Component
// Release date: 04-08-2016
// Language: ENU
//
// Version: 3.1.9
// Description: An extended Peakmeter component based on the
//              MfPeakmeterEx Sample with MMCS support.
//
// Company: FactoryX
// Intiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX), Carmen (Carmenh)

//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 01/04/2026 All                 Sineead O'Connor release  SDK 10.0.26100.4654 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: To install the visual components, choose Install in the Project Manager.
//          Requires Windows 10 or later.
//
// Related objects: -
// Related projects: MfPackX319
// Known Issues: -
//
// Compiler version: 23 up to 35               q
// SDK version: 10.0.26100.4654
//
// Todo: -
//
// =============================================================================
// Source: Parts of Peakmeter example from MSDN.
//
// Copyright (c) Microsoft Corporation. All rights reserved.
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
unit MfPeakMeterMmcs;

interface

uses

  {WinApi}
  WinApi.Windows,
  WinApi.Messages,
  WinApi.WinApiTypes,
  WinApi.ComBaseApi,
  {System}
  System.SysUtils,
  System.Math,
  System.Classes,
  System.Win.ComObj,
  System.Services.Avrt, // Multimedia class scheduler service (mmcss)
  {VCL}
  VCL.Graphics,
  VCL.Controls,
  VCL.Forms,
  VCL.ExtCtrls,
  {ActiveX}
  WinApi.ActiveX.ObjBase,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfApi,
  WinApi.MediaFoundationApi.MfUtils,
  {WinMM}
  WinApi.WinMM.MMeApi,
  WinApi.WinMM.MMReg,
  {CoreAudioApi}
  WinApi.CoreAudioApi.MMDeviceApi,
  WinApi.CoreAudioApi.EndPointVolume,
  WASAPIEngine;

const
  // Timer period (in milliseconds)
  TIMER_PERIOD = 30;

const
  // Internal tick message (posted to the component window handle).
  WM_MFPEAKMETEREX_TICK = WM_USER + $5A1;


type


  TLightMmcssTimer = class;

  TLightMmcssTimerThread = class(TThread)
  private

    FOwner: TLightMmcssTimer;
  protected

    procedure Execute(); override;
  public

    constructor Create(AOwner: TLightMmcssTimer);
  end;

  // Lightweight periodic timer that posts WM_MFPEAKMETEREX_TICK to a target window.
  // Runs on its own thread and registers that thread with MMCSS (Pro Audio by default).
  TLightMmcssTimer = class(TComponent)
  private

    FEnabled: Boolean;
    FDueTime: Cardinal;
    FPeriod: Cardinal;
    FTargetHwnd: HWND;
    FThread: TLightMmcssTimerThread;
    FStopEvent: THandle;
    FMmcssTaskName: UnicodeString;
    FMmcssPriority: Integer;

    procedure SetEnabled(const Value: Boolean);
    procedure SetDueTime(const Value: Cardinal);
    procedure SetPeriod(const Value: Cardinal);
    procedure Start;
    procedure Stop;

  public

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Reset; // restart with current settings
    property TargetHwnd: HWND read FTargetHwnd write FTargetHwnd;

  published

    property Enabled: Boolean read FEnabled write SetEnabled default False;
    property DueTime: Cardinal read FDueTime write SetDueTime default 0;
    property Period: Cardinal read FPeriod write SetPeriod default TIMER_PERIOD;
    // MMCSS task name, e.g. 'Pro Audio', 'Audio', 'Playback'. Default: 'Pro Audio'.
    property MmcssTaskName: UnicodeString read FMmcssTaskName write FMmcssTaskName;
    // MMCSS priority: -2..+2 (maps to AVRT_PRIORITY_*). Default: 1 (High).
    property MmcssPriority: Integer read FMmcssPriority write FMmcssPriority default 1;
  end;


type

  TMfPeakMeterExStyle = (dsVertical,
                         dsHorizontal);

  TMfPeakMeterExDirection = (ddRightDown,
                             ddLeftUp);

  TMfPeakMeterExChannel = (mcLeft,
                           mcRight);

  // Input source for the meter.
  // - isWasapiEndpoint: uses IAudioMeterInformation from the default endpoint.
  // - isWasapiEngine:   external feed via PushPcm (per-engine metering).
  TMfPeakMeterInputSource = (isWasapiEndpoint,
                             isWasapiEngine);

  // Engine-fed level mode.
  // - elmPeak: use block peak (0..1)
  // - elmRms:  use block RMS (VU-like, more movement)
  TMfPeakMeterEngineLevelMode = (elmPeak,
                                 elmRms);

  TMfPeakMeterMmcs = class(TGraphicControl)
  private
    { private fields }

    fHwnd: HWnd; // Handle to this meter.

    Fbevelstyle: Tpanelbevel;
    Fbevelwidth: Byte;

    fGreenLeds: Integer;
    fYellowLeds: Integer;
    fRedLeds: Integer;
    fGreenMax: Integer;
    fYellowMax: Integer;
    fRedMax: Integer;

    fColors: array [1..3,
                    False..True] of TColor;

    fShowSingleLed: Boolean;
    fSeparatorSpacing: Integer;
    fSeparatorColor: TColor;
    fStyle: TMfPeakMeterExStyle;
    fDirection: TMfPeakMeterExDirection;

    fDataFlow: EDataFlow;
    fRole: ERole;

    fBmp: TBitmap;

    //
    pEnumerator: IMMDeviceEnumerator;
    pDevice: IMMDevice;
    pMeterInfo: IAudioMeterInformation;

    // Peak values
    sPeakValue: Single;
    iPeakValue: Integer;
    afPeakValues: array of Float;

    FTimer: TLightMmcssTimer;
    FSafeTimerInterval: Cardinal;      // Timer interval
    FPeakMeterRunning: Boolean;
    FChannelCount: UINT;
    FSampleAllChannels: Boolean;
    FMeterChannel: TMfPeakMeterExChannel;
    FEnabled: Boolean;

    // Input source (endpoint vs engine-fed).
    FInputSource: TMfPeakMeterInputSource;
    FWasApiEngine: TWasApiEngine              ;

    // External (engine-fed) peak values (thread-safe via InterlockedExchange)
    FExtPeakBitsL: LongInt;
    FExtPeakBitsR: LongInt;

    // External (engine-fed) RMS values (thread-safe via InterlockedExchange)
    FExtRmsBitsL: LongInt;
    FExtRmsBitsR: LongInt;

    // Which external metric to use for drawing
    FEngineLevelMode: TMfPeakMeterEngineLevelMode;
    FExtChannels: Integer;
    FExtSmoothValue: Single;
    
    // VU integration (power average), engine-fed mode
    FVuPowerL: Double;
    FVuPowerR: Double;

    { private methods }

    procedure SetBevelStyle(value: TPanelBevel);
    procedure SetBevelWidth(value: Byte);

    procedure SetGreenLeds(value: Integer);
    procedure SetgreenColorOn(value: TColor);
    procedure SetgreenColorOff(value: TColor);
    procedure SetGreenMax(value: Integer);

    procedure SetYellowLeds(value: Integer);
    procedure SetyellowColorOn(value: TColor);
    procedure SetYellowColorOff(value: TColor);
    procedure SetYellowMax(value: Integer);

    procedure SetRedLeds(value: Integer);
    procedure SetRedColorOn(value: TColor);
    procedure SetRedColorOff(value: TColor);
    procedure SetRedMax(value: Integer);

    procedure SetSingleLed(value: Boolean);
    procedure SetSeparatorSpacing(value: Integer);
    procedure SetSeparatorColor(value: TColor);
    procedure SetStyle(value: TMfPeakMeterExStyle);
    procedure SetDirection(value: TMfPeakMeterExDirection);
    procedure SetPeakValue(value: Single);
    procedure SetPeakMeterChannel(value: TMfPeakMeterExChannel);

    // Callback timer
    procedure TimerTimer(sender: TObject);
    procedure SetSafeTimerInterval(value: Cardinal);
    //
    // Source selector
    procedure SetInputSource(const Value: TMfPeakMeterInputSource);
    // Endpoint metering lifecycle
    procedure ReleaseEndpointInterfaces;
    function EnsureEndpointInterfaces: HRESULT;
    procedure SetDeviceDataFlow(value: EDataFlow);
    procedure SetDeviceRole(value: ERole);

  protected
    { protected methods }

    procedure SetEnabled(value: Boolean); override;
    procedure WindProc(var Msg: TMessage); virtual;
    procedure PaintBar();
    function CreateEngine(): HRESULT;
    procedure Paint(); override;
    function CalculateX(X: Integer): Integer;
    function CalculateY(Y: Integer): Integer;
    procedure CalculatePeakValue();
    function GetLastLedPos(value: Integer): Integer;

  public
    { public fields }

    { public methods }
    constructor Create(aOwner: Tcomponent); override;
    destructor Destroy(); override;


    // External (engine-fed) metering
    procedure ResetExternalPeak();
    procedure PushPcm(const pData: PByte;
                      const ByteCount: DWord;
                      const Wfx: PWAVEFORMATEX);
  published
    { published methods }
    property DragCursor;
    property DragMode;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property Visible;

    property BevelStyle: TPanelBevel read fBevelStyle write SetBevelStyle;
    property BevelWidth: Byte read fBevelWidth write SetBevelWidth;

    property GreenColorOn: Tcolor read fColors[1, True] write SetGreenColorOn;
    property GreenColorOff: Tcolor read fColors[1, False] write SetGreenColorOff;
    property GreenLeds: Integer read fGreenLeds write SetGreenLeds;
    property GreenMax: Integer read fGreenMax write SetGreenMax;

    property YellowColorOn: Tcolor read fcolors[2, True] write setYellowColorOn;
    property YellowColorOff: Tcolor read fColors[2, False] write SetYellowColorOff;
    property YellowLeds: Integer read fYellowLeds write SetYellowLeds;
    property YellowMax: Integer read fYellowMax write SetYellowMax;

    property RedColorOn: Tcolor read fColors[3, True] write SetRedColorOn;
    property RedColorOff: Tcolor read fColors[3, False] write SetRedColorOff;
    property RedLeds: Integer read fRedLeds write SetRedLeds;
    property RedMax: Integer read fRedMax write SetRedMax;

    property ShowSingleLed: Boolean read fShowSingleLed write SetSingleLed;
    property SeparatorWidth: Integer read fSeparatorSpacing write SetSeparatorSpacing;
    property SeparatorColor: Tcolor read fSeparatorColor write SetSeparatorColor;

    property DeviceDataFlow: EDataFlow read fDataFlow write SetDeviceDataFlow default eRender;
    property DeviceRole: ERole read fRole write SetDeviceRole default eMultimedia;

    property Style: TMfPeakMeterExstyle read fStyle write SetStyle;
    property Direction: TMfPeakMeterExDirection read fDirection write SetDirection;
    property PeakValue: Single read sPeakValue write SetPeakValue;
    property Channels: UINT read fChannelCount;
    property SampleChannel: TMfPeakMeterExChannel read fMeterChannel write SetPeakMeterChannel;
    property IntTimer: TLightMmcssTimer read FTimer write FTimer;
    property Precision: Cardinal read fSafeTimerInterval write SetSafeTimerInterval default TIMER_PERIOD;
    property Enabled: Boolean read fEnabled write SetEnabled default False;
    // Select the metering source.
    property InputSource: TMfPeakMeterInputSource read FInputSource write SetInputSource default isWasapiEndpoint;
    // In engine-fed mode, choose which level metric drives the LEDs.
    property EngineLevelMode: TMfPeakMeterEngineLevelMode read FEngineLevelMode write FEngineLevelMode default elmPeak;
    property WasApiEngine: TWasApiEngine read FWasApiEngine write FWasApiEngine;
end;


procedure Register;


implementation

{ TLightMmcssTimerThread }

constructor TLightMmcssTimerThread.Create(AOwner: TLightMmcssTimer);
begin

  inherited Create(False);

  FreeOnTerminate := False;
  FOwner := AOwner;
end;


// Adjusts the thread priority of the calling thread relative to other threads performing the same task.
procedure TLightMmcssTimerThread.Execute();
var
  TaskIndex: DWORD;
  AvrtHandle: THandle;
  WaitMs: Cardinal;
  Prio: Integer;

begin

  // Register this thread with MMCSS (if available).
  TaskIndex := 0;

  if (FOwner.FMmcssTaskName = '') then
    FOwner.FMmcssTaskName := 'Pro Audio';

  AvrtHandle := AvSetMmThreadCharacteristicsW(PWideChar(FOwner.FMmcssTaskName),
                                              @TaskIndex);
  // AvrtHandle = 0 means failed.
  if (AvrtHandle <> 0) then
    begin

      Prio := FOwner.FMmcssPriority;
      if (Prio < -2) then
        Prio := -2;        // Low prio
      if (Prio > 2) then   //
        Prio := 2;

    // Map -2..+2 into AVRT_PRIORITY_*.
    case Prio of
      -2: AvSetMmThreadPriority(AvrtHandle,
                                AVRT_PRIORITY_VERYLOW);  // Very low prio
      -1: AvSetMmThreadPriority(AvrtHandle,              // Low prio
                                AVRT_PRIORITY_LOW);
       0: AvSetMmThreadPriority(AvrtHandle,
                                AVRT_PRIORITY_NORMAL);   // Normal prio
       1: AvSetMmThreadPriority(AvrtHandle,
                                AVRT_PRIORITY_HIGH);     // High prio
       2: AvSetMmThreadPriority(AvrtHandle,
                                AVRT_PRIORITY_CRITICAL); // Critical prio
    end;
  end;

  try

    // Initial delay (DueTime).
    WaitMs := FOwner.FDueTime;
    if (WaitMs > 0) then
      WaitForSingleObject(FOwner.FStopEvent,
                          WaitMs);

    while (not Terminated) and (WaitForSingleObject(FOwner.FStopEvent, 0) <> WAIT_OBJECT_0) do
      begin

        if (FOwner.FTargetHwnd <> 0) then
          PostMessage(FOwner.FTargetHwnd,
                      WM_MFPEAKMETEREX_TICK,
                      0,
                      0);

      WaitMs := FOwner.FPeriod;
      if (WaitMs < 1) then
        WaitMs := 1;

      // Wait for next tick, or stop request.
      WaitForSingleObject(FOwner.FStopEvent,
                          WaitMs);
    end;
  finally

    if (AvrtHandle <> 0) then
      AvRevertMmThreadCharacteristics(AvrtHandle);
  end;
end;

{ TLightMmcssTimer }

constructor TLightMmcssTimer.Create(AOwner: TComponent);
begin

  inherited Create(AOwner);

  FEnabled := False;
  FDueTime := 0;
  FPeriod := TIMER_PERIOD;
  FTargetHwnd := 0;
  FThread := nil;
  FStopEvent := CreateEvent(nil,
                            True,
                            False,
                            nil); // manual-reset.
  FMmcssTaskName := 'Pro Audio';
  FMmcssPriority := 1;
end;


destructor TLightMmcssTimer.Destroy();
begin

  Stop();
  if (FStopEvent <> 0) then
    CloseHandle(FStopEvent);

  inherited Destroy;
end;


procedure TLightMmcssTimer.Start;
begin

  if FEnabled and (FThread = nil) then
    begin

      if (csDesigning in ComponentState) then
        Exit;
      ResetEvent(FStopEvent);
      FThread := TLightMmcssTimerThread.Create(Self);
    end;
end;


procedure TLightMmcssTimer.Stop();
begin

  if (FThread <> nil) then
    begin

      SetEvent(FStopEvent);
      FThread.Terminate();
      // Give it a chance to exit quickly (it waits on FStopEvent).
      WaitForSingleObject(FThread.Handle,
                          2000);
      FreeAndNil(FThread);
    end;

  FEnabled := False;
end;


procedure TLightMmcssTimer.Reset();
begin

  if FEnabled then
    begin

      if (csDesigning in ComponentState) then
        Exit;

      Stop();
      FEnabled := True;
      Start();
    end;
end;


procedure TLightMmcssTimer.SetEnabled(const Value: Boolean);
begin

  if (Value <> FEnabled) then
    begin

      if (csDesigning in ComponentState) then
        Exit;

      FEnabled := Value;
      if FEnabled then
        Start()
      else
        Stop();
    end;
end;


procedure TLightMmcssTimer.SetDueTime(const Value: Cardinal);
begin

  if (Value <> FDueTime) then
    begin

      FDueTime := Value;
      Reset();
    end;
end;


procedure TLightMmcssTimer.SetPeriod(const Value: Cardinal);
begin

  if (Value <> FPeriod) then
    begin

      FPeriod := Value;
      Reset();
    end;
end;


procedure Register;
begin

  RegisterComponents('MfPack Core Audio Samples', [TMfPeakMeterMmcs]);
end;


constructor TMfPeakMeterMmcs.Create(aOwner: Tcomponent);
var
  hr: HResult;

label
  done;

begin
  inherited Create(aOwner);

  hr := S_OK;

  fEnabled := False;
  Width := 12;
  Height := 80;
  fBevelstyle := bvLowered;
  fBevelwidth := 1;
  fShowSingleLed := False;

  fColors[1,
          True] := clLime;
  fColors[2,
          True] := clYellow;
  fColors[3,
          True] := clRed;
  fColors[1,
          False] := clGreen;
  fColors[2,
          False] := clOlive;
  fColors[3,
          False] := clMaroon;

  fGreenLeds := 10;
  fYellowLeds := 6;
  fRedLeds := 4;
  fGreenMax := 50;
  fYellowMax := 30;
  fRedMax := 20;
  fSeparatorSpacing := 1;
  fSeparatorColor := clBlack;
  fStyle := dsVertical;
  fDirection := ddLeftUp;

  // Default endpoint selection
  fDataFlow := eRender;
  fRole := eMultimedia;

  pEnumerator := nil;
  pDevice := nil;
  pMeterInfo := nil;

  sPeakValue := 0.0;
  iPeakValue := 0;

  fSampleAllChannels := False;
  fMeterChannel := mcLeft;

  // Create the handle for this component
  fHWnd := AllocateHWnd(WindProc);

  // Default configuration
  fPeakMeterRunning := False; // legacy flag (no longer used to lock properties)
  fSafeTimerInterval := TIMER_PERIOD;

  // Default metering source = endpoint
  FInputSource := isWasapiEndpoint;
  FEngineLevelMode := elmRms;
  FExtRmsBitsL := 0;
  FExtRmsBitsR := 0;

  
  FExtPeakBitsL := 0;
  FExtPeakBitsR := 0;
  FExtSmoothValue := 0.0;
  FVuPowerL := 0.0;
  FVuPowerR := 0.0;

  // Create and configure a lightweight MMCSS-aware timer (posts to our hidden HWND).
  // The timer drives repaint/ballistics for BOTH sources; it is enabled only when Enabled=True.
  FTimer := TLightMmcssTimer.Create(Self);
  FTimer.TargetHwnd := fHWnd;
  FTimer.DueTime := 0;  // Immediate
  FTimer.Period := fSafeTimerInterval;
  FTimer.MmcssTaskName := 'Pro Audio';
  FTimer.MmcssPriority := 1; // High
  FTimer.Enabled := False;

  // Do NOT create endpoint interfaces here. They are created on-demand when:
  // Enabled=True AND InputSource=isWasapiEndpoint.

  // Create the meter
  fBmp := TBitmap.Create;
  // We paint our entire bounds; this reduces flicker for windowless controls.
  ControlStyle := ControlStyle + [csOpaque];

done:

  if ((csDesigning in ComponentState) = False) then
    if (FAILED(hr)) then
      begin
        MessageBox(0,
                   LPCWSTR('An error occured in ' + Self.ClassName + #13 +
                           'Error: ' + IntToStr(hr) + #13 +
                           Self.ClassName + ' will be disabled.') ,
                   LPCWSTR('Error'),
                   MB_OK or MB_ICONSTOP);
      end;
end;


destructor TMfPeakMeterMmcs.Destroy();
begin

  if Assigned(FTimer) then
    begin

      FTimer.Enabled := False;
      FTimer.Free;
    end;

  fBmp.Free;

  // Release endpoint interfaces (if any)
  ReleaseEndpointInterfaces();

  // Destroy handle
  DeallocateHWnd(fHWnd);

  inherited Destroy();
end;


function TMfPeakMeterMmcs.CreateEngine(): HRESULT;
var
  hr: HResult;

label
  done;

begin

  // Release any previous endpoint interfaces.
  ReleaseEndpointInterfaces;

  // Single instance
  hr := CoCreateInstance(CLSID_MMDeviceEnumerator,
                         nil,
                         CLSCTX_ALL,
                         IID_IMMDeviceEnumerator,
                         pEnumerator);
  if FAILED(hr) then
    goto done;

  // Get peak meter for default audio device.
  hr := pEnumerator.GetDefaultAudioEndpoint(fDataFlow,
                                            fRole,
                                            pDevice);
  if FAILED(hr) then
    goto done;

  hr := pDevice.Activate(IID_IAudioMeterInformation,
                         CLSCTX_ALL,
                         nil,
                         Pointer(pMeterInfo));
  if FAILED(hr) then
    goto done;

  // Get the number of channels
  hr := pMeterInfo.GetMeteringChannelCount(fChannelCount);
  if FAILED(hr) then
    goto done;

  // Set the length of the array to retrieve the samples
  SetLength(afPeakValues,
            fChannelCount);

done:
  Result := hr;
end;


procedure TMfPeakMeterMmcs.ReleaseEndpointInterfaces;
begin

  SafeRelease(pMeterInfo);
  SafeRelease(pDevice);
  SafeRelease(pEnumerator);

  fChannelCount := 0;
  SetLength(afPeakValues,
            0);
end;


function TMfPeakMeterMmcs.EnsureEndpointInterfaces: HRESULT;
begin

  if (pMeterInfo <> nil) then
    Exit(S_OK);

  Result := CreateEngine();
end;


procedure TMfPeakMeterMmcs.SetInputSource(const Value: TMfPeakMeterInputSource);
var
  hr: HRESULT;

begin

  if (FInputSource = Value) then
    Exit;

  FInputSource := Value;
  if (fEnabled = False) then
    begin

      // If disabled, just keep the desired setting; ensure endpoint is not held in engine mode.
      if (FInputSource = isWasapiEngine) then
        ReleaseEndpointInterfaces;
      Exit;
    end;

  // Enabled and source changed: reconfigure live.
  if (FInputSource = isWasapiEngine) then
    begin

      // Engine-fed mode: release endpoint and reset external peaks.
      ReleaseEndpointInterfaces();
      ResetExternalPeak;
      sPeakValue := 0.0;
      Paint;
    end
  else
    begin

      // Endpoint mode: ensure endpoint interfaces exist.
      hr := EnsureEndpointInterfaces();
      if FAILED(hr) then
        begin
          // Fail-safe: keep running but show silence.
          ReleaseEndpointInterfaces();
          sPeakValue := 0.0;
        end;
      Paint;
    end;
end;


// Using a regular TTimer is less precise, but can do as well.
procedure TMfPeakMeterMmcs.ResetExternalPeak;
var
  z: Single;
  b: LongInt;

begin

  z := 0.0;
  b := PLongInt(@z)^;

  InterlockedExchange(FExtPeakBitsL,
                      b);
  InterlockedExchange(FExtPeakBitsR,
                      b);
  InterlockedExchange(FExtRmsBitsL,
                      b);
  InterlockedExchange(FExtRmsBitsR,
                      b);
  FExtChannels := 0;
  FExtSmoothValue := 0.0;
end;


function MfReadWaveExtSubFormat(const Wfx: PWAVEFORMATEX; out SubFormat: TGUID): Boolean;
var
  p: PByte;


begin

  Result := False;
  if (Wfx = nil) then
    Exit;

  if (Wfx.wFormatTag <> WAVE_FORMAT_EXTENSIBLE) then
    Exit;

  // Windows WAVEFORMATEXTENSIBLE uses cbSize = 22 bytes of extra data.
  if (Wfx.cbSize < 22) then
    Exit;

  // Use Windows header layout offsets (WAVEFORMATEX is 18 bytes):
  // 18 (WAVEFORMATEX) + 2 (Samples union) + 4 (dwChannelMask) = 24.
  p := PByte(Wfx);
  SubFormat := PGUID(p + 24)^;
  Result := True;
end;


// Hardened, fail-soft, meter-friendly PushPcm.
// - Does NOT clamp audio before measuring.
// - Sanitizes NaN/Inf to 0 (prevents meter peg / poisoned envelope).
// - Kills denormals (optional but recommended; avoids CPU spikes & weird tails).
// - Computes BOTH block-peak and block-RMS per channel (stereo-aware).
// - Publishes the latest values atomically as Single-bits (InterlockedExchange).

procedure TMfPeakMeterMmcs.PushPcm(const pData: PByte;
                                   const ByteCount: DWord;
                                   const Wfx: PWAVEFORMATEX);
type

  PInt24 = ^TInt24;
  TInt24 = packed record
    b0,
    b1,
    b2: Byte;
  end;

  function IsFiniteSingle(const x: Single): Boolean; inline;
  var
    u: UInt32 absolute x;

  begin

    // Exponent all 1s => NaN/Inf
    Result := (u and $7F800000) <> $7F800000;
  end;

  function KillDenormal(const x: Single): Single; inline;
  const
    // Smallest normal Single ~ 1.17549435E-38
    MIN_NORMAL: Single = 1.17549435E-38;

  begin

    if (x > -MIN_NORMAL) and (x < MIN_NORMAL) then
      Result := 0.0
    else
      Result := x;
  end;

  function ReadS16(const p: PSmallInt): Single; inline;
  begin

    Result := p^ * (1.0 / 32768.0);
  end;

  function ReadS32(const p: PInteger): Single; inline;
  begin

    // 32-bit signed PCM full-scale is 2147483648
    Result := p^ * (1.0 / 2147483648.0);
  end;

  function ReadS24Packed(const p: PInt24): Single; inline;
  var
    v: Integer;

  begin

    // Build signed 24-bit in 32-bit container (sign extend)
    v := Integer(p^.b0) or
         (Integer(p^.b1) shl 8) or
         (Integer(p^.b2) shl 16);

    if ((v and $00800000) <> 0) then
      v := v or Integer($FF000000);
    Result := v * (1.0 / 8388608.0); // 2^23
  end;

  function ReadF32(const p: PSingle): Single; inline;
  var
    s: Single;

  begin

    s := p^;
    if not IsFiniteSingle(s) then
      s := 0.0;
    Result := KillDenormal(s);
  end;

var
  channels: Integer;
  blockAlign: Integer;
  bytesPerSample: Integer;

  p: PByte;
  frames: Integer;
  i: Integer;

  sL,
  sR: Single;
  absL,
  absR: Single;

  peakL,
  peakR: Single;
  sumSqL,
  sumSqR: Double;

  rmsL,
  rmsR: Single;

  fmtTag: Word;
  bits: Word;
  isIeeeFloat: Boolean;

  // Pointers for specific formats
  pS16: PSmallInt;
  pS32: PInteger;
  pF32: PSingle;
  p24: PInt24;

begin

  if (pData = nil) or
     (ByteCount <= 0) or
     (wfx = nil) then
    Exit;

  channels := wfx.nChannels;
  if (channels <= 0) then
    Exit;

  blockAlign := wfx.nBlockAlign;
  if (blockAlign <= 0) then
    Exit;

  frames := Integer(ByteCount) div Integer(blockAlign);
  if (frames <= 0) then
    Exit;

  fmtTag := wfx.wFormatTag;
  bits := wfx.wBitsPerSample;

  // WAVE_FORMAT_EXTENSIBLE can carry IEEE float or PCM in SubFormat; we treat it as PCM unless marked float.
  // If you already normalize this elsewhere, feel free to simplify.
  isIeeeFloat := (fmtTag = WAVE_FORMAT_IEEE_FLOAT);

  peakL := 0.0;
  peakR := 0.0;
  sumSqL := 0.0;
  sumSqR := 0.0;

  p := PByte(pData);

  // Fast paths:
  // - stereo: compute L/R.
  // - mono: compute L and mirror to R (so the UI keeps working).
  // - >2 channels: meter first two channels only (common DJ use-case). Adapt if downmix is needed.
  if isIeeeFloat then
    begin

      // Expect 32-bit float (most MF/WASAPI float meters)
      if (bits <> 32) then
        Exit;

      for i := 0 to frames - 1 do
        begin

          // channel 0
          pF32 := PSingle(p);
          sL := ReadF32(pF32);

          if (channels >= 2) then
            begin

              pF32 := PSingle(p + 4);
              sR := ReadF32(pF32);
            end
          else
            sR := sL;

          absL := Abs(sL);
          absR := Abs(sR);

          if (absL > peakL) then
            peakL := absL;
          if (absR > peakR) then
            peakR := absR;

          sumSqL := sumSqL + Double(sL) * Double(sL);
          sumSqR := sumSqR + Double(sR) * Double(sR);

          Inc(p,
              blockAlign);
        end;
    end
  else
    begin
      // PCM integer
      case bits of
      16:
        begin

          bytesPerSample := 2;
          for i := 0 to frames - 1 do
            begin
              pS16 := PSmallInt(p);
              sL := ReadS16(pS16);

              if (channels >= 2) then
                begin

                  pS16 := PSmallInt(p + bytesPerSample);
                  sR := ReadS16(pS16);
                end
              else
                sR := sL;

              absL := Abs(sL);
              absR := Abs(sR);

              if (absL > peakL) then
                peakL := absL;
              if (absR > peakR) then
                peakR := absR;

              sumSqL := sumSqL + Double(sL) * Double(sL);
              sumSqR := sumSqR + Double(sR) * Double(sR);

              Inc(p,
                  blockAlign);
            end;
        end;

      24:
        begin

          // Packed 24-bit (3 bytes per sample)
          bytesPerSample := 3;
          for i := 0 to frames - 1 do
            begin

              p24 := PInt24(p);
              sL := ReadS24Packed(p24);

              if (channels >= 2) then
                begin

                  p24 := PInt24(p + bytesPerSample);
                  sR := ReadS24Packed(p24);
                end
              else
                sR := sL;

              absL := Abs(sL);
              absR := Abs(sR);

              if (absL > peakL) then
                peakL := absL;
              if (absR > peakR) then
                peakR := absR;

              sumSqL := sumSqL + Double(sL) * Double(sL);
              sumSqR := sumSqR + Double(sR) * Double(sR);

              Inc(p,
                  blockAlign);
            end;
        end;

      32:
        begin

          // Could be 32-bit PCM int (common in some pipelines)
          bytesPerSample := 4;
          for i := 0 to frames - 1 do
            begin

              pS32 := PInteger(p);
              sL := ReadS32(pS32);

              if (channels >= 2) then
                begin

                  pS32 := PInteger(p + bytesPerSample);
                  sR := ReadS32(pS32);
                end
              else
                sR := sL;

              // No NaN/Inf possible here, but denorm-kill is harmless
              sL := KillDenormal(sL);
              sR := KillDenormal(sR);

              absL := Abs(sL);
              absR := Abs(sR);

              if (absL > peakL) then
                peakL := absL;
              if (absR > peakR) then
                peakR := absR;

              sumSqL := sumSqL + Double(sL) * Double(sL);
              sumSqR := sumSqR + Double(sR) * Double(sR);

              Inc(p,
                blockAlign);
            end;
        end;

    else
      // Unsupported bit depth for this hardened path
      Exit;
    end;
  end;

  // Convert sums to RMS (block RMS)
  if (frames > 0) then
    begin

      rmsL := Single(Sqrt(sumSqL / frames));
      rmsR := Single(Sqrt(sumSqR / frames));
    end
  else
    begin

      rmsL := 0.0;
      rmsR := 0.0;
    end;

  // Final safety: ensure meter values are sane 0..(a bit over 1 allowed if float overshoots)
  // We clamp ONLY the meter outputs (never the audio).
  if not IsFiniteSingle(peakL) then
    peakL := 0.0;
  if not IsFiniteSingle(peakR) then
    peakR := 0.0;
  if not IsFiniteSingle(rmsL) then
    rmsL  := 0.0;
  if not IsFiniteSingle(rmsR) then
    rmsR  := 0.0;

  if (peakL < 0) then
    peakL := 0;
  if (peakR < 0) then
    peakR := 0;
  if (rmsL  < 0) then
    rmsL  := 0;
  if (rmsR  < 0) then
    rmsR  := 0;

  // Keep a modest ceiling to avoid UI freakouts on float overs.
  // (If you prefer, set to 1.0 strictly.)
  if (peakL > 4.0) then
    peakL := 4.0;
  if (peakR > 4.0) then
    peakR := 4.0;
  if (rmsL  > 4.0) then
    rmsL  := 4.0;
  if (rmsR  > 4.0) then
    rmsR  := 4.0;

  // Publish atomically as Single bits (works with your InterlockedExchangeAdd read trick).
  InterlockedExchange(FExtPeakBitsL,
                      PLongInt(@peakL)^);
  InterlockedExchange(FExtPeakBitsR,
                      PLongInt(@peakR)^);

  InterlockedExchange(FExtRmsBitsL,
                      PLongInt(@rmsL)^);
  InterlockedExchange(FExtRmsBitsR,
                      PLongInt(@rmsR)^);
end;


procedure TMfPeakMeterMmcs.TimerTimer(sender: TObject);
var
  bL: LongInt;
  bR: LongInt;
  vL: Single;
  vR: Single;
  v: Single;
  dt: Double;
  releaseSec: Double;
  coeff: Double;
  hr: HRESULT;

  gL, gR: Single;

  // Display mapping helpers ----------------------------------------------------
  function Clamp01(const x: Single): Single; inline;
  begin
    if (x < 0) then
      Exit(0);
    if (x > 1) then
      Exit(1);
    Result := x;
  end;

  function LinearToDbFS(const lin: Single): Single; inline;
  const
    EPS: Single = 1.0e-12;
  begin
    Result := 20.0 * Log10(Max(lin, EPS));
  end;

  function DbToMeter01(const db: Single;
                       const FloorDb: Single;
                       const CeilDb: Single): Single; inline;
  begin
    Result := Clamp01((db - FloorDb) / (CeilDb - FloorDb));
  end;

  function Curve(const x, gamma: Single): Single; inline;
  begin
    Result := Power(Clamp01(x), gamma);
  end;

  // Change these values if you want to improve these basic settings.
  const

    METER_FLOOR_DB: Single = -60.0;
    METER_CEIL_DB: Single = -6.0;
    METER_GAMMA: Single =  0.65;

var
  db: Single;

begin

  // ===========================================================================
  // ENGINE SOURCE (per-deck metering).
  // mmPreFader  = raw engine RMS (gain staging)
  // mmPostFader = raw engine RMS * GUI fader/balance gains (matches audio)
  // ===========================================================================
  if (FInputSource = isWasapiEngine) then
    begin

      // Always use engine-fed RMS as the base
      bL := InterlockedExchangeAdd(FExtRmsBitsL,
                                   0);

      bR := InterlockedExchangeAdd(FExtRmsBitsR,
                                   0);

      vL := PSingle(@bL)^;
      vR := PSingle(@bR)^;

      // Default = unity (acts like pre-fader if engine not connected).
      gL := 1.0;
      gR := 1.0;

      // Apply GUI gains only for post-fader
      if Assigned(FWasApiEngine) then
        begin

          FWasApiEngine.GetMeterFaderGains(gL,
                                           gR);
          vL := vL * gL;
          vR := vR * gR;
        end;

      // Channel handling
      if Assigned(FWasApiEngine) then
        FExtChannels := FWasApiEngine.SoundChannels;

      if (FExtChannels <= 1) then
        begin

          vR := vL;
          fMeterChannel := mcLeft;
        end;

      if (fSampleAllChannels = True) then
        begin

          if (vR > vL) then
            v := vR
          else
            v := vL;
        end
      else
        begin
          if (fMeterChannel = mcRight) and (FExtChannels >= 2) then
            v := vR
          else
            v := vL;
        end;

      // Display mapping
      if (v < 0.0) then
        v := 0.0;

      db := LinearToDbFS(v);
      v := DbToMeter01(db,
                       METER_FLOOR_DB,
                       METER_CEIL_DB);

      if (METER_GAMMA <> 1.0) then
        v := Curve(v,
                   METER_GAMMA);

      // Ballistics
      dt := fSafeTimerInterval / 1000.0;
      if (dt <= 0.0) then
        dt := 0.01;

      releaseSec := 0.25;
      coeff := Exp(-dt / releaseSec);

      if (v >= FExtSmoothValue) then
        FExtSmoothValue := v
      else
        FExtSmoothValue := Max(v,
                               (FExtSmoothValue * coeff) * 1.0);

      sPeakValue := FExtSmoothValue;

      Invalidate;
      Exit;
    end;

  // ===========================================================================
  // ENDPOINT SOURCE (global Windows meter).
  // ===========================================================================
  if (pMeterInfo = nil) then
    begin
      hr := EnsureEndpointInterfaces();
      if FAILED(hr) or (pMeterInfo = nil) then
        begin

          sPeakValue := 0.0;
          Invalidate;
          Exit;
        end;
    end;

  if (fSampleAllChannels = True) then
    pMeterInfo.GetPeakValue(sPeakValue)
  else
    begin
      pMeterInfo.GetChannelsPeakValues(fChannelCount, @afPeakValues[0]);

      if (fChannelCount = 1) then
        fMeterChannel := mcLeft;

      if (fMeterChannel = mcLeft) then
        sPeakValue := afPeakValues[0]
      else if (fMeterChannel = mcRight) and (fChannelCount = 2) then
        sPeakValue := afPeakValues[1]
      else
        sPeakValue := afPeakValues[0];
    end;

  Invalidate;
end;


procedure TMfPeakMeterMmcs.SetSafeTimerInterval(value: Cardinal);
begin

  if (value < 10) then
    value := 10;
  if (value > 10000) then
    value := 1000;

  fSafeTimerInterval := value;
  if Assigned(FTimer) then
    FTimer.Period := fSafeTimerInterval;
end;


procedure TMfPeakMeterMmcs.SetDeviceDataFlow(value: EDataFlow);
begin

  if (fDataFlow <> value) then
    fDataFlow := value;
end;


procedure TMfPeakMeterMmcs.SetDeviceRole(value: ERole);
begin

  if (fRole <> value) then
    fRole := value;
end;


procedure TMfPeakMeterMmcs.SetEnabled(value: Boolean);
var
  hr: HRESULT;
begin

  if (fEnabled = value) then
    begin
      inherited;
      Exit;
    end;

  fEnabled := value;

  if (fEnabled = False) then
    begin

      // Disable everything.
      if (FTimer <> nil) then
        FTimer.Enabled := False;

      // Release endpoint interfaces so endpoint volume/mute can never influence visuals.
      ReleaseEndpointInterfaces;

      // Reset external peaks and visuals.
      ResetExternalPeak;
      sPeakValue := 0.0;
      Paint;

      inherited;
      Exit;
    end;

  // Enabling
  if (FTimer <> nil) and ((csDesigning in ComponentState) = False) then
    FTimer.Enabled := True;

  // Ensure the correct source is active.
  if (FInputSource = isWasapiEndpoint) then
    begin

      hr := EnsureEndpointInterfaces;
      if FAILED(hr) then
        begin
          // Fail-safe: keep running but show silence.
          ReleaseEndpointInterfaces;
          sPeakValue := 0.0;
        end;
    end
  else
    begin

      // Engine-fed mode: ensure endpoint interfaces are not held.
      ReleaseEndpointInterfaces;
      ResetExternalPeak;
      sPeakValue := 0.0;
    end;

  Paint;

  inherited;
end;


function TMfPeakMeterMmcs.CalculateX(X: Integer): Integer;
begin

  Result := (Width - X) - 1;
end;


function TMfPeakMeterMmcs.CalculateY(Y: Integer): Integer;
begin

  Result := (Height - Y) - 1;
end;


procedure TMfPeakMeterMmcs.SetBevelWidth(value: Byte);
begin

  if (value <> fbevelwidth) then
    begin
      if (value = 0) then
        value := 1;
      if (value > (height div 3)) or (value > (width div 3)) then
        value := 1;
      fBevelWidth := value;
      Paint;
    end;
end;


procedure TMfPeakMeterMmcs.SetBevelStyle(value: TPanelbevel);
begin

  if (value <> fBevelStyle) then
    begin
      fBevelStyle := value;
      Paint;
    end;
end;


procedure TMfPeakMeterMmcs.SetGreenColorOn(value: TColor);
begin

  if (value <> fColors[1,
                       True]) then
    begin
      fColors[1,
              True] := value;
      Paint;
    end;
end;


procedure TMfPeakMeterMmcs.SetGreenMax(value: Integer);
begin

  if (value <> fGreenMax) then
    begin
      fGreenMax := value;
      Paint;
    end;
end;


procedure TMfPeakMeterMmcs.SetgreenLeds(value: Integer);
begin

  if (value <> fGreenLeds) then
    begin
      fGreenLeds := value;
      Paint;
    end;
end;


procedure TMfPeakMeterMmcs.SetGreenColorOff(value: TColor);
begin

  if (value <> fColors[1,
                       False]) then
    begin
      fColors[1,
              False] := value;
      Paint;
    end;
end;


procedure TMfPeakMeterMmcs.SetYellowColorOn(value: TColor);
begin

  if (value <> fColors[2, True]) then
    begin
      fColors[2,
              True] := value;
      Paint;
    end;
end;


procedure TMfPeakMeterMmcs.SetYellowMax(value: Integer);
begin

  if (value <> fYellowMax) then
    begin
      fYellowMax := value;
      Paint;
    end;
end;


procedure TMfPeakMeterMmcs.SetYellowLeds(value: Integer);
begin

  if (value <> fYellowLeds) then
    begin
      fYellowLeds := value;
      Paint;
    end;
end;


procedure TMfPeakMeterMmcs.SetYellowColorOff(value: TColor);
begin

  if value <> fColors[2,
                      False] then
    begin
      fColors[2,
              False] := value;
      Paint;
    end;
end;


procedure TMfPeakMeterMmcs.SetRedColorOn(value: TColor);
begin

  if (value <> fColors[3,
                       True]) then
    begin
      fColors[3,
              True] := value;
      Paint;
    end;
end;


procedure TMfPeakMeterMmcs.SetRedMax(value: Integer);
begin

  if (value <> fRedMax) then
    begin
      fRedmax := value;
      Paint;
    end;
end;


procedure TMfPeakMeterMmcs.SetRedLeds(value: Integer);
begin

try

  if (value <> fRedLeds) then
    begin

      fRedLeds := value;
      Paint;
    end;
except

  on Exception do Exit;
end;
end;


procedure TMfPeakMeterMmcs.SetRedColorOff(value: TColor);
begin

try

  if (value <> fcolors[3,
                       False]) then
    begin
        fColors[3,
                False] := value;
        Paint;
    end;
except

  on Exception do Exit;
end;
end;


procedure TMfPeakMeterMmcs.SetSingleLed(value: Boolean);
begin

try

  if (value <> fShowSingleLed) then
    begin

      fShowSingleLed := value;
      Paint;
    end;
except

  on Exception do Exit;
end;
end;


procedure TMfPeakMeterMmcs.SetSeparatorSpacing(value: Integer);
begin

try

  if (value <> fSeparatorSpacing) then
    begin

      fSeparatorSpacing := value;
      Paint;
    end;
except

  on Exception do Exit;
end;
end;


procedure TMfPeakMeterMmcs.SetSeparatorColor(value: TColor);
begin

try

  if (value <> fSeparatorColor) then
    begin

      fSeparatorColor := value;
      Paint;
    end;
except
  on Exception do Exit;
end;
end;


procedure TMfPeakMeterMmcs.SetStyle(value: TMfPeakMeterExStyle);
begin

try

  if (value <> fstyle) then
    begin

      fStyle := value;
      Paint;
    end;
except

  on Exception do Exit;
end;
end;


procedure TMfPeakMeterMmcs.SetDirection(value: TMfPeakMeterExdirection);
begin

try

  if (value <> fDirection) then
    begin

      fDirection := value;
      Paint;
    end;
except

  on Exception do Exit;
end;
end;


procedure TMfPeakMeterMmcs.SetPeakValue(value: Single);
begin

try

  if (value <> sPeakValue) then
    begin

      sPeakValue := value;
      CalculatePeakValue;
      Paint;
    end;
except

  on Exception do Exit;
end;
end;


procedure TMfPeakMeterMmcs.SetPeakMeterChannel(value: TMfPeakMeterExChannel);
begin

  if (fMeterChannel <> value) then
    fMeterChannel := value;
end;


// Calculates the peak value returned from a device (0.0 - 1.0) to an integer
procedure TMfPeakMeterMmcs.CalculatePeakValue();
const
  DB_FLOOR = -60.0;  // meter floor
  DB_GREENHI = -12.0;  // green->yellow boundary
  DB_YELLOWHI = -3.0;   // yellow->red boundary
  DB_TOP = 0.0;    // 0 dBFS
  EPS_AMP = 1e-12;

var
  v: Double;
  db: Double;
  idx: Integer;
  totalLeds: Integer;

  function ClampD(const x,
                  lo,
                  hi: Double): Double; inline;
  begin

    if (x < lo) then
      Exit(lo);
    if (x > hi) then
      Exit(hi);
    Result := x;
  end;

  function MapDbToCount(const d,
                        lo,
                        hi: Double;
                        count: Integer): Integer; inline;
  var
    t: Double;

  begin

    if (count <= 0) then
      Exit(0);

    if (hi <= lo) then
      Exit(0);

    t := (d - lo) / (hi - lo);          // 0..1
    t := ClampD(t,
                0.0,
                1.0);

    Result := Trunc(t * count + 1e-9);  // 0..count
  end;

begin

  if (fEnabled = False) then
    begin

      iPeakValue := 0;
      Exit;
    end;

  // sPeakValue expected 0..1 amplitude
  v := sPeakValue;
  if (v <= 0.0) then
    begin

      iPeakValue := 0;
      Exit;
    end;

  // Endpoint mode: keep existing linear mapping if you prefer
  if (FInputSource = isWasapiEndpoint) then
    begin

      if (v > 1.0) then
        v := 1.0;

      totalLeds := fGreenLeds + fYellowLeds + fRedLeds;
      iPeakValue := Round(v * totalLeds);
      Exit;
    end;

  // Engine-fed: map to dBFS
  if (v < EPS_AMP) then
    v := EPS_AMP;

  db := 20.0 * Log10(v);
  db := ClampD(db,
               DB_FLOOR,
               DB_TOP);

  totalLeds := fGreenLeds + fYellowLeds + fRedLeds;
  if (totalLeds <= 0) then
    begin

      iPeakValue := 0;
      Exit;
    end;

  // Piecewise map into LED index (0..totalLeds)
  if (db < DB_GREENHI) then
    begin

      // Green zone: 0..GreenLeds
      idx := MapDbToCount(db,
                          DB_FLOOR,
                          DB_GREENHI,
                          fGreenLeds);
      iPeakValue := idx;
    end
  else
    if (db < DB_YELLOWHI) then
      begin

        // Yellow zone: GreenLeds..GreenLeds+YellowLeds
        idx := MapDbToCount(db,
                            DB_GREENHI,
                            DB_YELLOWHI,
                            fYellowLeds);
        iPeakValue := fGreenLeds + idx;
      end
    else
      begin

        // Red zone: Green+Yellow..Total
        idx := MapDbToCount(db,
                            DB_YELLOWHI,
                            DB_TOP,
                            fRedLeds);
        iPeakValue := fGreenLeds + fYellowLeds + idx;
      end;

  if (iPeakValue < 0) then
    iPeakValue := 0
  else
    if (iPeakValue > totalLeds) then
      iPeakValue := totalLeds;
end;


function TMfPeakMeterMmcs.GetLastLedPos(value: Integer): Integer;
var
  totalLeds: Integer;

begin

  // value is expected to be total LED count (Green+Yellow+Red)
  CalculatePeakValue();

  totalLeds := value;
  if (totalLeds <= 0) then
    Exit(0);

  // iPeakValue is already 0..totalLeds
  if (iPeakValue <= 0) then
    Exit(0);

  if (iPeakValue > totalLeds) then
    Result := totalLeds
  else
    Result := iPeakValue;
end;


procedure TMfPeakMeterMmcs.WindProc(var Msg: TMessage);
var
  Handled: Boolean;

begin

  Handled := True;

  case Msg.Msg of
    WM_PAINT: Paint;
    WM_MFPEAKMETEREX_TICK: TimerTimer(Self);
  else
    Handled := False;
  end;

  if Handled then
    Msg.Result := 0
  else
    Msg.Result := DefWindowProc(fHWnd,
                                Msg.Msg,
                                Msg.WParam,
                                Msg.LParam);
end;


procedure TMfPeakMeterMmcs.PaintBar();
var
  bw: Byte;
  tcbottom: TColor;
  tctop: TColor;
  lp: Integer;
  ileds: Integer;
  ibarwidth: Integer;
  ibarheight: Integer;
  inum : Integer;
  bactivate : Boolean;
  bcolor: Byte;
  ax: array [0..3] of Integer;
  ay: array [0..3] of Integer;
  i: Integer;

begin

try

  fBmp.Width := Width;
  fBmp.Height := Height;

  // Initiate the x and y coordinates
  for i := Low(ax) to High(ax) do
    begin

      ax[i] := 0;
      ay[i] := 0;
    end;

  fBmp.Canvas.Pen.Color := fSeparatorColor;
  fBmp.Canvas.Pen.Width := 0;
  fBmp.Canvas.Pen.Style := psSolid;
  fBmp.Canvas.Brush.Color := fSeparatorColor;
  fBmp.Canvas.Brush.Style := bsSolid;
  fBmp.Canvas.Rectangle(0,
                        0,
                        Width,
                        Height);

  // Calculate the number of leds.
  ileds := (fGreenLeds + fRedLeds + fYellowLeds);

  if (ileds > 0) then
    begin

      // Calculate the width.
      ibarwidth := (Width div ileds);
      ibarheight := Height;

      if (fStyle = dsVertical) then
        begin

          ibarwidth := Height div ileds;
          ibarheight := Width;
        end;

      if (ibarwidth > fSeparatorSpacing) then
        begin

          // Calculate the last led.
          inum := GetLastLedPos(ileds);
          if (inum = 0) then
            if (iPeakValue <> 0) then
              inum := 1;

          // Set colors.
          with fBmp.Canvas do
            begin

              Pen.Width := 0;
              Pen.Style := psSolid;
              Brush.Style := bsSolid;
            end;

          for ileds := 1 to ileds do
            begin
              bactivate := true;
              if (ileds < inum) then
                if fShowSingleLed then
                  bactivate := False;

              if (ileds > inum) then
                bactivate := False;

              bcolor := 1;
              if (ileds > (GreenLeds + YellowLeds)) then
                bcolor := 3
              else if (ileds > GreenLeds) then
                bcolor := 2;

              fBmp.canvas.brush.color := fColors[bcolor, bactivate];
              fBmp.canvas.pen.color := fColors[bcolor, bactivate];

              // Calculate positions
              case fStyle of
                dsHorizontal: begin
                                ax[0] := (ileds - 1) * ibarwidth;
                                ax[1] := ileds * ibarwidth;
                                ay[0] := 0;
                                ay[1] := ibarheight - 1;
                                // Brush
                                ax[2] := ileds * ibarwidth - fSeparatorSpacing;
                                ax[3] := ax[2] + fSeparatorSpacing + 1;
                                ay[2] := 0;
                                ay[3] := ibarheight - 1;

                                if (fDirection = ddLeftUp) then
                                  begin
                                    for i := Low(ax) to High(ax) do
                                      ax[i] := CalculateX(ax[i]);
                                  end;
                              end;

                  dsVertical: begin
                                ay[0] := (ileds - 1) * ibarwidth;
                                ay[1] := ileds * ibarwidth;
                                ax[0] := 0;
                                ax[1] := ibarheight - 1;
                                // Brush
                                ay[2] := ileds * ibarwidth - fSeparatorSpacing;
                                ay[3] := ay[2] + fSeparatorSpacing + 1;
                                ax[2] := 0;
                                ax[3] := ibarheight - 1;
                                if (fDirection = ddLeftUp) then
                                  begin
                                    for i := Low(ay) to High(ay) do
                                      ay[i] := CalculateY(ay[i]);
                                  end;
                              end;
              end;  //case fStyle of

            // Rectangle
            fBmp.canvas.rectangle(ax[0],
                                  ay[0],
                                  ax[1],
                                  ay[1]);

            if (fSeparatorSpacing > 0) then
              begin
                with fBmp.canvas do
                  begin
                    brush.color := fSeparatorColor;
                    pen.color := fSeparatorColor;
                    rectangle(ax[2],
                              ay[2],
                              ax[3],
                              ay[3]);
                 end;
             end;
        end; // for ileds
      end; // if (ibarwidth > fsepwidth)
    end; // if (ileds > 0)

  tcBottom := clWhite;
  tcTop := clGray;
  bw := fBevelWidth;

  if (fBevelStyle = bvNone) then
    bw := 0;

  if (bw > (height div 3)) or (bw > (width div 3)) then
    bw := 1;

  if (bw > 0) then
    begin
      if (fBevelStyle = bvRaised) then
        begin

          tcBottom := clGray;
          tcTop := clWhite;
        end;

      with fBmp.canvas do
        begin

          // Bottom right.
          Pen.Color := tcBottom;
          for lp := 0 to bw - 1 do
            begin

              MoveTo(CalculateX(Width),
                     CalculateY(lp));

              LineTo(CalculateX(lp),
                     CalculateY(lp));

              LineTo(CalculateX(lp),
                     CalculateY(Height));
            end;

          // Top left.
          Pen.Color := tcTop;
          for lp  := 0 to bw - 1 do
            begin

              MoveTo(Width,
                     lp);

              LineTo(lp,
                     lp);

              LineTo(lp,
                     Height - bw);
            end;
        end;
    end;

  canvas.draw(0,
              0,
              fBmp);
except

  on Exception do Exit;    // silent exception
end;
end;


procedure TMfPeakMeterMmcs.Paint();
begin

  PaintBar();
  inherited
end;

end.

