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
  System.Classes,
  System.Win.ComObj,
  {VCL}
  VCL.Graphics,
  VCL.Controls,
  VCL.Forms,
  VCL.ExtCtrls,
  {ActiveX}
  WinApi.ActiveX.ObjBase,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfUtils,
  {CoreAudioApi}
  WinApi.CoreAudioApi.MMDeviceApi,
  WinApi.CoreAudioApi.EndPointVolume;


const
  // Timer period (in milliseconds)
  TIMER_PERIOD = 100;

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

    fColors: array [1..3, False..True] of TColor;

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
    fSafeTimerInterval: Cardinal;      // Timer interval
    fGraphicsOnly: Boolean;
    fPeakMeterRunning: Boolean;
    fChannelCount: UINT;
    fSampleAllChannels: Boolean;
    fMeterChannel: TMfPeakMeterExChannel;
    fEnabled: Boolean;

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
    procedure SetGraphicsOnly(value: Boolean);
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
    procedure CalculatePeakValue;
    function GetLastLedPos(value: Integer): Integer;

  public
    { public fields }

    { public methods }
    constructor Create(aOwner: Tcomponent); override;
    destructor Destroy; override;

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
    property UseGraphicsOnly: Boolean read fGraphicsOnly write SetGraphicsOnly default False;
end;


procedure Register;


implementation

type
  AVRT_PRIORITY = Integer;

// mmcss
function AvSetMmThreadCharacteristicsW(TaskName: PWideChar;
                                       var TaskIndex: DWORD):
                                       THandle; stdcall; external 'avrt.dll';

function AvRevertMmThreadCharacteristics(AvrtHandle: THandle): BOOL; stdcall; external 'avrt.dll';

function AvSetMmThreadPriority(AvrtHandle: THandle;
                               Priority: AVRT_PRIORITY): BOOL; stdcall; external 'avrt.dll';


const
  AVRT_PRIORITY_LOW      = -1;
  AVRT_PRIORITY_NORMAL   = 0;
  AVRT_PRIORITY_HIGH     = 1;
  AVRT_PRIORITY_CRITICAL = 2;

{ TLightMmcssTimerThread }

constructor TLightMmcssTimerThread.Create(AOwner: TLightMmcssTimer);
begin

  inherited Create(False);

  FreeOnTerminate := False;
  FOwner := AOwner;
end;


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
                                              TaskIndex);
  if (AvrtHandle <> 0) then
    begin

      Prio := FOwner.FMmcssPriority;
      if (Prio < -2) then
        Prio := -2;
      if (Prio > 2) then
        Prio := 2;

    // Map -2..+2 into AVRT_PRIORITY_*.
    case Prio of
      -2: AvSetMmThreadPriority(AvrtHandle,
                                AVRT_PRIORITY_LOW);
      -1: AvSetMmThreadPriority(AvrtHandle,
                                AVRT_PRIORITY_LOW);
       0: AvSetMmThreadPriority(AvrtHandle,
                                AVRT_PRIORITY_NORMAL);
       1: AvSetMmThreadPriority(AvrtHandle,
                                AVRT_PRIORITY_HIGH);
       2: AvSetMmThreadPriority(AvrtHandle,
                                AVRT_PRIORITY_CRITICAL);
    end;
  end;

  try

    // Initial delay (DueTime)
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

  pEnumerator := nil;
  pDevice := nil;
  pMeterInfo := nil;

  sPeakValue := 0.0;
  iPeakValue := 0;

  // Create the handle for this component
  fHWnd := AllocateHWnd(WindProc);

  if (csDesigning in ComponentState) then
    fPeakMeterRunning := False
  else
    fPeakMeterRunning := True;

  fGraphicsOnly := False;

  if (fGraphicsOnly = False) then
    hr := CreateEngine();

   // Create the meter
   fBmp := TBitmap.Create;

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

  if (fGraphicsOnly = False) then
    begin

      SafeRelease(pEnumerator);
      SafeRelease(pDevice);
      SafeRelease(pMeterInfo);
    end;

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

  // Single instance
  hr := CoCreateInstance(CLSID_MMDeviceEnumerator,
                         nil,
                         CLSCTX_ALL,
                         IID_IMMDeviceEnumerator,
                         pEnumerator);
  if FAILED(hr) then
    goto done;

  // Get peak meter for default audio-rendering device.
  // You can easily modify for the default capture device.
  // Change the value of the first parameter in the call to the from eRender to eCapture.
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

  // Create and activate a lightweight MMCSS-aware timer (posts to our hidden HWND).
  FTimer := TLightMmcssTimer.Create(Self);
  FTimer.TargetHwnd := fHWnd;
  FTimer.DueTime := 0;  // Immediate
  FTimer.Period := TIMER_PERIOD;
  FTimer.MmcssTaskName := 'Pro Audio';
  FTimer.MmcssPriority := 1; // High

  if (csDesigning in ComponentState) = False then
    FTimer.Enabled := True;

done:
  Result := hr;
end;


// Using a regular TTimer is less precise, but can do as well.
procedure TMfPeakMeterMmcs.TimerTimer(sender: TObject);
begin

  if (fSampleAllChannels = True) then
    pMeterInfo.GetPeakValue(sPeakValue)
  else
    begin

      pMeterInfo.GetChannelsPeakValues(fChannelCount,
                                       @afPeakValues[0]);

      if (fChannelCount = 1) then
        fMeterChannel := mcLeft;

      // Now split the array: The first array member = leftchannel,
      // the second = rightchannel.
      if (fMeterChannel = mcLeft) then
        sPeakValue := afPeakValues[0]
      else if (fMeterChannel = mcRight) and (fChannelCount = 2) then
        sPeakValue := afPeakValues[1]
      else
        sPeakValue := afPeakValues[0]; // Fall back to default (mono = always left channel).
    end;
    Paint;
end;


procedure TMfPeakMeterMmcs.SetSafeTimerInterval(value: Cardinal);
begin

  // If running, user can't change this value
  if (fPeakMeterRunning = True) then
    Exit;

  if (value < 10) or (value > 10000) then
    value := 10; // Reset to default.
  fSafeTimerInterval := value;
  if Assigned(FTimer) then
    FTimer.Period := fSafeTimerInterval;
end;


procedure TMfPeakMeterMmcs.SetGraphicsOnly(value: Boolean);
begin

  // If running, user can't change this value
  if (fGraphicsOnly <> value) and (fPeakMeterRunning = False) then
    fGraphicsOnly := value;
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
begin

  if (fEnabled <> value) then
    fEnabled := value;
  // The timer will not be created if an error occured during
  // initialisation
  if (Ftimer <> nil) then
    Ftimer.Enabled := fEnabled;

  inherited
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
begin

  if (fEnabled = False) and (fPeakMeterRunning = true) then
    iPeakValue := 0
  else
    iPeakValue := Round(sPeakValue * (fRedMax + fYellowMax + fGreenMax));
end;


function TMfPeakMeterMmcs.GetLastLedPos(value: Integer): Integer;
var
  num: Integer;
  ye: Integer;
  gr: Integer;

begin

  CalculatePeakValue();

  ye := fYellowMax;
  if (YellowLeds = 0) then
    ye := 0;

  gr := fGreenMax;
  if (GreenLeds = 0) then
    gr := 0;

  if (iPeakValue >= (fRedMax + gr + ye)) then
    begin
      Result := value;
      Exit;
    end;

  // Calculate red led position
  if (iPeakValue > (ye + gr)) then
    begin
      if RedLeds = 0 then
        begin
          Result := value;
          Exit;
        end;

      num := (iPeakValue - ye - gr);
      Result := Round((fRedLeds / fRedMax) * num) + fGreenLeds + fYellowLeds;

      if (Result = (fgreenLeds + fyellowLeds)) then
        Result := Result + 1;
      Exit;
    end;

  // Calculate yellow led position
  if (iPeakValue > (gr)) then
    begin
      if (YellowLeds = 0) then
        begin
          Result := value;
          Exit;
        end;

      num := iPeakValue - gr;
      Result := round((fYellowLeds / ye) * num) + fGreenLeds;

      if (Result = fGreenLeds) then
        Result := result + 1;
      Exit;
    end;

  // Calculate green led position
  if (gr = 0) then
    begin
      Result := 0;
      Exit;
    end;

  Result := Round((fGreenLeds / gr) * iPeakValue);
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

