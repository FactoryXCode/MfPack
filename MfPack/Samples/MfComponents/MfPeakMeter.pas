// FactoryX
//
// Copyright:  FactoryX. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: MfPeakMeter.pas
// Kind: Pascal Unit Component
// Release date: 04-08-2016
// Language: ENU
//
// Version: 3.1.9
// Description: A basic Peakmeter component based on the Mf Peakmeter Sample.
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
// 24/01/2026 ChatGPT             Thread-safe snapshot + code polishing
//------------------------------------------------------------------------------
//
// Remarks: To install the visual components, choose Install in the Project Manager.
//          Requires Windows 7 or later.
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

unit MfPeakMeter;

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
  System.SyncObjs,
  {VCL}
  VCL.Graphics,
  VCL.Controls,
  VCL.Forms,
  VCL.ExtCtrls,
  {ActiveX}
  WinApi.ActiveX.ObjBase,
  {CoreAudioApi}
  WinApi.CoreAudioApi.MMDeviceApi,
  WinApi.CoreAudioApi.EndPointVolume;

type

  TPeakDirection = (pdVertical,
                    pdHorizontal);

  TPeakMeterChannel = (mcLeft,
                       mcRight);

  TMfPeakMeterErrorEvent = procedure(Sender: TObject;
                                     const Hr: HRESULT) of object;

  TMfPeakMeter = class(TGraphicControl)
  private
    { private fields }
    fPeakMeterBmp: TBitmap;
    fBackGroundColor: TColor;
    fBarColor: TColor;

    fChannelCount: UINT;
    fPeakDirection: TPeakDirection;
    fMeterChannel: TPeakMeterChannel;

    lwTimerPeriod: LongWord; // Timer period (in milliseconds)
    fEnabled: Boolean;

    // Interfaces
    pEnumerator: IMMDeviceEnumerator;
    pDevice: IMMDevice;
    pMeterInfo: IAudioMeterInformation;

    fDataFlow: EDataFlow; // The data-flow direction for the endpoint device.
    fRole: ERole;         // The role of the endpoint device.

    // Timer (runs in the VCL thread message loop)
    fTimer: TTimer;

    // Thread-safe peak snapshot (Single stored as atomic Int32 bit-pattern)
    fPeakLeftBits: Integer;
    fPeakRightBits: Integer;

    // Reusable buffer for GetChannelsPeakValues
    fPeakValues: TArray<Single>;

    fOnError: TMfPeakMeterErrorEvent;

    { private methods }
    procedure DrawPeakMeter();
    procedure SetBackGroundColor(value: TColor);
    procedure SetBarColor(value: TColor);
    procedure SetDirection(value: TPeakDirection);
    procedure SetPeakMeterChannel(value: TPeakMeterChannel);
    procedure SetDeviceDataFlow(value: EDataFlow);
    procedure SetDeviceRole(value: ERole);
    procedure SetTimerPeriod(value: LongWord);
    procedure TimerTick(Sender: TObject);
    function  EnsureMeterReady: Boolean;
    procedure ReleaseMeter();
    procedure DoError(const Hr: HRESULT);

    class function SingleToBits(const V: Single): Integer; static;
    class function BitsToSingle(const B: Integer): Single; static;
    class function Clamp_(const V: Single): Single; static;

    function GetPeakValueThreadSafe(): Single;

  protected

    procedure SetEnabled(value: Boolean); override;
    procedure Paint(); override;
    procedure Resize(); override;

  public

    constructor Create(aOwner: Tcomponent); override;
    destructor Destroy(); override;

    // Thread-safe accessor for the currently selected channel (0..1)
    property PeakValue: Single read GetPeakValueThreadSafe;

  published

    property DragCursor;
    property DragMode;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property Visible;

    property Enabled: Boolean read fEnabled write SetEnabled default False;
    property BackGroundColor: TColor read fBackGroundColor write SetBackGroundColor;
    property BarColor: TColor read fBarColor write SetBarColor;
    property Direction: TPeakDirection read fPeakDirection write SetDirection;
    property SampleChannel: TPeakMeterChannel read fMeterChannel write SetPeakMeterChannel;

    property DeviceDataFlow: EDataFlow read fDataFlow write SetDeviceDataFlow default eRender;
    property DeviceRole: ERole read fRole write SetDeviceRole default eMultimedia;
    property Precision: LongWord read lwTimerPeriod write SetTimerPeriod default 100;

    property OnError: TMfPeakMeterErrorEvent read fOnError write fOnError;
  end;

procedure Register;

implementation

procedure Register;
begin

  RegisterComponents('MfPack Core Audio Samples', [TMfPeakMeter]);
end;

{ TMfPeakMeter }

//-----------------------------------------------------------
// constructor -- creates a bitmap that contains a peak meter.
//   The peak meter displays the peak sample value that plays
//   through the default rendering device.
//-----------------------------------------------------------
constructor TMfPeakMeter.Create(aOwner: Tcomponent);
begin

  inherited Create(aOwner);

  // We paint our entire bounds; this reduces flicker for windowless controls.
  ControlStyle := ControlStyle + [csOpaque];

  // Defaults (important: these were previously uninitialized before first use)
  fEnabled := False;
  fBackGroundColor := clDkGray;
  fBarColor := clSkyBlue;
  fPeakDirection := pdVertical;
  fMeterChannel := mcLeft;
  fDataFlow := eRender;
  fRole := eMultimedia;
  lwTimerPeriod := 100;

  // Create the bitmap.
  fPeakMeterBmp := TBitmap.Create;

  // Use a VCL timer so we don't need our own hidden window proc/handle.
  // This keeps all COM calls on the VCL thread (same thread that paints).
  fTimer := TTimer.Create(Self);
  fTimer.Enabled := False;
  fTimer.Interval := lwTimerPeriod;
  fTimer.OnTimer := TimerTick;

  // Start with 0 peak
  fPeakLeftBits := SingleToBits(0.0);
  fPeakRightBits := SingleToBits(0.0);

  // No need to connect to endpoint at design-time.
  if not (csDesigning in ComponentState) then
    Invalidate;
end;


destructor TMfPeakMeter.Destroy();
begin

  if Assigned(fTimer) then
    fTimer.Enabled := False;

  ReleaseMeter();

  FreeAndNil(fPeakMeterBmp);

  inherited Destroy();
end;


procedure TMfPeakMeter.ReleaseMeter();
begin

  // Release COM interfaces
  pMeterInfo := nil;
  pDevice := nil;
  pEnumerator := nil;

  fChannelCount := 0;
  SetLength(fPeakValues, 0);

  // Reset snapshot
  InterlockedExchange(fPeakLeftBits,
                      SingleToBits(0.0));

  InterlockedExchange(fPeakRightBits,
                      SingleToBits(0.0));
end;


procedure TMfPeakMeter.DoError(const Hr: HRESULT);
begin

  // Disable metering on fatal errors and let the application decide what to do.
  if Assigned(fTimer) then
    fTimer.Enabled := False;

  // Keep COM objects around? safer to release and allow re-init later
  ReleaseMeter;

  if Assigned(fOnError) then
    fOnError(Self, Hr);
end;


class function TMfPeakMeter.SingleToBits(const V: Single): Integer;
begin

  Result := PInteger(@V)^;
end;


class function TMfPeakMeter.BitsToSingle(const B: Integer): Single;
begin

  Result := PSingle(@B)^;
end;


class function TMfPeakMeter.Clamp_(const V: Single): Single;
begin

  if (V < 0) then
    Exit(0.0);
  if (V > 1) then
    Exit(1.0);
  Result := V;
end;


function TMfPeakMeter.EnsureMeterReady: Boolean;
var
  Hr: HRESULT;

begin

  Result := Assigned(pMeterInfo);
  if Result then
    Exit;

  // Create MMDeviceEnumerator
  Hr := CoCreateInstance(CLSID_MMDeviceEnumerator,
                         nil,
                         CLSCTX_ALL,
                         IID_IMMDeviceEnumerator,
                         pEnumerator);
  if Failed(Hr) then
    begin

      DoError(Hr);
      Exit(False);
    end;

  // Default endpoint (render/capture + role)
  Hr := pEnumerator.GetDefaultAudioEndpoint(fDataFlow,
                                            fRole,
                                            pDevice);
  if Failed(Hr) then
    begin

      DoError(Hr);
      Exit(False);
    end;

  // Activate IAudioMeterInformation
  Hr := pDevice.Activate(IID_IAudioMeterInformation,
                         CLSCTX_ALL,
                         nil,
                         Pointer(pMeterInfo));
  if Failed(Hr) then
    begin

      DoError(Hr);
      Exit(False);
    end;

  // Channel count
  Hr := pMeterInfo.GetMeteringChannelCount(fChannelCount);
  if Failed(Hr) then
    begin

      DoError(Hr);
      Exit(False);
    end;

  if (fChannelCount = 0) then
    begin

      DoError(E_FAIL);
      Exit(False);
    end;

  // Prepare reusable buffer once
  SetLength(fPeakValues,
            fChannelCount);
  Result := True;
end;


procedure TMfPeakMeter.TimerTick(Sender: TObject);
var
  Hr: HRESULT;
  PeakL,
  PeakR: Single;
  ChBitsL,
  ChBitsR: Integer;

begin

  if (csDesigning in ComponentState) then
    Exit;

  if not fEnabled then
    Exit;

  if not EnsureMeterReady then
    Exit;

  // Query peaks
  Hr := pMeterInfo.GetChannelsPeakValues(fChannelCount,
                                         @fPeakValues[0]);
  if Failed(Hr) then
    begin

      DoError(Hr);
      Exit;
    end;

  // Note: Default mapping: [0] = Left, [1] = Right.
  // Mono devices always expose a single channel at index 0 ("Left" by convention).
  if (fChannelCount = 1) then
    begin
      PeakL := Clamp_(fPeakValues[0]);

      // Publish snapshot atomically (bitwise).
      ChBitsL := SingleToBits(PeakL);
      InterlockedExchange(fPeakLeftBits, ChBitsL);

      // Mono: mirror into "Right" so SampleChannel=mcRight still shows the same signal.
      InterlockedExchange(fPeakRightBits, ChBitsL);
    end
  else
    begin
      // Only publish the channel currently requested by SampleChannel to minimize work.
      if (fMeterChannel = mcLeft) then
        begin
          PeakL := Clamp_(fPeakValues[0]);
          ChBitsL := SingleToBits(PeakL);
          InterlockedExchange(fPeakLeftBits, ChBitsL);
        end
      else
        begin
          PeakR := Clamp_(fPeakValues[1]);
          ChBitsR := SingleToBits(PeakR);
          InterlockedExchange(fPeakRightBits, ChBitsR);
       end;
    end;

  // Trigger repaint (async). Do NOT call Invalidate from inside Paint/DrawPeakMeter.
  Invalidate;

      if (fMeterChannel = mcRight) then
        begin

          PeakR := Clamp_(fPeakValues[1]);
          // Publish snapshot atomically (bitwise).
          ChBitsR := SingleToBits(PeakR);
          InterlockedExchange(fPeakRightBits,
                      ChBitsR);
        end;
end;


procedure TMfPeakMeter.DrawPeakMeter();
var
  R: TRect;
  Peak: Single;
  B: Integer;

begin

  // Read snapshot atomically.
  if (fMeterChannel = mcRight) then
    B := InterlockedCompareExchange(fPeakRightBits,
                                    0,
                                    0)
  else
    B := InterlockedCompareExchange(fPeakLeftBits,
                                    0,
                                    0);

  Peak := Clamp_(BitsToSingle(B));
  // Resize backing bitmap only when needed (avoid realloc/flicker)
  if (fPeakMeterBmp.Width <> Width) or (fPeakMeterBmp.Height <> Height) then
    fPeakMeterBmp.SetSize(Width, Height);

  // Background
  R := Rect(0,
            0,
            Width,
            Height);

  fPeakMeterBmp.Canvas.Pen.Style := psClear;
  fPeakMeterBmp.Canvas.Brush.Style := bsSolid;
  fPeakMeterBmp.Canvas.Brush.Color := fBackGroundColor;
  fPeakMeterBmp.Canvas.FillRect(R);

  // Bar
  if (Width > 0) and (Height > 0) then
    begin

      if (fPeakDirection = pdHorizontal) then
        begin

          R.Right := R.Left + Round(Peak * Width);
        end
      else
        begin

          // vertical: fill from bottom
          R.Top := R.Bottom - Round(Peak * Height);
        end;

      fPeakMeterBmp.Canvas.Brush.Color := fBarColor;
      fPeakMeterBmp.Canvas.FillRect(R);
  end;

  // Blit once
  Canvas.Draw(0,
              0,
              fPeakMeterBmp);

  // No Invalidate here: repaint is driven by the timer / property changes.
end;


procedure TMfPeakMeter.Paint();
begin

  DrawPeakMeter();

  inherited;
end;


procedure TMfPeakMeter.Resize();
begin

  inherited;

  Invalidate();
end;


procedure TMfPeakMeter.SetBackGroundColor(value: TColor);
begin

  if (fBackGroundColor <> value) then
    begin

      fBackGroundColor := value;
      Invalidate();
    end;
end;


procedure TMfPeakMeter.SetBarColor(value: TColor);
begin

  if (fBarColor <> value) then
    begin

      fBarColor := value;
      Invalidate();
    end;
end;


procedure TMfPeakMeter.SetDirection(value: TPeakDirection);
begin

  if (fPeakDirection <> value) then
    begin

      fPeakDirection := value;
      Invalidate();
    end;
end;


procedure TMfPeakMeter.SetPeakMeterChannel(value: TPeakMeterChannel);
begin

  if (fMeterChannel <> value) then
    begin

      fMeterChannel := value;
      Invalidate;
    end;
end;


procedure TMfPeakMeter.SetDeviceDataFlow(value: EDataFlow);
begin

  if fDataFlow <> value then
    begin

      fDataFlow := value;

      // Re-init if running
      if fEnabled and not (csDesigning in ComponentState) then
        begin

          ReleaseMeter();
          EnsureMeterReady();
        end;
    end;
end;


procedure TMfPeakMeter.SetDeviceRole(value: ERole);
begin

  if (fRole <> value) then
    begin

      fRole := value;

      // Re-init if running
      if fEnabled and not (csDesigning in ComponentState) then
        begin

          ReleaseMeter();
          EnsureMeterReady;
        end;
  end;
end;


procedure TMfPeakMeter.SetTimerPeriod(value: LongWord);
begin

  if lwTimerPeriod <> value then
    begin

      lwTimerPeriod := value;
      if Assigned(fTimer) then
      fTimer.Interval := lwTimerPeriod;
    end;
end;


procedure TMfPeakMeter.SetEnabled(value: Boolean);
begin

  if (fEnabled = value) then
    Exit;

  fEnabled := value;

  if (csDesigning in ComponentState) then
    Exit;

  if fEnabled then
    begin

      if EnsureMeterReady() then
        begin

          fTimer.Interval := lwTimerPeriod;
          fTimer.Enabled := True;
        end;
    end
  else
    begin

      if Assigned(fTimer) then
        fTimer.Enabled := False;

      // Reset peaks
      InterlockedExchange(fPeakLeftBits,
                          SingleToBits(0.0));

      InterlockedExchange(fPeakRightBits,
                          SingleToBits(0.0));

      Invalidate();
    end;

  inherited;
end;


function TMfPeakMeter.GetPeakValueThreadSafe(): Single;
var
  B: Integer;

begin

  if (fMeterChannel = mcRight) then
    B := InterlockedCompareExchange(fPeakRightBits,
                                    0,
                                    0)
  else
    B := InterlockedCompareExchange(fPeakLeftBits,
                                    0,
                                    0);

  Result := Clamp_(BitsToSingle(B));
end;

end.
