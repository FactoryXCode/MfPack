// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: MfPeakMeterEx.pas
// Kind: Pascal Unit Component
// Release date: 04-08-2016
// Language: ENU
//
// Version: 3.1.3
// Description: An extended Peakmeter component based on the Mf Peakmeter Sample.
//
// Company: FactoryX
// Intiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX), Peter Larson (ozships)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 28/08/2022 All                 PiL release  SDK 10.0.22621.0 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: To install the visual components, choose Install in the Project Manager.
//          Requires Windows 7 or later.
//
// Related objects: -
// Related projects: MfPackX313
// Known Issues: -
//
// Compiler version: 23 up to 35               q
// SDK version: 10.0.22621.0
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
// Groupname: FactoryX
// The Initial Developers of the Original Code are: Tony Kalf (maXcomX)
//                                                  Peter Larson (ozships)
//                                                  Ramyses De Macedo Rodrigues
// Contributor(s): Tony Kalf (maXcomX),
//                 Peter Larson (ozships),
//
//
// Users may distribute this source code provided that this header is included
// in full at the top of the file.
//
//==============================================================================

unit MfPeakMeterEx;

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
  WinApi.CoreAudioApi.EndPointVolume,
  {Additional}
  QueueTimer;

const
  // Timer period (in milliseconds)
  TIMER_PERIOD = 100;


type
  TMfPeakMeterExStyle = (dsVertical, dsHorizontal);
  TMfPeakMeterExDirection = (ddRightDown, ddLeftUp);
  TMfPeakMeterExChannel = (mcLeft, mcRight);


  TMfPeakMeterEx = class(TGraphicControl)
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

    FTimer: TQTimer;
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
    procedure PaintBar;
    function CreateEngine(): HRESULT;
    procedure Paint; override;
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
    property IntTimer: TQTimer read FTimer write FTimer;
    property Precision: Cardinal read fSafeTimerInterval write SetSafeTimerInterval default TIMER_PERIOD;
    property Enabled: Boolean read fEnabled write SetEnabled default False;
    property UseGraphicsOnly: Boolean read fGraphicsOnly write SetGraphicsOnly default False;
end;


procedure Register;


implementation


procedure Register;
begin
  RegisterComponents('MfPack Core Audio Samples', [TMfPeakMeterEx]);
end;


constructor TMfPeakMeterEx.Create(aOwner: Tcomponent);
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
  fColors[1, True] := clLime;
  fColors[2, True] := clYellow;
  fColors[3, True] := clRed;
  fColors[1, False] := clGreen;
  fColors[2, False] := clOlive;
  fColors[3, False] := clMaroon;
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

  pEnumerator := Nil;
  pDevice := Nil;
  pMeterInfo := Nil;

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
                   MB_OK Or MB_ICONSTOP);
      end;
end;


destructor TMfPeakMeterEx.Destroy;
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
      CoUninitialize();
    end;
  // Destroy handle
  DeallocateHWnd(fHWnd);
  inherited Destroy;
end;


function TMfPeakMeterEx.CreateEngine(): HRESULT;
var
  hr: HResult;

label
  done;

begin
  CoInitializeEx(Nil,
                 COINIT_APARTMENTTHREADED);

  // Single instance
  hr := CoCreateInstance(CLSID_MMDeviceEnumerator,
                         Nil,
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
                         Nil,
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

  // Create and activate a qtimer
  FTimer := TQTimer.Create(Self);
  FTimer.DueTime := 0;  // Immediate
  FTimer.Period := TIMER_PERIOD;
  FTimer.OnTimerEvent := TimerTimer;

done:
  Result := hr;
end;


// Using a regular TTimer is less precise, but can do as well
procedure TMfPeakMeterEx.TimerTimer(sender: TObject);
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
      else if (fMeterChannel = mcRight) And (fChannelCount = 2) then
        sPeakValue := afPeakValues[1]
      else
        sPeakValue := afPeakValues[0]; // fall back to default (mono = always left channel)
    end;
    Paint;
end;


procedure TMfPeakMeterEx.SetSafeTimerInterval(value: Cardinal);
begin
  // If running, user can't change this value
  if (fPeakMeterRunning = True) then
    Exit;

  if (value < 10) or (value > 10000) then
    value := 10; //reset to default
  fSafeTimerInterval := value;
  FTimer.Period := fSafeTimerInterval;
end;


procedure TMfPeakMeterEx.SetGraphicsOnly(value: Boolean);
begin
  // If running, user can't change this value
  if (fGraphicsOnly <> value) And (fPeakMeterRunning = False) then
    fGraphicsOnly := value;
end;


procedure TMfPeakMeterEx.SetDeviceDataFlow(value: EDataFlow);
begin
  if (fDataFlow <> value) then
    fDataFlow := value;
end;


procedure TMfPeakMeterEx.SetDeviceRole(value: ERole);
begin
  if (fRole <> value) then
    fRole := value;
end;


procedure TMfPeakMeterEx.SetEnabled(value: Boolean);
begin

  if (fEnabled <> value) then
    fEnabled := value;
  // The timer will not be created if an error occured during
  // initialisation
  if (Ftimer <> Nil) then
    Ftimer.Enabled := fEnabled;

  inherited
end;


function TMfPeakMeterEx.CalculateX(X: Integer): Integer;
begin
  Result := (Width - X) - 1;
end;


function TMfPeakMeterEx.CalculateY(Y: Integer): Integer;
begin
  Result := (Height - Y) - 1;
end;


procedure TMfPeakMeterEx.SetBevelWidth(value: Byte);
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


procedure TMfPeakMeterEx.SetBevelStyle(value: TPanelbevel);
begin
  if (value <> fBevelStyle) then
    begin
      fBevelStyle := value;
      Paint;
    end;
end;


procedure TMfPeakMeterEx.SetGreenColorOn(value: TColor);
begin
  if (value <> fColors[1, True]) then
    begin
      fColors[1, True] := value;
      Paint;
    end;
end;


procedure TMfPeakMeterEx.SetGreenMax(value: Integer);
begin
  if (value <> fGreenMax) then
    begin
      fGreenMax := value;
      Paint;
    end;
end;


procedure TMfPeakMeterEx.SetgreenLeds(value: Integer);
begin
  if (value <> fGreenLeds) then
    begin
      fGreenLeds := value;
      Paint;
    end;
end;


procedure TMfPeakMeterEx.SetGreenColorOff(value: TColor);
begin
  if (value <> fColors[1, False]) then
    begin
      fColors[1, False] := value;
      Paint;
    end;
end;


procedure TMfPeakMeterEx.SetYellowColorOn(value: TColor);
begin
  if (value <> fColors[2, True]) then
    begin
      fColors[2, True] := value;
      Paint;
    end;
end;


procedure TMfPeakMeterEx.SetYellowMax(value: Integer);
begin
  if (value <> fYellowMax) then
    begin
      fYellowMax := value;
      Paint;
    end;
end;


procedure TMfPeakMeterEx.SetYellowLeds(value: Integer);
begin
  if (value <> fYellowLeds) then
    begin
      fYellowLeds := value;
      Paint;
    end;
end;


procedure TMfPeakMeterEx.SetYellowColorOff(value: TColor);
begin
  if value <> fColors[2, False] then
    begin
      fColors[2, False] := value;
      Paint;
    end;
end;


procedure TMfPeakMeterEx.SetRedColorOn(value: TColor);
begin
  if (value <> fColors[3, True]) then
    begin
      fColors[3, True] := value;
      Paint;
    end;
end;


procedure TMfPeakMeterEx.SetRedMax(value: Integer);
begin
  if (value <> fRedMax) then
    begin
      fRedmax := value;
      Paint;
    end;
end;


procedure TMfPeakMeterEx.SetRedLeds(value: Integer);
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


procedure TMfPeakMeterEx.SetRedColorOff(value: TColor);
begin
try
  if (value <> fcolors[3, False]) then
    begin
        fColors[3, False] := value;
        Paint;
    end;
except
  on Exception do Exit;
end;
end;


procedure TMfPeakMeterEx.SetSingleLed(value: Boolean);
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


procedure TMfPeakMeterEx.SetSeparatorSpacing(value: Integer);
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


procedure TMfPeakMeterEx.SetSeparatorColor(value: TColor);
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


procedure TMfPeakMeterEx.SetStyle(value: TMfPeakMeterExStyle);
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


procedure TMfPeakMeterEx.SetDirection(value: TMfPeakMeterExdirection);
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


procedure TMfPeakMeterEx.SetPeakValue(value: Single);
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


procedure TMfPeakMeterEx.SetPeakMeterChannel(value: TMfPeakMeterExChannel);
begin
  if (fMeterChannel <> value) then
    fMeterChannel := value;
end;


// Calculates the peak value returned from a device (0.0 - 1.0) to an integer
procedure TMfPeakMeterEx.CalculatePeakValue;
begin
  if (fEnabled = False) and (fPeakMeterRunning = true) then
    iPeakValue := 0
  else
    iPeakValue := Round(sPeakValue * (fRedMax + fYellowMax + fGreenMax));
end;


function TMfPeakMeterEx.GetLastLedPos(value: Integer): Integer;
var
  num: Integer;
  ye: Integer;
  gr: Integer;

begin

  CalculatePeakValue;

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


procedure TMfPeakMeterEx.WindProc(var Msg: TMessage);
var
  Handled: Boolean;

begin

  Handled := True;

  case Msg.Msg of
    WM_PAINT: Paint;
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


procedure TMfPeakMeterEx.PaintBar;
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
  ax: Array [0..3] of Integer;
  ay: Array [0..3] of Integer;
  i: Integer;

begin

try
  fBmp.Width := Width;
  fBmp.Height := Height;

  //Initiate the x and y coordinates
  for i := Low(ax) to High(ax) do
    begin
      ax[i] := 0;
      ay[i] := 0;
    end;


  with fBmp.Canvas do
    begin
      Pen.Color := fSeparatorColor;
      Pen.Width := 0;
      Pen.Style := psSolid;
      Brush.Color := fSeparatorColor;
      Brush.Style := bsSolid;
      Rectangle(0,
                0,
                Width,
                Height);
     end;

  //Calculate the number of leds
  ileds := (fGreenLeds + fRedLeds + fYellowLeds);

  if (ileds > 0) then
    begin
      // Calculate the width
      ibarwidth := (Width div ileds);
      ibarheight := Height;

      if (fStyle = dsVertical) then
        begin
          ibarwidth := Height div ileds;
          ibarheight := Width;
        end;

      if (ibarwidth > fSeparatorSpacing) then
        begin
          // Calculate the last led
          inum := GetLastLedPos(ileds);
          if (inum = 0) then
            if (iPeakValue <> 0) then
              inum := 1;

          // Set colors
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
          // Bottom right
          Pen.Color := tcBottom;
          for lp := 0 to bw - 1 do
            begin
              moveto(CalculateX(Width), CalculateY(lp));
              lineto(CalculateX(lp), CalculateY(lp));
              lineto(CalculateX(lp), CalculateY(Height));
            end;

          // Top left
          Pen.Color := tcTop;
          for lp  := 0 to bw - 1 do
            begin
              MoveTo(Width, lp);
              LineTo(lp, lp);
              LineTo(lp, Height - bw);
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


procedure TMfPeakMeterEx.Paint;
begin
  PaintBar;
  inherited
end;

end.

