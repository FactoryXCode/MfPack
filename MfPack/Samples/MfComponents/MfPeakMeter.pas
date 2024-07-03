// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: MfPeakMeter.pas
// Kind: Pascal Unit Component
// Release date: 04-08-2016
// Language: ENU
//
// Version: 3.1.6
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
// 30/06/2024 All                 RammStein release  SDK 10.0.26100.0 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: To install the visual components, choose Install in the Project Manager.
//          Requires Windows 7 or later.
//
// Related objects: -
// Related projects: MfPackX317
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.26100.0
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

const
  ID_TIMER = 1;

type
  TPeakDirection = (pdVertical, pdHorizontal);
  TPeakMeterChannel = (mcLeft, mcRight);

  TMfPeakMeter = class(TGraphicControl)
  private
    { private fields }
    fPeakMeterBmp: TBitmap;
    fBackGroundColor: TColor;
    fBarColor: TColor;
    afPeakValues: array of Float;
    fChannelCount: UINT;
    fPeakDirection: TPeakDirection;
    fMeterChannel: TPeakMeterChannel;
    lwTimerPeriod: LongWord; // Timer period (in milliseconds)
    // Interfaces
    pEnumerator: IMMDeviceEnumerator;
    pDevice: IMMDevice;
    pMeterInfo: IAudioMeterInformation;

    fDataFlow: EDataFlow; // The data-flow direction for the endpoint device.
    fRole: ERole;         // The role of the endpoint device.

    { private methods }
    procedure DrawPeakMeter;
    procedure SetBackGroundColor(value: TColor);
    procedure SetBarColor(value: TColor);
    procedure SetDirection(value: TPeakDirection);
    procedure SetPeakMeterChannel(value: TPeakMeterChannel);
    procedure SetDeviceDataFlow(value: EDataFlow);
    procedure SetDeviceRole(value: ERole);
    procedure SetTimerPeriod(value: LongWord);

  protected
    { protected fields }
    fHwnd: HWnd; // Handle to this meter.
    fEnabled: Boolean;

    { protected methods }
    procedure WindProc(var Msg: TMessage); virtual;
    procedure SetEnabled(value: Boolean); override;
    procedure Paint; override;

  public
    { public fields }


    { public methods }
    constructor Create(aOwner: Tcomponent); override;
    destructor Destroy(); override;

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

    property Enabled: Boolean read fEnabled write SetEnabled default False;
    property BackGroundColor: TColor read fBackGroundColor write SetBackGroundColor;
    property BarColor: TColor read fBarColor write SetBarColor;
    property Direction: TPeakDirection read fPeakDirection write SetDirection;
    property SampleChannel: TPeakMeterChannel read fMeterChannel write SetPeakMeterChannel;
    property DeviceDataFlow: EDataFlow read fDataFlow write SetDeviceDataFlow default eRender;
    property DeviceRole: ERole read fRole write SetDeviceRole default eMultimedia;
    property Precision: LongWord read lwTimerPeriod write SetTimerPeriod default 100;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('MfPack Core Audio Samples', [TMfPeakMeter]);
end;


//-----------------------------------------------------------
// constructor -- creates a bitmap that contains a peak meter.
//   The peak meter displays the peak sample value that plays
//   through the default rendering device.
//-----------------------------------------------------------
constructor TMfPeakMeter.Create(aOwner: Tcomponent);
var
  hr: HResult;

label
  done;

begin
  inherited Create(aOwner);

  // Create the bitmap
  fPeakMeterBmp := TBitmap.Create();
  fBackGroundColor := clDkGray;
  fBarColor := clSkyBlue;

  // Create the handle for this component
  fHWnd := AllocateHWnd(WindProc);

  //CoInitializeEx(nil,
  //               COINIT_APARTMENTTHREADED);

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
                         nil,
                         Pointer(pMeterInfo));
  if FAILED(hr) then
    goto done;

  // Get the number of channels
  hr := pMeterInfo.GetMeteringChannelCount(fChannelCount);
  if FAILED(hr) then
    goto done;

  Paint;

done:
  if ((csDesigning in ComponentState) = False) then
    if (FAILED(hr)) then
      begin
        MessageBox(0,
                   LPCWSTR('An error occured.  Error: ' + IntToStr(hr)) ,
                   LPCWSTR('Error termination'),
                   MB_OK And MB_ICONSTOP);
      end;

end;


destructor TMfPeakMeter.Destroy();
begin
  KillTimer(fHwnd,
            ID_TIMER);

  FreeAndNil(fPeakMeterBmp);

  pEnumerator := nil;
  pDevice := nil;
  pMeterInfo := nil;

  //CoUninitialize();

  // Destroy handle
  DeallocateHWnd(fHWnd);
  inherited Destroy;
end;



//-----------------------------------------------------------
// DrawPeakMeter -- Draws the peakmeter on the bitmap.
//-----------------------------------------------------------
procedure TMfPeakMeter.DrawPeakMeter;
var
  bmrect: TRect;
  sPeakValue: Single;

begin
  sPeakValue := 0.0;

  if (Length(afPeakValues) > 0) then
    begin
      // If there's only one channel, the default would always be the leftchannel (also in mono)
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

  fPeakMeterBmp.Width := Width;
  fPeakMeterBmp.Height := Height;

  bmrect.left := 0;
  bmrect.top := 0;
  bmrect.right := Width;
  bmrect.bottom := Height;

  with fPeakMeterBmp.Canvas do
    begin
      Pen.Color := fBackGroundColor;
      Pen.Width := 0;
      Pen.Style := psSolid;
      Brush.Color := fBackGroundColor;
      Brush.Style := bsSolid;
      Rectangle(0,
                0,
                Width,
                Height);

     end;

  canvas.draw(0,
              0,
              fPeakMeterBmp);

  bmrect.left := 0;
  bmrect.top := 0;
  bmrect.bottom := Height;

  if (Length(afPeakValues) > 0) then
    begin
      if (fPeakDirection = pdHorizontal) then
        bmrect.right := bmrect.left + Trunc(sPeakValue * (bmrect.right - bmrect.left) - 1.5)
      else
        bmrect.Top := bmrect.Bottom + Trunc(sPeakValue * (bmrect.Top - bmrect.Bottom) - 1.5);

      fPeakMeterBmp.Canvas.Brush.Color := fBarColor;
      fPeakMeterBmp.Canvas.FillRect(bmrect);
    end
  else
    bmrect.right := Width;

  canvas.draw(0,
              0,
              fPeakMeterBmp);
end;


procedure TMfPeakMeter.SetBackGroundColor(value: TColor);
begin
  if (fBackGroundColor <> value) then
    begin
      fBackGroundColor := value;
      Paint;
    end;
end;


procedure TMfPeakMeter.SetBarColor(value: TColor);
begin
  if (fBarColor <> value) then
    begin
      fBarColor := value;
      Paint;
    end;
end;


procedure TMfPeakMeter.SetDirection(value: TPeakDirection);
begin
  if (fPeakDirection <> value) then
    begin
      fPeakDirection := value;
      Paint;
    end;
end;


procedure TMfPeakMeter.SetPeakMeterChannel(value: TPeakMeterChannel);
begin
  if (fMeterChannel <> value) then
    fMeterChannel := value;
end;


procedure TMfPeakMeter.SetDeviceDataFlow(value: EDataFlow);
begin
  if (fDataFlow <> value) then
    fDataFlow := value;
end;


procedure TMfPeakMeter.SetDeviceRole(value: ERole);
begin
  if (fRole <> value) then
    fRole := value;
end;


procedure TMfPeakMeter.SetTimerPeriod(value: LongWord);
begin
  if (lwTimerPeriod <> value) then
    lwTimerPeriod := value;
end;


procedure TMfPeakMeter.WindProc(var Msg: TMessage);
var
  hr: HResult;
  Handled: Boolean;

begin

  Handled := True;

  case Msg.Msg of
    WM_TIMER   :  case Msg.WParam of
                    ID_TIMER :  begin
                                  // Update the peak meter.
                                  // Set the length of the array to retrieve the samples.
                                  SetLength(afPeakValues,
                                            fChannelCount);
                                  hr := pMeterInfo.GetChannelsPeakValues(fChannelCount,
                                                                         @afPeakValues[0]);
                                  if (FAILED(hr)) then
                                    begin
                                      MessageBox(fHwnd,
                                                 LPCWSTR('The peakmeter will stop.'),
                                                 LPCWSTR('Fatal error'),
                                                 MB_OK);
                                      KillTimer(fHwnd,
                                                ID_TIMER);
                                    end
                                  else
                                    begin
                                      Paint;
                                    end;
                                end;
                  end;

    WM_PAINT   :  begin
                    DrawPeakMeter;
                  end;
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


procedure TMfPeakMeter.SetEnabled(value: Boolean);
begin
  if (fEnabled <> value) then
    fEnabled := value;

  if (fEnabled = True) then
    begin
      SetTimer(fHwnd,
               ID_TIMER,
               lwTimerPeriod,
               Nil);
    end
  else
    begin
      KillTimer(fHwnd,
                ID_TIMER);
    end;

  inherited
end;


procedure TMfPeakMeter.Paint;
begin
  DrawPeakMeter;
  inherited
end;

end.
