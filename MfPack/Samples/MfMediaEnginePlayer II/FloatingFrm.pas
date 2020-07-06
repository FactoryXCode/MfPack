// FactoryX
//
// Copyright © FactoryX, Netherlands/Australia
//
// Project: Media Foundation - MFPack - Samples
// Project location: http://sourceforge.net/projects/MFPack
// Module: FloatingFrm.pas
// Kind: Pascal Unit
// Release date: 24-04-2019
// Language: ENU
//
// Version: 2.6.4
// Description: Floating form for subtitles.
//
// Company: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX), Peter Larson (ozships),
//                 Ramyses De Macedo Rodrigues, (Ciaran).
//
// Rudy Velthuis 1960 ~ 2019.
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 28/05/2020                     Kraftwerk release. (WIN10 May 2020 update, version 2004)
//                                #1 Autobahn
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 or higher.
//
// Related objects: -
// Related projects: MfPackX264
// Known Issues: -
//
// Compiler version: 23 up to 33
// SDK version: 10.0.19041.0
//
// Todo: -
//
// =============================================================================
// Source: -
//
// Copyright © FactoryX. All rights reserved.
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
//
// Users may distribute this source code provided that this header is included
// in full at the top of the file.
//
//==============================================================================
unit FloatingFrm;

interface

uses
  {WinApi}
  WinApi.Windows,
  WinApi.Messages,
  {System}
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.Math,
  {Vcl}
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  {MfPack}
  MfPack.MfpUtils;

const

  WM_PARENTCHANGED = WM_USER + 1023;

  // Aspectratio's
  //////////////////////////////////////
  // Television
  AR_4_3   : Single = 1.333333333333333;
  AR_5_3   : Single = 1.666666666666667;
  // HDTV / WideScreen
  AR_16_9  : Single = 1.777777777777778;
  // Mobile phones
  AR_9_16  : Single = 0.5625;
  AR_18_9  : Single = 2.0;   // Univisium format: Google Pix, LG, Huawei etc.
  AR_195_9 : Single = 2.16;  // Apple, Samsung
  // Cinema
  AR_186_1 : Single = 1.86;
  AR_21_9  : Single = 2.37037037037037; // = 64:27 !
  AR_235_1 : Single = 2.35; // European cinema format
  AR_239_1 : Single = 2.39;
  AR_24_1  : Single = 2.4;


type
  // Screen capabillities
  TScrRes = record
    ScrResH: Integer;      // Horizontal width in pixels
    ScrResV: Integer;      // Vertical height in pixels
    LogPixelsX: Integer;   // Logical pixelsinch in X
    LogPixelsY: Integer;   // Logical pixelsinch in Y
  end;

  TFloatingForm = class(TForm)

    lblSubTitle: TLabel;

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);

  protected
    procedure CreateParams(var Params: TCreateParams); override;

  private
    { Private declarations }

    crTransparency: TColor;   // transparency key color
    btOpacity: Byte;          // opacity 0 - 255
    hwParent: HWnd;           // Parent handle
    sSubTitleTxt: string;     // Subtitle string that needs to be displayed
    iBottomMargin: Integer;   // Vertical margin between text and bottom of the form
    iTextLines: Integer;      // Number of textlines to be displayed
    fLayerFont: TFont;        // The subtitlefont  (recommended is the BBC standard Tiresias™, free Font by The Royal National Institute for the Blind)

    procedure SetSubTitleText(aValue: string);
    procedure GetTextLines(aValue: string);  // use this property to get the number of textlines
    procedure SetFormPosition();  // Set form position if auto
    procedure SetFont(aValue: TFont);
    procedure SetBottomMargin(aValue: Integer);
    function GetScreenResolution(): TScrRes;
    procedure OnParentChanged(var message: TMessage); message WM_PARENTCHANGED;
    procedure CalculateFontSize();
    procedure WMSize(var message: TWMSize); message WM_SIZE;

  public
    { Public declarations }
    ParentRect: TRect;        // rect of the parent window
    ParentPosition: TPoint;   // parent window position

  published
    // You need to set FullScreenMode from the caller when running fullscreen
    property Opacity: Byte read btOpacity;
    property SubtitleText: string read sSubTitleTxt write SetSubTitleText;
    property TextLines: Integer read iTextLines; //read only
    property LayerFont: TFont read fLayerFont write SetFont;
    property BottomMargin: Integer read iBottomMargin write SetBottomMargin default 0;
  end;

var
  FloatingForm: TFloatingForm;


implementation

{$R *.dfm}

const
  User32Lib = 'user32.dll';


procedure TFloatingForm.FormCreate(Sender: TObject);
begin
  // Initial settings
  btOpacity := 255;  // all controls on the form will be visible
  crTransparency := clBlack;
  hwParent := (Sender as TWinControl).Handle;
  fLayerFont := TFont.Create();
  // Set Font to it's default values
  fLayerFont.Size := 12;
  fLayerFont.Style := [fsBold];
  fLayerFont.Color := clWhite;
  FormStyle := fsStayOnTop; // Keep the form always on top
  BorderStyle := bsNone;
  Ctl3D := False;
  BorderIcons := [];
  AlphaBlend := True;
  AlphaBlendValue := btOpacity;
  TransparentColor := True;
  TransparentColorValue := clBlack;
  Show();
end;


procedure TFloatingForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(fLayerFont);
end;


procedure TFloatingForm.SetFormPosition();
begin
  // Note: We replace the top and left for the found position X and Y
  {void} SetWindowPos(Self.Handle,
                      HWND_TOPMOST,
                      ParentPosition.X,
                      ParentPosition.Y,
                      ParentRect.Width,
                      ParentRect.Height,
                      0);
end;


procedure TFloatingForm.SetSubTitleText(aValue: string);
begin
  sSubTitleTxt := aValue;
  GetTextLines(aValue);
  if Not Assigned(fLayerFont) then
    begin
      lblSubTitle.Font.Size  := 12;
      lblSubTitle.Font.Style := [fsBold];
      lblSubTitle.Font.Color := clWhite;
    end;
  lblSubTitle.Caption := sSubTitleTxt;
  Invalidate();
end;


procedure TFloatingForm.GetTextLines(aValue: string);
var
  i: integer;

begin
  iTextLines := 1; // default value
  // Get the textlines
  for i := 1 to Length(aValue)-1 do
    if aValue[i] = #13 then
      inc(iTextLines);
end;


procedure TFloatingForm.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);

  if not (csDesigning in ComponentState) then
    begin
      // Set the form click-through
      Params.ExStyle := WS_EX_LAYERED or WS_EX_TRANSPARENT;
    end;
end;


// Sets the font of the window (the label Parentfont has been set to True)
procedure TFloatingForm.SetFont(aValue: TFont);
begin
  Font := fLayerFont;
  lblSubTitle.Font := fLayerFont;
  Invalidate;
end;


procedure TFloatingForm.SetBottomMargin(aValue: Integer);
begin
  iBottomMargin := aValue;
  lblSubTitle.Margins.Bottom := iBottomMargin;
end;


function TFloatingForm.GetScreenResolution(): TScrRes;
var
  dc: THandle; // the Display Context

begin
  dc := GetDC(Handle);
  Result.ScrResH := GetDeviceCaps(dc,
                                  HORZRES);
  Result.ScrResV := GetDeviceCaps(dc,
                                  VERTRES);
  Result.LogPixelsX := GetDeviceCaps(dc,
                                     LOGPIXELSX);
  Result.LogPixelsY := GetDeviceCaps(dc,
                                     LOGPIXELSY);
  ReleaseDC(Self.Handle, dc);
end;


procedure TFloatingForm.OnParentChanged(var message: TMessage);
begin
  SetFormPosition();
end;


procedure TFloatingForm.CalculateFontSize();
const
  DEFAULTFONTSIZE = 100;

var
  screenRes: TScrRes; // screen resolution
  formsize: TRect;
  xd, xf: Single;
  perc: Single;

begin
  // Realtime video measurement: aspectratio 2,37 : 1 (which is the cinema format)
  // Calculate the fontsize as a percentage of the videosurface (aspectratio 16:9)
  // If the videosurfacesize is (H, W) 0,0 the percentage = 0
  // If the videosurfacesize is fullscreen the percentage is 100 or some less.
  //
  // So, first thing to do is calculate the maximum fullscreen size
  // For example: screensize = 1680 x 1050
  //              aspectratio = 16:9
  //              videosurfacesize = (1680 x 945), 100%, best readable fontsize will be approx int(40)
  //              videosurfacesize = (840 x 473) = 50%, best readable fontsize will be approx int(12)
  //              videosurfacesize = (420 x 236) = 25%, best readable fontsize will be approx int(8)

  // Get current screensize
  screenRes := GetScreenResolution();
  // Get the form size
  GetWindowRect(Self.Handle,
                formsize);

  xd := (formsize.Width / screenRes.ScrResH) * 100;
  xf := (formsize.Height / screenRes.ScrResV) * 100;
  perc := (xf + xd) / 2;

  if (perc >= 100) then
    lblSubTitle.Font.Size := 40
  else if InRange(perc, 90, 100) then
    lblSubTitle.Font.Size := 38
  else if InRange(perc, 80, 90) then
    lblSubTitle.Font.Size := 32
  else if InRange(perc, 70, 80) then
    lblSubTitle.Font.Size := 28
  else if InRange(perc, 60, 70) then
    lblSubTitle.Font.Size := 22
  else if InRange(perc, 50, 60) then
    lblSubTitle.Font.Size := 22
  else if InRange(perc, 40, 50) then
    lblSubTitle.Font.Size := 18
  else if InRange(perc, 30, 40) then
    lblSubTitle.Font.Size := 16
  else if InRange(perc, 20, 30) then
    lblSubTitle.Font.Size := 14
  else if InRange(perc, 0, 20) then
    lblSubTitle.Font.Size := 12;
  // test
  //lblSubTitle.Caption := IntToStr(lblSubTitle.Font.Size) + ' - ' + FloatToStr(perc) + ' %';
end;


// Here the fontsize will be adjusted when the formsize is changing.
procedure TFloatingForm.WMSize(var message: TWMSize);
begin
  inherited;
  CalculateFontSize();
end;

end.
