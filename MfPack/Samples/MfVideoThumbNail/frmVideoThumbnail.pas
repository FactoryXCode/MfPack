// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - Shared
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: frmVideoThumbnail.pas
// Kind: Pascal / Delphi unit
// Release date: 08-07-2012
// Language: ENU
//
// Revision Version: 3.1.4
// Description: Videothumbnail Mainform
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX), Peter Larson (ozships)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 28/08/2022 All                 PiL release  SDK 10.0.22621.0 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 7 or higher.
//
// Related objects: -
// Related projects: MfPackX314
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.20348.0
//
// Todo: -
//
//==============================================================================
// Source: Microsoft samples.
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
unit frmVideoThumbnail;

interface

uses
  {Winapi}
  Winapi.Windows,
  Winapi.Messages,
  WinApi.ComBaseApi,
  WinApi.WinApiTypes,
  {System}
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.Types,
  {Vcl}
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.Menus,
  {DirectX}
  WinApi.DirectX.DXGIType,
  WinApi.DirectX.D2D1,
  WinApi.DirectX.D2D1Helper,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfApi,
  WinApi.MediaFoundationApi.MfUtils,
  {ActiveX}
  WinApi.ActiveX.ObjBase,
  {Project}
  SimpleTimer,
  Thumbnail,
  Sprite,
  VideoTumbNailHelpers;

const
  // Constants
  WM_DONE_ONSHOW = WM_USER + 300;
  ANIMATION_DURATION = 0.4;

type
  TForm1 = class(TForm)
    dlgOpen: TOpenDialog;
    MainMenu: TMainMenu;
    File1: TMenuItem;
    OpenFile1: TMenuItem;
    Exit1: TMenuItem;
    procedure OpenFile1Click(Sender: TObject);
    function MessageLoop(hw: HWND): INT;
    procedure FormShow(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Exit1Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);

  private
    { Private declarations }
    bTerminate: Boolean;

    function InitializeApp(): Boolean;
    procedure CleanUp();

    // Callbacks
    procedure WmAfterShow(var Msg: TMessage); message WM_DONE_ONSHOW;
    procedure OnSize(var Message: TWMSize); message WM_SIZE;
    procedure OnPaint(var message: TMessage); message WM_PAINT;

  public
    { Public declarations }
  end;

var
  Form1: TForm1;

  // Global variables
  g_pRT: ID2D1HwndRenderTarget; // Render target for D2D animation
  g_pSprites: TSpritesArray;
  g_Selection: Integer = -1;    // Which sprite is selected (-1 = no selection)

  // The following rectangles define the small (unselected) and
  // big (selected) locations for the sprites.
  g_rcSmall: array[0..MAX_SPRITES] of D2D1_RECT_F; // TrcSmallThumbs;
  g_rcBig: D2D1_RECT_F; // TrcBigThumb;

  //
  //  DPI Scaling logic - helper functions to allow us to respect DPI settings.
  //
  g_fDPIScaleX: FLOAT = 1.0;
  g_fDPIScaleY: FLOAT = 1.0;
  BACKGROUND_COLOR: D3DCOLORVALUE;

  // Helpers
  ///////////////////////////////////////////////////////////////////

  procedure InitializeDPIScale(hw: HWND);
  function DPIScaleX(iValue: Integer): Integer;
  function DPIScaleY(iValue: Integer): Integer;
  procedure UnselectAllSprites();
  procedure SelectSprite(iSelection: Integer);
  function CreateDrawingResources(hw: HWND): HRESULT;
  function RenderFrame(hw: HWND): HRESULT;

  // Delphi specific
  procedure InitSmallRects();
  procedure InitBigRect();

  ///////////////////////////////////////////////////////////////////


implementation


{$R *.dfm}


procedure TForm1.Exit1Click(Sender: TObject);
begin
  Close();
end;


procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := False;
  CleanUp();
  CanClose := True;
end;


procedure TForm1.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  i: Integer;
  pSprite: TSprite;
  _x, _y: Integer;

begin
  // Scale for DPI
  _x := round(X / g_fDPIScaleX);
  _y := round(Y / g_fDPIScaleY);

  for i := 0 to MAX_SPRITES do
    begin
      // Hit-test each sprite.
      pSprite := g_pSprites[i];
      if (Assigned(pSprite) and pSprite.HitTest(_x, _y)) then
        begin
          SelectSprite(i);
          Break;
        end;
    end;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  // We have to initialize after the form is shown.
  // Post the custom message WM_DONE_ONSHOW
  PostMessage(Form1.Handle,
              WM_DONE_ONSHOW,
              0,
              0);
end;

//
procedure TForm1.WmAfterShow(var Msg: TMessage);
begin

  // Initialize the DPI scalar.
  InitializeDPIScale(Self.Handle);

  // Initialize the thumb rects
  InitSmallRects();
  InitBigRect();

  // Initialize
  if InitializeApp() then
    begin
       MessageLoop(Self.Handle);
    end;
end;


//-------------------------------------------------------------------
// WM_SIZE handler.
//-------------------------------------------------------------------
procedure TForm1.OnSize(var Message: TWMSize);
var
  d2d1su: D2D1_SIZE_U;

begin

  if (g_pRT <> nil) then
    begin

      d2d1su := D2D1SizeU(Form1.Width,
                          Form1.Height);

      {void} g_pRT.Resize(d2d1su);

      InvalidateRect(Form1.Handle,
                     Nil,
                     FALSE);
    end;
end;


//-------------------------------------------------------------------
// WM_PAINT handler.
//-------------------------------------------------------------------
procedure TForm1.OnPaint(var message: TMessage);
var
  ps: PAINTSTRUCT;
  //dc: HDC;

begin
  inherited Paint();
  BeginPaint(Self.Handle,
             ps);

  RenderFrame(Self.Handle);

  EndPaint(Self.Handle,
           ps);
end;


//-------------------------------------------------------------------
// InitializeApp: Initializes the application.
//-------------------------------------------------------------------
function TForm1.InitializeApp(): Boolean;
var
  hr: HResult;
  i: Integer;

begin

  bTerminate := False;

  // Initialize COM
  hr := CoInitializeEx(Nil,
                       COINIT_APARTMENTTHREADED OR COINIT_DISABLE_OLE1DDE);

  if (SUCCEEDED(hr)) then
    begin
      // Initialize Media Foundation.
      hr := MFStartup(MF_VERSION{,
                      MFSTARTUP_FULL}); // The default is MFSTARTUP_FULL (0), so there is no need to add this parameter.
    end;

  // Set the background color
  BACKGROUND_COLOR := D2D1TColorF($2F4F4F);

  for i := 0 to MAX_SPRITES do
    begin
      g_pSprites[i] := TSprite.Create();
      g_pSprites[i].m_SpriteIndex := i+1; // non-zerobased index
    end;

  // initialize ThumbnailGenerator & sprites
  g_ThumbnailGen := TThumbnailGenerator.Create();

  if (SUCCEEDED(hr)) then
    begin
      g_Timer := TSimpleTimer.Create();
      if Assigned(g_Timer) then
        begin
          // Start the clock
          g_Timer.InitializeTimer(30);
        end;
    end;

  Result := (SUCCEEDED(hr));
end;


//-------------------------------------------------------------------
// Releases resources
//-------------------------------------------------------------------

procedure TForm1.CleanUp();
var
  i: Integer;
begin
  bTerminate := True;

  if assigned(g_ThumbnailGen) then
    begin
      FreeAndNil(g_ThumbnailGen);
    end;

  if assigned(g_Timer) then
    begin
      FreeAndNil(g_Timer);
    end;

  SafeRelease(g_pRT);

  for i := 0 to MAX_SPRITES do
    begin
      g_pSprites[i].Clear();
      FreeAndNil(g_pSprites[i]);
    end;

end;


//-------------------------------------------------------------------
// OpenVideoFile: Opens a new video file and creates thumbnails.
//-------------------------------------------------------------------
procedure TForm1.OpenFile1Click(Sender: TObject);
var
  hr: HResult;
  i: Integer;

begin
  if dlgOpen.execute then
    begin
      if dlgOpen.FileName <> '' then
        begin
          hr := g_ThumbnailGen.OpenFile(PWideChar(dlgOpen.FileName));

          if (FAILED(hr)) then
            begin
              Exit;
            end;

          // Clear all the sprites.
          for i := 0 to MAX_SPRITES do
            begin
              g_pSprites[i].Clear();
            end;

          if (g_pRT = Nil) then
            begin
              // Create the Direct2D resources.
              hr := CreateDrawingResources(Form1.Handle);
            end;

          // Generate new sprites.
          if (SUCCEEDED(hr)) then
            begin
              {$IFDEF DEBUG}
              assert(g_pRT <> nil);
              {$ENDIF}
              hr := g_ThumbnailGen.CreateBitmaps(g_pRT,
                                                 MAX_SPRITES,
                                                 g_pSprites);
            end;


         if (SUCCEEDED(hr)) then
           begin
             UnselectAllSprites();
             // Select the first sprite.
             SelectSprite(0);
           end;

         if (SUCCEEDED(hr)) then
           begin
             hr := RenderFrame(Self.WindowHandle);

             if FAILED(hr) then
               begin
                 ShowMessage('Function RenderFrame() in TForm1.OpenFile1Click failed!');
                 Exit;
               end;
           end;


       end;
    end;
end;


//-------------------------------------------------------------------
// Message loop for the application window.
//-------------------------------------------------------------------
function TForm1.MessageLoop(hw: HWND): INT;
var
  _msg: TMsg;
  timerhandle: THandle;

begin


  repeat
      if (PeekMessage(_msg,
                      0,
                      0,
                      0,
                      PM_REMOVE)) then
        begin
          TranslateMessage(_msg);
          DispatchMessage(_msg);
          Continue;
        end;

      timerhandle := g_Timer.Handle;
      // Wait until the timer expires or any message is posted.
      if (WAIT_OBJECT_0 = MsgWaitForMultipleObjects(1,
                                                    timerhandle,
                                                    FALSE,
                                                    INFINITE,
                                                    QS_ALLINPUT)) then
        begin
          RenderFrame(hw);
        end;
  until (_msg.Message = WM_QUIT) or (bTerminate = True);


  DestroyWindow(hw);

  Result := INT(_msg.wParam);
end;




/////

procedure InitializeDPIScale(hw: HWND);
var
  dchdc: HDC;

begin
  //
  //  Create the font to be used in the OSD, making sure that
  //  the font size is scaled system DPI setting.
  //
  dchdc := GetDC(hw);
  g_fDPIScaleX := GetDeviceCaps(dchdc,
                                LOGPIXELSX) / 96.0;
  g_fDPIScaleY := GetDeviceCaps(dchdc,
                                LOGPIXELSY) / 96.0;
  ReleaseDC(hw,
            dchdc);
end;

function DPIScaleX(iValue: Integer): Integer;
begin
  Result := round(iValue / g_fDPIScaleX);
end;

function DPIScaleY(iValue: Integer): Integer;
begin
  Result := round(iValue / g_fDPIScaleY);
end;

//-------------------------------------------------------------------
// UnselectAllSprites: Unselects all sprites.
//-------------------------------------------------------------------
procedure UnselectAllSprites();
var
  i: integer;
begin
  for i := 0 to MAX_SPRITES do
    begin
      g_pSprites[i].AnimateBoundingBox(g_rcSmall[i],
                                       0.0,
                                       0.0);
    end;

  g_Selection := -1;
end;


//-------------------------------------------------------------------
// SelectSprite: Selects a sprite.
//-------------------------------------------------------------------
procedure SelectSprite(iSelection: Integer);
var
  _time: Float;

begin

  if (iSelection > MAX_SPRITES) then
    begin
      assert(FALSE); // Debug
      Exit;
    end;

  _time := (g_Timer.GetFrameNumber()) div 30;

  // Apply animation to the sprite.

  // NOTE: If the sprite is already selected, this call is still used
  // to apply "wobble" to the sprite.

  g_pSprites[iSelection].AnimateBoundingBox(g_rcBig,
                                            _time,
                                            ANIMATION_DURATION);

  // Unselect the current selection.
  if ((g_Selection <> -1) and (g_Selection <> iSelection)) then
    begin
      g_pSprites[g_Selection].AnimateBoundingBox(g_rcSmall[g_Selection],
                                                 _time,
                                                 ANIMATION_DURATION);
    end;

  g_Selection := iSelection;
end;

//-------------------------------------------------------------------
// CreateDrawingResources: Creates Direct2D resources.
//-------------------------------------------------------------------
function CreateDrawingResources(hw: HWND): HRESULT;
var
  hr: HResult;
  rcClient: TRect;
  pFactory: ID2D1Factory;
  pRenderTarget: ID2D1HwndRenderTarget;

begin

  rcClient:= TRect.Empty();

  pFactory := Nil;
  pRenderTarget := Nil;

  if Not GetClientRect(hw,
                       rcClient) then
  begin
    Result := E_FAIL;
    Exit;
  end;

  hr := D2D1CreateFactory(D2D1_FACTORY_TYPE_SINGLE_THREADED,
                          IID_ID2D1Factory,
                          Nil,
                          pFactory);

  if (SUCCEEDED(hr)) then
    begin
      hr := pFactory.CreateHwndRenderTarget(D2D1RenderTargetProperties(),
                                            D2D1HwndRenderTargetProperties(hw,
                                                                           D2D1SizeU(rcClient.right,
                                                                                     rcClient.bottom)),
                                            pRenderTarget);

    end;

  if (SUCCEEDED(hr)) then
    begin
      g_pRT := pRenderTarget;
    end;

  Result:= hr;
end;


//-------------------------------------------------------------------
// RenderFrame: Draw all the sprites.
//-------------------------------------------------------------------
function RenderFrame(hw: HWND): HRESULT;
var
  hr: HResult;
  fTime: Single;
  i: Integer;
  pSprite: TSprite;

begin
  hr := S_OK;

  if Assigned(g_pRT) and (g_pRT.CheckWindowState() = D2D1_WINDOW_STATE_OCCLUDED) then
    begin
      Result := S_OK; // The render target is occluded.
      Exit;
    end;

  if not Assigned(g_pRT) then
    begin
      // Create the Direct2D resources.
      hr := CreateDrawingResources(hw);
    end;
  

  if (SUCCEEDED(hr)) then
    begin

      //fTime := round(g_Timer.GetFrameNumber()) / 30;
      fTime := g_Timer.GetFrameNumber() div 30;

      g_pRT.BeginDraw();

      g_pRT.Clear(BACKGROUND_COLOR);  //D2D1ColorF(0.93, 0.94, 0.96, 1.0)

      // Update and draw each sprite.
      for i := 0 to MAX_SPRITES do
        begin
          //pSprite:= Nil;
          pSprite := g_pSprites[i];
          if (pSprite <> Nil) then
            begin
                pSprite.Update(g_pRT,
                               fTime);
                pSprite.Draw(g_pRT);
            end;
           // Reset the transform to the identiy matrix.
           g_pRT.SetTransform(D2D1_MATRIX_3X2_F.Identity());
        end;
      hr := g_pRT.EndDraw();
    end;

  Result := hr;
end;

// Small & big thunmbrects initialization methods
procedure InitSmallRects();
begin
                            {left  top   right  bottom}
   g_rcSmall[0] := D2D1RectF(0.05, 0.0,  0.2,   0.20);
   g_rcSmall[1] := D2D1RectF(0.05, 0.20, 0.2,   0.40);
   g_rcSmall[2] := D2D1RectF(0.05, 0.40, 0.2,   0.60);
   g_rcSmall[3] := D2D1RectF(0.05, 0.60, 0.2,   0.80);
   g_rcSmall[4] := D2D1RectF(0.05, 0.80, 0.2,   1.00);
   // Hint:
   // You can add as many sprites as you want.
   // Just change the MAX_SPRITES and add sprites here
   // like: g_rcSmall[5] := D2D1RectF(float, float, float, float) etc.
   // or write an algorithm that calculates the interval (elapse time, in timer) and filelength to get a preview
end;

// The big (right) blown'up picture
procedure InitBigRect();
begin
  g_rcBig := D2D1RectF(0.25, 0.05, 0.95, 0.95);
end;


initialization
//


finalization
  MFShutdown();
  CoUninitialize();

end.
