// FactoryX
//
// Copyright © FactoryX, Netherlands/Australia
//
// Project: Media Foundation - MFPack - Samples
// Project location: http://sourceforge.net/projects/MFPack
// Module: Device.pas
// Kind: Pascal Unit
// Release date: 08-03-2019
// Language: ENU
//
// Version: 3.1.1
//
// Description: Manages the Direct3D device.
//
// Intiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX), Peter Larson (ozships)
//
// Rudy Velthuis 1960 ~ 2019.
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 28/10/2021 All                 Bowie release  SDK 10.0.22000.0 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 or higher.
//
// Related objects: -
// Related projects: MfPackX311
// Known Issues: -
//
// Compiler version: 23 up to 34
// SDK version: 10.0.22000.0
//
// Todo: -
//
// =============================================================================
// Source: MFCaptureD3D Sample
//         device.h : Direct3D device manager class.
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
// Users may distribute this source code provided that this header is included
// in full at the top of the file.
//
//==============================================================================
unit Device;

interface

uses
  {WinApi}
  WinApi.Windows,
  WinApi.WinApiTypes,
  {System}
  System.Types,
  System.SysUtils,
  {DirectX - Clootie, WinApi or MfPack}
  WinApi.DirectX.D3D9,
  WinApi.DirectX.D3D9Types,
  WinApi.DirectX.D3d9Caps,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfUtils,
  WinApi.MediaFoundationApi.MfObjects,
  WinApi.MediaFoundationApi.MfApi,
  WinApi.MediaFoundationApi.Mferror,
  {Project}
  VideoBufferLock;


const
  NUM_BACK_BUFFERS = DWORD(2);



// Conversion methods //////////////////////////////////////////////////////////

// Procedural type (similar to the C language function pointer).
type
   IMAGE_TRANSFORM_FN = procedure(pDest: PByte;
                                  lDestStride: LONG;
                                  pSrc: PByte;
                                  lSrcStride: LONG;
                                  dwWidthInPixels: DWORD;
                                  dwHeightInPixels: DWORD);

  TConversionFunction = record
    subtype: TGuid;
    xform: IMAGE_TRANSFORM_FN;
  end;

  TFormatConversions = array of TConversionFunction;

  // Procedures that match IMAGE_TRANSFORM_FN

  procedure TransformImage_RGB24(pDest: PByte;
                                 lDestStride: LONG;
                                 pSrc: PByte;
                                 lSrcStride: LONG;
                                 dwWidthInPixels: DWORD;
                                 dwHeightInPixels: DWORD);

  procedure TransformImage_RGB32(pDest: PByte;
                                 lDestStride: LONG;
                                 pSrc: PByte;
                                 lSrcStride: LONG;
                                 dwWidthInPixels: DWORD;
                                 dwHeightInPixels: DWORD);

  procedure TransformImage_YUY2(pDest: PByte;
                                lDestStride: LONG;
                                pSrc: PByte;
                                lSrcStride: LONG;
                                dwWidthInPixels: DWORD;
                                dwHeightInPixels: DWORD);

  procedure TransformImage_NV12(pDest: PByte;
                                lDestStride: LONG;
                                pSrc: PByte;
                                lSrcStride: LONG;
                                dwWidthInPixels: DWORD;
                                dwHeightInPixels: DWORD);


var
  g_FormatConversions: TFormatConversions;
  g_cFormats: DWORD; // this will be set in procedure InitConversionProcs()

////////////////////////////////////////////////////////////////////////////////

type

  TDrawDevice = class(TObject)
  private
    m_hwnd: HWND;
    m_d3dpp: D3DPRESENT_PARAMETERS;
    // Format information
    m_Format: D3DFORMAT;
    m_LDefaultStride: LONG;
    m_PixelAR: MFRatio;
    m_Interlace: MFVideoInterlaceMode;
    m_RcDest: TRect;        // Destination rectangle

    m_pD3D:       IDirect3D9;
    m_pDevice:    IDirect3DDevice9;
    m_pSwapChain: IDirect3DSwapChain9;

    // Drawing
    m_ConvertFn: IMAGE_TRANSFORM_FN;     // Function to convert the video to RGB32

    function SetConversionFunction(subtype: REFGUID): HResult;
    function CreateSwapChains(): HResult;

  public
    m_srcWidth: UINT32;
    m_srcHeight: UINT32;
    m_DestWidth: WORD;
    m_destHeight: WORD;

    constructor Create();
    destructor Destroy(); override;  // DestroyDevice

    function CreateDevice(hw: HWND): HResult;
    function ResetDevice(): HResult;
    procedure DestroyDevice();

    procedure UpdateDestinationRect();
    function DrawFrame(pBuffer: IMFMediaBuffer): HResult;
    function SetVideoType(pType: IMFMediaType): HResult;
    // What video formats we accept
    function IsFormatSupported(subtype: REFGUID): BOOL;
    function GetFormat(index: DWORD;
                       out pSubtype: TGUID): HResult;

    function TestCooperativeLevel(): HResult;

  end;


  // Helpers
  function LetterBoxRect(rcSrc: TRect;
                         rcDst: TRect): TRect;

  function CorrectAspectRatio(src: TRect;
                              srcPAR: MFRatio): TRect;

  function GetDefaultStride(pType: IMFMediaType;
                            out plStride: LONG): HResult;

  // Rect helpers
  // function GetTRectWidth(r: TRECT): LONG;
  // function GetTRectHeight(r: TRECT): LONG;

  // initialize the image transform procedures
  procedure InitTransformImages();


implementation


procedure InitTransformImages();
begin
   SetLength(g_FormatConversions, 4);
   g_cFormats := Length(g_FormatConversions);

   g_FormatConversions[0].subtype := MFVideoFormat_RGB32;
   g_FormatConversions[0].xform := @TransformImage_RGB32;
   g_FormatConversions[1].subtype := MFVideoFormat_RGB24;
   g_FormatConversions[1].xform := @TransformImage_RGB24;
   g_FormatConversions[2].subtype := MFVideoFormat_YUY2;
   g_FormatConversions[2].xform := @TransformImage_YUY2;
   g_FormatConversions[3].subtype := MFVideoFormat_NV12;
   g_FormatConversions[3].xform := @TransformImage_NV12;
end;


// TDrawDevice class ///////////////////////////////////////////////////////////

//-------------------------------------------------------------------
// Constructor
//-------------------------------------------------------------------
constructor TDrawDevice.Create();
begin
  inherited Create();

  m_hwnd := 0;
  m_pD3D := Nil;
  m_pDevice := Nil;
  m_pSwapChain := Nil;
  m_format := D3DFMT_UNKNOWN;
  m_srcwidth := 0;
  m_srcheight := 0;
  m_lDefaultStride := 0;
  m_interlace := MFVideoInterlace_Unknown;
  m_convertFn := Nil; //If we had pointer sisters or brothers

  m_PixelAR.Numerator := 1;
  m_PixelAR.Denominator := 1;

  ZeroMemory(@m_d3dpp,
             SizeOf(m_d3dpp));

  // Delphi specific implementation

  InitTransformImages();

end;

//-------------------------------------------------------------------
// DestroyDevice
//
// Release all Direct3D resources.
//-------------------------------------------------------------------
destructor TDrawDevice.Destroy();
begin
  //DestroyDevice();
  inherited Destroy();
end;

////////////////////////////////////////////////////////////////////////////////

//-------------------------------------------------------------------
// CreateDevice
//
// Create the Direct3D device.
//-------------------------------------------------------------------
function TDrawDevice.CreateDevice(hw: HWND): HResult;
var
  hr: HRESULT;
  pp: D3DPRESENT_PARAMETERS;
  mode: D3DDISPLAYMODE;

label
  done;

begin

  // Nothing todo > exit
  if Assigned(m_pDevice) then
    begin
      Result := S_OK;
      Exit;
    end;

  // Create the Direct3D object.
  if (m_pD3D = Nil) then
    begin
      m_pD3D := Direct3DCreate9(D3D_SDK_VERSION);
      //m_pD3D:= IDirect3D9(_Direct3DCreate9(D3D_SDK_VERSION));

      if (m_pD3D = Nil) then
        begin
          Result := E_FAIL;
          Exit;
        end;
    end;

  hr := m_pD3D.GetAdapterDisplayMode(D3DADAPTER_DEFAULT,
                                     mode);

  if FAILED(hr) then
    goto done;

  hr := m_pD3D.CheckDeviceType(D3DADAPTER_DEFAULT,
                               D3DDEVTYPE_HAL,
                               mode.Format,
                               D3DFMT_X8R8G8B8,
                               True);    // windowed
  if FAILED(hr) then
    goto done;

  pp := Default(D3DPRESENT_PARAMETERS);

  pp.BackBufferFormat := D3DFMT_X8R8G8B8;
  pp.SwapEffect := D3DSWAPEFFECT_COPY;
  pp.PresentationInterval := D3DPRESENT_INTERVAL_IMMEDIATE;
  pp.Windowed := TRUE;
  pp.hDeviceWindow := hw;

  hr := m_pD3D.CreateDevice(D3DADAPTER_DEFAULT,
                            D3DDEVTYPE_HAL,
                            hw,
                            D3DCREATE_HARDWARE_VERTEXPROCESSING or D3DCREATE_FPU_PRESERVE,
                            @pp,
                            m_pDevice);

  if FAILED(hr) then
    begin
      if (hr = -2005530516) then  // This is a well known error if D3DPRESENT_PARAMETERS parameters have an incorrect value.
        MessageBox(0,
                   lpwstr('Error: Failed to create D3D device'),
                   lpwstr('DirectX is unable to create a device with current D3DPRESENT_PARAMETERS settings.'),
                   MB_ICONHAND)
      else
        MessageBox(0,
                   lpwstr('Error: Failed to create D3D device'),
                   lpwstr('Unkown error ' + IntToStr(hr)),
                   MB_ICONHAND);
      goto done;
    end;

  m_hwnd := hw;
  m_d3dpp := pp;

done:
  Result := hr;
end;


//-------------------------------------------------------------------
// ResetD3DDevice
//
// Resets the Direct3D device.
//-------------------------------------------------------------------
function TDrawDevice.ResetDevice(): HResult;
var
  hr: HRESULT;
  d3dpp: D3DPRESENT_PARAMETERS;

label
  Done;

begin
  hr := S_OK;

  if (m_pDevice <> Nil) then
    begin
      d3dpp := m_d3dpp;

      hr := m_pDevice.Reset(d3dpp);

      if FAILED(hr) then   // in most cases D3DERR_INVALIDCALL will be returned when failed
        DestroyDevice();
    end;

  if m_pDevice = Nil then
    begin
      hr := CreateDevice(m_hwnd);
      if FAILED(hr) then
        goto Done;
    end;

  if ((m_pSwapChain = Nil) and (m_format <> D3DFMT_UNKNOWN)) then
    begin
      hr := CreateSwapChains();
      if FAILED(hr) then
        goto done;
      UpdateDestinationRect();
    end;

done:
   Result := hr;
end;

//-------------------------------------------------------------------
// DestroyDevice
//
// Release all Direct3D resources.
//-------------------------------------------------------------------
procedure TDrawDevice.DestroyDevice();
begin
  SafeRelease(m_pSwapChain);
  SafeRelease(m_pDevice);
  SafeRelease(m_pD3D);
end;

//-------------------------------------------------------------------
// SetVideoType
//
// Set the video format.
//-------------------------------------------------------------------
function TDrawDevice.SetVideoType(pType: IMFMediaType): HResult;
var
  hr: HRESULT;
  subtype: REFGUID; //TGUID;
  PAR: MFRatio;

label
  done;

begin

  subtype := GUID_NULL;
  PAR := Default(MFRatio);

  // Find the video subtype.
  hr := pType.GetGUID(MF_MT_SUBTYPE,
                     subtype);

  if FAILED(hr) then
    goto done;

  // Choose a conversion function.
  // (This also validates the format type.)

  hr := SetConversionFunction(subtype);

  if FAILED(hr) then
    goto done;

  //
  // Get some video attributes.
  //

  // Get the frame size.
  hr := MFGetAttributeSize(pType,
                           MF_MT_FRAME_SIZE,
                           m_srcWidth,
                           m_srcHeight);

  if FAILED(hr) then
    goto done;

  // Get the interlace mode. Default: assume progressive.

  m_interlace := MFVideoInterlaceMode(MFGetAttributeUINT32(pType,
                                      MF_MT_INTERLACE_MODE,
                                      MFVideoInterlace_Progressive));

  // Get the image stride.
  hr := GetDefaultStride(pType,
                         m_lDefaultStride);
  if FAILED(hr) then
    goto done;

  // Get the pixel aspect ratio. Default: Assume square pixels (1:1)
  hr := MFGetAttributeRatio(pType,
                            MF_MT_PIXEL_ASPECT_RATIO,
                            UINT32(PAR.Numerator),
                            UINT32(PAR.Denominator));

  if SUCCEEDED(hr) then
    m_PixelAR := PAR
  else
    begin
      m_PixelAR.Denominator := 1;
      m_PixelAR.Numerator := m_PixelAR.Denominator;
    end;

  m_format := D3DFORMAT(subtype.D1);

  // Create Direct3D swap chains.

  hr := CreateSwapChains();

  if FAILED(hr) then
    goto done;

  // Update the destination rectangle for the correct
  // aspect ratio.

  UpdateDestinationRect();

done:
  if FAILED(hr) then
    begin
      m_format := D3DFMT_UNKNOWN;
      m_convertFn := Nil;
    end;
  Result := hr;
end;

//-------------------------------------------------------------------
// DrawFrame
//
// Draw the video frame.
//-------------------------------------------------------------------
function TDrawDevice.DrawFrame(pBuffer: IMFMediaBuffer): HResult;
var
  hr: HRESULT;
  pbScanline0: PByte;
  lStride: LONG;
  lr: D3DLOCKED_RECT;
  pSurf: IDirect3DSurface9;
  pBB:   IDirect3DSurface9;
  buffer: TVideoBufferLock;

label
  done;

begin
  pbScanline0 := Nil;
  lStride := 0;

  if Not Assigned(m_convertFn) then
    begin
      Result := MF_E_INVALIDREQUEST;
      Exit;
    end;

  if (m_pDevice = Nil) or (m_pSwapChain = Nil) then
    begin
      Result := S_OK;
      Exit;
    end;

  buffer := TVideoBufferLock.Create(pBuffer); // Helper object to lock the video buffer.

  hr := TestCooperativeLevel();
  if FAILED(hr) then
    goto done;

  // Lock the video buffer. This method returns a pointer to the first scan
  // line in the image, and the stride in bytes.
  hr := buffer.LockBuffer(m_lDefaultStride,
                          m_srcHeight,
                          pbScanline0,
                          lStride);
  if FAILED(hr) then
    goto done;

  // Get the swap-chain surface.
  hr := m_pSwapChain.GetBackBuffer(0,
                                   D3DBACKBUFFER_TYPE_MONO,
                                   pSurf);
  if FAILED(hr) then
    goto done;

  // Lock the swap-chain surface.
  hr := pSurf.LockRect(lr,
                       Nil,
                       D3DLOCK_NOSYSLOCK);
  if FAILED(hr) then
    goto done;

  // Convert the frame. This also copies it to the Direct3D surface.
  m_convertFn(PByte(lr.pBits),
              lr.Pitch,
              pbScanline0,
              lStride,
              m_srcWidth,
              m_srcHeight);

  hr := pSurf.UnlockRect();
  if FAILED(hr) then
    goto done;

  // Color fill the back buffer.
  hr := m_pDevice.GetBackBuffer(0,
                                0,
                                D3DBACKBUFFER_TYPE_MONO,
                                pBB);
  if FAILED(hr) then
    goto done;

  hr := m_pDevice.ColorFill(pBB,
                            Nil,
                            D3DCOLOR_XRGB(0,
                                          0,
                                          $80));   // Blue background
  if FAILED(hr) then
    goto done;

  // Blit the frame.
  hr := m_pDevice.StretchRect(pSurf,
                              Nil,
                              pBB,
                              @m_rcDest,
                              D3DTEXF_LINEAR);
  if FAILED(hr) then
    goto done;

  // Present the frame.
  hr := m_pDevice.Present(Nil,
                          Nil,
                          0,
                          Nil);

done:

  FreeAndNil(buffer); // MUST DO!!! Otherwise the buffer stays in locked state and
                      // OnReadSample will halt after approx 10 samples (because of an exhausted sample pool)

  pbScanline0 := Nil;
  Result := hr;
end;

//-------------------------------------------------------------------
//  IsFormatSupported
//
//  Query if a format is supported.
//-------------------------------------------------------------------

function TDrawDevice.IsFormatSupported(subtype: REFGUID): BOOL;
var
  i: Integer;
begin
  Result:= False;
  for i:= 0 to g_cFormats -1 do
    Result:= IsEqualGuid(subtype, g_FormatConversions[i].subtype);
end;

//-------------------------------------------------------------------
// GetFormat
//
// Get a supported output format by index.
//-------------------------------------------------------------------
function TDrawDevice.GetFormat(index: DWORD;
                               out pSubtype: TGUID): HResult;
begin
 if (index < g_cFormats) then
   begin
     pSubtype:= g_FormatConversions[index].subtype;
     Result:= S_OK;
     Exit;
   end;

  Result:= MF_E_NO_MORE_TYPES;
end;

// private ---------------------------------------------------------------------

//-------------------------------------------------------------------
// TestCooperativeLevel
//
// Test the cooperative-level status of the Direct3D device.
//-------------------------------------------------------------------
function TDrawDevice.TestCooperativeLevel(): HResult;
var
  hr: HRESULT;

begin

  if (m_pDevice = Nil) then
    begin
      Result := E_FAIL;
      Exit;
    end;

  // Check the current status of D3D9 device.
  hr:= m_pDevice.TestCooperativeLevel();

  case hr of

    D3D_OK:                 begin
                              hr := S_OK;
                            end;

    D3DERR_DEVICELOST:      begin
                              hr := S_OK;
                            end;

    D3DERR_DEVICENOTRESET:  begin
                              hr := ResetDevice();
                            end;

    else                    begin
                              // Some other failure.
                            end;
  end;

  Result := hr;
end;

//-------------------------------------------------------------------
// SetConversionFunction
//
// Set the conversion function for the specified video format.
//-------------------------------------------------------------------
function TDrawDevice.SetConversionFunction(subtype: REFGUID): HResult;
var
  i: Integer;

begin
  m_convertFn := Nil;

  for i := 0 to g_cFormats -1 do
    begin
      if IsEqualGuid(g_FormatConversions[i].subtype, subtype) then
        begin
          m_convertFn := @g_FormatConversions[i].xform;
          Result := S_OK;
          Exit;
        end;
    end;
  Result := MF_E_INVALIDMEDIATYPE;
end;


//-------------------------------------------------------------------
// CreateSwapChains
//
// Create Direct3D swap chains.
//-------------------------------------------------------------------
function TDrawDevice.CreateSwapChains(): HResult;
var
  hr: HRESULT;
  pp: D3DPRESENT_PARAMETERS;

begin
  //pp := 0; << Use Default() to accomplish the same
  pp := Default(D3DPRESENT_PARAMETERS);

  SafeRelease(m_pSwapChain);

  pp.BackBufferWidth := m_srcWidth;
  pp.BackBufferHeight := m_srcHeight;
  pp.Windowed := TRUE;
  pp.SwapEffect := D3DSWAPEFFECT_FLIP;
  pp.hDeviceWindow := m_hwnd;
  pp.BackBufferFormat := D3DFMT_X8R8G8B8;
  pp.Flags := D3DPRESENTFLAG_VIDEO or D3DPRESENTFLAG_DEVICECLIP or
              D3DPRESENTFLAG_LOCKABLE_BACKBUFFER;
  pp.PresentationInterval := D3DPRESENT_INTERVAL_IMMEDIATE;
  pp.BackBufferCount := NUM_BACK_BUFFERS;

  hr := m_pDevice.CreateAdditionalSwapChain(pp,
                                            m_pSwapChain);

  Result := hr;
end;

//-------------------------------------------------------------------
//  UpdateDestinationRect
//
//  Update the destination rectangle for the current window size.
//  The destination rectangle is letterboxed to preserve the
//  aspect ratio of the video image.
//-------------------------------------------------------------------
procedure TDrawDevice.UpdateDestinationRect();
var
  rcClient: TRect;
  rcSrc: TRect;

begin
  rcSrc.Top := 0;
  rcSrc.Left := 0;
  rcSrc.Right := m_srcWidth;
  rcSrc.Bottom := m_srcHeight;

  GetClientRect(m_hwnd,
                rcClient);

  rcSrc := CorrectAspectRatio(rcSrc,
                              m_PixelAR);

  m_rcDest := LetterBoxRect(rcSrc,
                            rcClient);
end;


// HELPERS /////////////////////////////////////////////////////////////////////


//-------------------------------------------------------------------
// TransformImage_RGB24
//
// RGB-24 to RGB-32
//-------------------------------------------------------------------
procedure TransformImage_RGB24(pDest: PByte;
                               lDestStride: LONG;
                               pSrc: PByte;
                               lSrcStride: LONG;
                               dwWidthInPixels: DWORD;
                               dwHeightInPixels: DWORD);
var
  x, y: Integer;
  pSrcPel: PRGBTRIPLE;
  pDestPel: PDWORD;

begin

{$POINTERMATH ON}

  for y := 0 to dwHeightInPixels -1 do
    begin

      pSrcPel := PRGBTRIPLE(pSrc);
      pDestPel := PDWORD(pDest);

      for x := 0 to dwWidthInPixels -1 do
        begin
          pDestPel[x] := D3DCOLOR_XRGB(pSrcPel[x].rgbtRed,
                                       pSrcPel[x].rgbtGreen,
                                       pSrcPel[x].rgbtBlue
                                       );

          // or like this:
          //
          //pDestPel[x] := DWord((psrcPel[x].rgbtRed shl 16) or
          //                     (psrcPel[x].rgbtGreen shl 8) or
          //                     (psrcPel[x].rgbtBlue shl 0) or
          //                     ($00000000 shl 24)); // padding byte, must be 0

          // or like this:
          //
          // CopyRgbTripleToClrRef(pSrcPel[x], pDestPel[x]);

        end;

      inc(pSrc, lSrcStride);   // or if you like, pSrc:= pSrc + lSrcStride;
      inc(pDest, lDestStride); // or if you like, pDest:= pDest + lDestStride;

   end;

{$POINTERMATH OFF}

end;

//-------------------------------------------------------------------
// TransformImage_RGB32
//
// RGB-32 to RGB-32
//
// Note: This function is needed to copy the image from system
// memory to the Direct3D surface.
//-------------------------------------------------------------------
procedure TransformImage_RGB32(pDest: PByte;
                               lDestStride: LONG;
                               pSrc: PByte;
                               lSrcStride: LONG;
                               dwWidthInPixels: DWORD;
                               dwHeightInPixels: DWORD);
begin

  MFCopyImage(pDest,
              lDestStride,
              pSrc,
              lSrcStride,
              (dwWidthInPixels * 4),
              dwHeightInPixels);
end;




//-------------------------------------------------------------------
// TransformImage_YUY2
//
// YUY2 to RGB-32
//-------------------------------------------------------------------

procedure TransformImage_YUY2(pDest: PByte;
                              lDestStride: LONG;
                              pSrc: PByte;
                              lSrcStride: LONG;
                              dwWidthInPixels: DWORD;
                              dwHeightInPixels: DWORD);
var
  y: DWord;
  x: DWord;
  pDestPel: PRGBQUAD;
  pSrcPel: PWORD;
  y0: Integer;
  u0: Integer;
  y1: Integer;
  v0: Integer;

begin

{$POINTERMATH ON}

  for y := 0 to dwHeightInPixels -1 do
    begin
      pDestPel := PRGBQUAD(pDest);
      pSrcPel := PWORD(pSrc);
      x := 0;
      while (x < dwWidthInPixels) do
        begin
          // Byte order is U0 Y0 V0 Y1
          // Each WORD is a byte pair (Y, U/V)
          // Windows is little-endian so the order appears reversed.
          y0 := int(LOBYTE(pSrcPel[x]));
          u0 := int(HIBYTE(pSrcPel[x]));
          y1 := int(LOBYTE(pSrcPel[x + 1]));
          v0 := int(HIBYTE(pSrcPel[x + 1]));

          pDestPel[x] := ConvertYCrCbToRGB(y0, v0, u0);
          pDestPel[x + 1] := ConvertYCrCbToRGB(y1, v0, u0);

          Inc(x, 2);
        end;

      Inc(pSrc, lSrcStride);
      Inc(pDest, lDestStride);
  end;

{$POINTERMATH OFF}

end;


//-------------------------------------------------------------------
// TransformImage_NV12
//
// NV12 to RGB-32
//-------------------------------------------------------------------
procedure TransformImage_NV12(pDest: PByte;
                              lDestStride: LONG;
                              pSrc: PByte;
                              lSrcStride: LONG;
                              dwWidthInPixels: DWORD;
                              dwHeightInPixels: DWORD);
var
  lpBitsY, lpBitsCb, lpBitsCr: PByte;
  Y, X: DWORD;
  lpLineY1, lpLineY2, lpLineCr, lpLineCb: PByte;
  lpDibLine1, lpDibLine2: LPByte;
  y0, y1, y2, y3, cb, cr: Int;
  r: RGBQUAD;

begin

{$POINTERMATH ON}

  lpBitsY := pSrc;
  lpBitsCb := lpBitsY + (dwHeightInPixels * Dword(lsrcStride));
  lpBitsCr := lpBitsCb + 1;
  Y := 0;

  repeat
  //for (UINT y = 0; y < dwHeightInPixels; y += 2)

    lpLineY1 := lpBitsY;
    lpLineY2 := lpBitsY + lsrcStride;
    lpLineCr := lpBitsCr;
    lpLineCb := lpBitsCb;

    lpDibLine1 := pDest;
    lpDibLine2 := pDest + ldestStride;

    // for (UINT x = 0; x < dwWidthInPixels; x += 2)
    X := 0;
    repeat

      y0 := Int(lpLineY1[0]);
      y1 := Int(lpLineY1[1]);
      y2 := Int(lpLineY2[0]);
      y3 := Int(lpLineY2[1]);
      cb := Int(lpLineCb[0]);
      cr := Int(lpLineCr[0]);

      r := ConvertYCrCbToRGB(y0, cr, cb);
      lpDibLine1[0] := r.rgbBlue;
      lpDibLine1[1] := r.rgbGreen;
      lpDibLine1[2] := r.rgbRed;
      lpDibLine1[3] := 0; // Alpha

      r := ConvertYCrCbToRGB(y1, cr, cb);
      lpDibLine1[4] := r.rgbBlue;
      lpDibLine1[5] := r.rgbGreen;
      lpDibLine1[6] := r.rgbRed;
      lpDibLine1[7] := 0; // Alpha

      r := ConvertYCrCbToRGB(y2, cr, cb);
      lpDibLine2[0] := r.rgbBlue;
      lpDibLine2[1] := r.rgbGreen;
      lpDibLine2[2] := r.rgbRed;
      lpDibLine2[3] := 0; // Alpha

      r := ConvertYCrCbToRGB(y3, cr, cb);
      lpDibLine2[4] := r.rgbBlue;
      lpDibLine2[5] := r.rgbGreen;
      lpDibLine2[6] := r.rgbRed;
      lpDibLine2[7] := 0; // Alpha

      inc(lpLineY1, 2);
      inc(lpLineY2, 2);
      inc(lpLineCr, 2);
      inc(lpLineCb, 2);

      inc(lpDibLine1, 8);
      inc(lpDibLine2, 8);

      inc(x, 2);

    until (X = dwWidthInPixels);

    inc(pDest, (2 * ldestStride));
    inc(lpBitsY, (2 * lsrcStride));
    inc(lpBitsCr, lsrcStride);
    inc(lpBitsCb, lsrcStride);
    inc(Y, 2);
  until (Y = dwHeightInPixels);
{$POINTERMATH OFF}
end;


//-------------------------------------------------------------------
// LetterBoxDstRect
//
// Takes a src rectangle and constructs the largest possible
// destination rectangle within the specifed destination rectangle
// such thatthe video maintains its current shape.
//
// This function assumes that pels are the same shape within both the
// source and destination rectangles.
//
//-------------------------------------------------------------------
function LetterBoxRect(rcSrc: TRect;
                       rcDst: TRect): TRect;
var
  iSrcWidth: Integer;
  iSrcHeight: Integer;
  iDstWidth: Integer;
  iDstHeight: Integer;
  iDstLBWidth: Integer;
  iDstLBHeight: Integer;
  rc: TRect;
  lleft: LONG;
  ltop: LONG;

begin

  // figure out src/dest scale ratios
  iSrcWidth := rcSrc.Width;
  iSrcHeight := rcSrc.Height;

  iDstWidth := rcDst.Width;
  iDstHeight := rcDst.Height;


  if (MulDiv(iSrcWidth,
             iDstHeight,
             iSrcHeight) <= iDstWidth) then
    begin
      // Column letter boxing ("pillar box")
      iDstLBWidth := MulDiv(iDstHeight, iSrcWidth, iSrcHeight);
      iDstLBHeight := iDstHeight;
    end
  else
    begin
      // Row letter boxing.
      iDstLBWidth := iDstWidth;
      iDstLBHeight := MulDiv(iDstWidth, iSrcHeight, iSrcWidth);
    end;

  // Create a centered rectangle within the current destination rect
  lleft := rcDst.left + ((iDstWidth - iDstLBWidth) div 2);
  ltop := rcDst.top + ((iDstHeight - iDstLBHeight) div 2);

  SetRect(rc,
          lleft,
          ltop,
          lleft + iDstLBWidth,
          ltop + iDstLBHeight);

  CopyTRectToTRect(rc, Result);
end;


//-----------------------------------------------------------------------------
// CorrectAspectRatio
//
// Converts a rectangle from the source's pixel aspect ratio (PAR) to 1:1 PAR.
// Returns the corrected rectangle.
//
// For example, a 720 x 486 rect with a PAR of 9:10, when converted to 1x1 PAR,
// is stretched to 720 x 540.
//-----------------------------------------------------------------------------
function CorrectAspectRatio(src: TRect;
                            srcPAR: MFRatio): TRect;
var
  rc: TRect;

begin
  // Start with a rectangle the same size as src, but offset to the origin (0,0).
  rc.Left := 0;
  rc.Top := 0;
  rc.Right := src.Width;
  rc.Bottom := src.Height;

  if (srcPAR.Numerator <> 1) or (srcPAR.Denominator <> 1) then
    begin
      // Correct for the source's PAR.

      if (srcPAR.Numerator > srcPAR.Denominator) then
        begin
          // The source has "wide" pixels, so stretch the width.
          rc.right := MulDiv(rc.right,
                             srcPAR.Numerator,
                             srcPAR.Denominator);
        end
      else if (srcPAR.Numerator < srcPAR.Denominator) then
        begin
          // The source has "tall" pixels, so stretch the height.
          rc.bottom := MulDiv(rc.bottom,
                              srcPAR.Denominator,
                              srcPAR.Numerator);
        end;
    end;
  // else: PAR is 1:1, which is a no-op.
  Result:= rc;
end;


//-----------------------------------------------------------------------------
// GetDefaultStride
//
// Gets the default stride for a video frame, assuming no extra padding bytes.
//
//-----------------------------------------------------------------------------

function GetDefaultStride(pType: IMFMediaType;
                          out plStride: LONG): HResult;
var
  hr: HRESULT;
  lStride: LONG;
  subtype: TGUID;
  uiwidth: UINT32;
  uiheight: UINT32;

begin
  lStride:= 0;

  // Try to get the default stride from the media type.
  hr:= pType.GetUINT32(MF_MT_DEFAULT_STRIDE,
                       UINT32(lStride));
  if FAILED(hr) then
    begin
      // Attribute not set. Try to calculate the default stride.
      subtype := GUID_NULL;

      uiwidth := 0;
      uiheight := 0;

      // Get the subtype and the image size.
      hr := pType.GetGUID(MF_MT_SUBTYPE,
                          subtype);
      if SUCCEEDED(hr) then
        hr := MFGetAttributeSize(pType,
                                 MF_MT_FRAME_SIZE,
                                 uiwidth,
                                 uiheight);

      if SUCCEEDED(hr) then
        hr := MFGetStrideForBitmapInfoHeader(subtype.D1,
                                             uiwidth,
                                             lStride);

      // Set the attribute for later reference.
      if SUCCEEDED(hr) then
        hr := pType.SetUINT32(MF_MT_DEFAULT_STRIDE,
                              UINT32(lStride));

    end;

  if SUCCEEDED(hr) then
    plStride := lStride;

  Result := hr;
end;


end.
