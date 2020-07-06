// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MFPack - DirectComposition
// Project location: https://sourceforge.net/projects/MFPack
// Module: MfPack.DDrawGdi.pas
// Kind: Pascal / Delphi unit
// Release date: 30-04-2019
// Language: ENU
//
// Revision Version: 2.6.4
//
// Description: Structures and defines for the private entry points in GDI to support
//              DirectDraw.
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX), Peter Larson (ozships)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 28/05/2020                     Kraftwerk release. (WIN10 May 2020 update, version 20H1)
//------------------------------------------------------------------------------
//
// Remarks: -
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
//==============================================================================
// Source: ddrawgdi.h
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
unit MfPack.DDrawGdi;

interface

uses
  WinApi.Windows;




const

// We rename the actual entry points for added protection against anyone
// trying to call our private entry points directly:

  GdiEntry1: function(pDirectDrawGlobal: LPDDRAWI_DIRECTDRAW_GBL;
                      _hdc: HDC): BOOL = DdCreateDirectDrawObject;

  GdiEntry2: function(): BOOL = DdQueryDirectDrawObject;

  GdiEntry3: function(): BOOL = DdDeleteDirectDrawObject;

  GdiEntry4: function(): BOOL = DdCreateSurfaceObject;

  GdiEntry5: function(): BOOL = DdDeleteSurfaceObject;

  GdiEntry6: function(): BOOL = DdResetVisrgn;

  GdiEntry7: function(): BOOL = DdGetDC;

  GdiEntry8: function(): BOOL = DdReleaseDC;

  GdiEntry9: function(): BOOL = DdCreateDIBSection;

  GdiEntry10: function(): BOOL = DdReenableDirectDrawObject;

  GdiEntry11: function(): BOOL = DdAttachSurface;

  GdiEntry12: function(): BOOL = DdUnattachSurface;

  GdiEntry13: function(): BOOL = DdQueryDisplaySettingsUniqueness;

  GdiEntry14: function(): BOOL = DdGetDxHandle;

  GdiEntry15: function(): BOOL = DdSetGammaRamp;

  GdiEntry16: function(): BOOL = DdSwapTextureHandles;

//#ifdef DX_LONGHORN_PRESERVEDC
  GdiEntry17: function(): BOOL = DdChangeSurfacePointer;
//#endif // DX_LONGHORN_PRESERVEDC




function DdCreateDirectDrawObject(pDirectDrawGlobal: LPDDRAWI_DIRECTDRAW_GBL;
                                  hdc: HDC): BOOL; stdcall;

  {$EXTERNALSYM LPD3DHAL_CALLBACKS}
  LPD3DHAL_CALLBACKS = ^_D3DHAL_CALLBACKS;
  {$EXTERNALSYM LPD3DHAL_GLOBALDRIVERDATA}
  LPD3DHAL_GLOBALDRIVERDATA = ^_D3DHAL_GLOBALDRIVERDATA;


function DdQueryDirectDrawObject(pDirectDrawGlobal: LPDDRAWI_DIRECTDRAW_GBL;
                                 pHalInfo: LPDDHALINFO;
                                 pDDCallbacks: LPDDHAL_DDCALLBACKS;
                                 pDDSurfaceCallbacks: LPDDHAL_DDSURFACECALLBACKS;
                                 pDDPaletteCallbacks: LPDDHAL_DDPALETTECALLBACKS;
                                 pD3dCallbacks: LPD3DHAL_CALLBACKS;
                                 pD3dDriverData: LPD3DHAL_GLOBALDRIVERDATA;
                                 pD3dBufferCallbacks: LPDDHAL_DDEXEBUFCALLBACKS;
                                 pD3dTextureFormats: LPDDSURFACEDESC;
                                 pdwFourCC: PDWORD;          // Can be NULL
                                 pvmList: LPVIDMEM): BOOL; stdcall;

function DdDeleteDirectDrawObject(pDirectDrawGlobal: LPDDRAWI_DIRECTDRAW_GBL): BOOL; stdcall;

function DdCreateSurfaceObject(pSurfaceLocal: LPDDRAWI_DDRAWSURFACE_LCL;
                               bPrimarySurface: BOOL): BOOL; stdcall;

function DdDeleteSurfaceObject(pSurfaceLocal: LPDDRAWI_DDRAWSURFACE_LCL): BOOL; stdcall;

function DdResetVisrgn(pSurfaceLocal: LPDDRAWI_DDRAWSURFACE_LCL;
                       hWnd: HWND): BOOL; stdcall;

function DdGetDC(pSurfaceLocal: LPDDRAWI_DDRAWSURFACE_LCL;
                 pColorTable: LPPALETTEENTRY): HDC; stdcall;

function DdReleaseDC(pSurfaceLocal: LPDDRAWI_DDRAWSURFACE_LCL): BOOL; stdcall;

function DdCreateDIBSection(hdc: HDC;
                            const pbmi: PBITMAPINFO;
                            iUsage: UINT;
                            ppvBits: Pointer;
                            hSectionApp: THandle;
                            dwOffset: DWORD): HBITMAP; stdcall;

function DdReenableDirectDrawObject(pDirectDrawGlobal: LPDDRAWI_DIRECTDRAW_GBL;
                                    pbNewMode: BOOL): BOOL; stdcall;

function DdAttachSurface(pSurfaceFrom: LPDDRAWI_DDRAWSURFACE_LCL;
                         pSurfaceTo: LPDDRAWI_DDRAWSURFACE_LCL): BOOL; stdcall;

procedure DdUnattachSurface(pSurface: LPDDRAWI_DDRAWSURFACE_LCL;
                            pSurfaceAttached: LPDDRAWI_DDRAWSURFACE_LCL); stdcall;

function DdQueryDisplaySettingsUniqueness(): ULONG; stdcall;

function DdGetDxHandle(pDDraw: LPDDRAWI_DIRECTDRAW_LCL;
                       pSurface: LPDDRAWI_DDRAWSURFACE_LCL;
                       bRelease: BOOL): HANDLE; stdcall;

function DdSetGammaRamp(pDDraw: LPDDRAWI_DIRECTDRAW_LCL;
                        hdc: HDC;
                        lpGammaRamp: Pointer): BOOL; stdcall;

function DdSwapTextureHandles(pDDraw: LPDDRAWI_DIRECTDRAW_LCL;
                              pDDSLcl1: LPDDRAWI_DDRAWSURFACE_LCL;
                              pDDSLcl2: LPDDRAWI_DDRAWSURFACE_LCL): DWORD; stdcall;

//#ifdef DX_LONGHORN_PRESERVEDC

function DdChangeSurfacePointer(pSurfaceLocal: LPDDRAWI_DDRAWSURFACE_LCL;
                                pDirectDrawGlobal: LPDDRAWI_DIRECTDRAW_GBL;
                                pSurfacePointer: Pointer): DWORD; stdcall;

//#endif // DX_LONGHORN_PRESERVEDC



implementation



end.
