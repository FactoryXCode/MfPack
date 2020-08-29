// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - MediaFoundation
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.MediaFoundationApi.Evr9.pas
// Kind: Pascal / Delphi unit
// Release date: 07-07-2012
// Language: ENU
//
// Revision Version: 3.0.0
// Description: The enhanced video renderer (EVR) is a component that displays video on the user's
//              monitor.
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX), Peter Larson (ozships)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 13/08/2020 All                 Enigma release. New layout and namespaces
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows Vista or later.
//
// Related objects: -
// Related projects: MfPackX300
// Known Issues: -
//
// Compiler version: 23 up to 33
// SDK version: 10.0.19041.0
//
// Todo: -
//
//==============================================================================
// Source: evr9.h
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
//==============================================================================
unit WinApi.MediaFoundationApi.Evr9;

  {$HPPEMIT '#include "evr9.h"'}

interface

uses
  {WinApi}
  WinApi.Windows,
  WinApi.WinApiTypes,
  {System}
  System.Win.ComObj,
  {DirectX}
  {Use WinApi, Clootie Dx or MfPack}
  WinApi.DirectX.DXVa2Api,
  WinApi.DirectX.DxVaHd,
  WinApi.DirectX.D3D9,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MFObjects,
  WinApi.MediaFoundationApi.MFTransform,
  WinApi.MediaFoundationApi.Evr;

  {$WEAKPACKAGEUNIT ON}
  {$MINENUMSIZE 4}

  {$IFDEF WIN32}
    {$ALIGN 1}
  {$ELSE}
    {$ALIGN 8} // Win64
  {$ENDIF}

  {$I 'WinApiTypes.inc'}

// defined in WinApi.WinApiTypes
// FLOAT
// PPTGUID
// LPGUID


type
  PMFVideoAlphaBitmapParams = ^MFVideoAlphaBitmapParams;
  MFVideoAlphaBitmapParams = record
    dwFlags: DWORD;
    clrSrcKey: COLORREF;
    rcSrc: TRect;
    nrcDest: MFVideoNormalizedRect;
    fAlpha: FLOAT;
    dwFilterMode: DWORD;
  end;
  {$EXTERNALSYM MFVideoAlphaBitmapParams}

  PtMFPackSource = ^tMFPackSource;
  tMFPackSource = record
    hdc:                HDC;
    pDDS:               IDirect3DSurface9;
  end;
  {$EXTERNALSYM tMFPackSource}

  PMFVideoAlphaBitmap = ^MFVideoAlphaBitmap;
  MFVideoAlphaBitmap = record
    GetBitmapFromDC: BOOL;
    Source: tMFPackSource;
    bitmap : record     //union part
       case byte of
         0: (hdc: HDC;
             params: MFVideoAlphaBitmapParams);
         1: (pDDS: PIDirect3DSurface9);
       end;
  end;
  {$EXTERNALSYM MFVideoAlphaBitmap}

type
  PMFVideoAlphaBitmapFlags = ^MFVideoAlphaBitmapFlags;
  MFVideoAlphaBitmapFlags = DWord;
  {$EXTERNALSYM MFVideoAlphaBitmapFlags}
const
  MFVideoAlphaBitmap_EntireDDS   = MFVideoAlphaBitmapFlags($1);
  {$EXTERNALSYM MFVideoAlphaBitmap_EntireDDS}
  MFVideoAlphaBitmap_SrcColorKey = MFVideoAlphaBitmapFlags($2);
  {$EXTERNALSYM MFVideoAlphaBitmap_SrcColorKey}
  MFVideoAlphaBitmap_SrcRect     = MFVideoAlphaBitmapFlags($4);
  {$EXTERNALSYM MFVideoAlphaBitmap_SrcRect}
  MFVideoAlphaBitmap_DestRect    = MFVideoAlphaBitmapFlags($8);
  {$EXTERNALSYM MFVideoAlphaBitmap_DestRect}
  MFVideoAlphaBitmap_FilterMode  = MFVideoAlphaBitmapFlags($10);
  {$EXTERNALSYM MFVideoAlphaBitmap_FilterMode}
  MFVideoAlphaBitmap_Alpha       = MFVideoAlphaBitmapFlags($20);
  {$EXTERNALSYM MFVideoAlphaBitmap_Alpha}
  MFVideoAlphaBitmap_BitMask     = MFVideoAlphaBitmapFlags($3F);
  {$EXTERNALSYM MFVideoAlphaBitmap_BitMask}


type

  // INTERFACES ////////////////////////////////////////////////////////////////

  // Interface IEVRVideoStreamControl
  // ================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IEVRVideoStreamControl);'}
  {$EXTERNALSYM IEVRVideoStreamControl}
  IEVRVideoStreamControl = interface(IUnknown)
  ['{d0cfe38b-93e7-4772-8957-0400c49a4485}']

    function SetStreamActiveState(fActive: BOOL): HResult; stdcall;

    function GetStreamActiveState(out lpfActive: BOOL): HResult; stdcall;

  end;
  IID_IEVRVideoStreamControl = IEVRVideoStreamControl;
  {$EXTERNALSYM IID_IEVRVideoStreamControl}


  // Interface IMFVideoProcessor
  // ===========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFVideoProcessor);'}
  {$EXTERNALSYM IMFVideoProcessor}
  IMFVideoProcessor = interface(IUnknown)
  ['{6AB0000C-FECE-4d1f-A2AC-A9573530656E}']

    function GetAvailableVideoProcessorModes(lpdwNumProcessingModes: PUINT;
                                             out ppVideoProcessingModes: PPTGUID): HResult; stdcall;

    function GetVideoProcessorCaps(const lpVideoProcessorMode: LPGUID;
                                   out lpVideoProcessorCaps: DXVA2_VideoProcessorCaps): HResult; stdcall;

    function GetVideoProcessorMode(out lpMode: LPGUID): HResult; stdcall;

    function SetVideoProcessorMode(const lpMode: LPGUID): HResult; stdcall;

    function GetProcAmpRange(dwProperty: DWORD;
                             out pPropRange: DXVA2_ValueRange): HResult; stdcall;

    function GetProcAmpValues(dwFlags: DWORD;
                              out Values: DXVA2_ProcAmpValues): HResult; stdcall;

    function SetProcAmpValues(pValues: DXVA2_ProcAmpValues): HResult; stdcall;

    function GetFilteringRange(dwProperty: DWORD;
                               out pPropRange: DXVA2_ValueRange): HResult; stdcall;

    function GetFilteringValue(dwProperty: DWORD;
                               out pValue: DXVA2_Fixed32): HResult; stdcall;

    function SetFilteringValue(pValue: DXVA2_Fixed32): HResult; stdcall;

    function GetBackgroundColor(out lpClrBkg: COLORREF): HResult; stdcall;

    function SetBackgroundColor(ClrBkg: COLORREF): HResult; stdcall;

  end;
  IID_IMFVideoProcessor = IMFVideoProcessor;
  {$EXTERNALSYM IID_IMFVideoProcessor}


  // Interface IMFVideoMixerBitmap
  // =============================
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFVideoMixerBitmap);'}
  {$EXTERNALSYM IMFVideoMixerBitmap}
  IMFVideoMixerBitmap = interface(IUnknown)
  ['{814C7B20-0FDB-4eec-AF8F-F957C8F69EDC}']

    function SetAlphaBitmap(var pBmpParms: MFVideoAlphaBitmap): HResult; stdcall;

    function ClearAlphaBitmap(): HResult; stdcall;

    function UpdateAlphaBitmapParameters(var pBmpParms: MFVideoAlphaBitmapParams): HResult; stdcall;

    function GetAlphaBitmapParameters(out pBmpParms: MFVideoAlphaBitmapParams): HResult; stdcall;

  end;
  IID_IMFVideoMixerBitmap = IMFVideoMixerBitmap;
  {$EXTERNALSYM IID_IMFVideoMixerBitmap}


  // Additional Prototypes for ALL interfaces

  // End of Additional Prototypes

implementation

  // Implement Additional Prototypes here.

end.
