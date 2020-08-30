// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: WinApi.DirectX - DirectX
// Project location: https://sourceforge.net/projects/MfPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.DirectX.DXVA2SWDev.pas
// Kind: Pascal / Delphi unit
// Release date: 02-10-2015
// Language: ENU
//
// Revision Version: 3.0.0
// Description: DirectX Video Acceleration 2 header file for software video
//              processing devices.
//              https://docs.microsoft.com/en-us/windows/win32/medfound/directx-video-acceleration-2-0
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
// Source: dxva2SWDev.h
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

unit WinApi.DirectX.DXVA2SWDev;

interface

uses
  {WinApi}
  WinApi.Windows,
  {DirectX}
  WinApi.DirectX.DXVA9Typ,
  WinApi.DirectX.DXVA2Api,
  WinApi.DirectX.D3D9;

  {$WEAKPACKAGEUNIT ON}
  {$MINENUMSIZE 4}

  {$IFDEF WIN32}
    {$ALIGN 1}
  {$ELSE}
    {$ALIGN 8} // Win64
  {$ENDIF}


type
  PDXVA2_SampleFlags = ^DXVA2_SampleFlags;
  _DXVA2_SampleFlags = DWord;
  {$EXTERNALSYM _DXVA2_SampleFlags}
  DXVA2_SampleFlags = _DXVA2_SampleFlags;
  {$EXTERNALSYM DXVA2_SampleFlags}
const
  DXVA2_SampleFlag_Palette_Changed     = DXVA2_SampleFlags($00000001);
  DXVA2_SampleFlag_SrcRect_Changed     = DXVA2_SampleFlags($00000002);
  DXVA2_SampleFlag_DstRect_Changed     = DXVA2_SampleFlags($00000004);
  DXVA2_SampleFlag_ColorData_Changed   = DXVA2_SampleFlags($00000008);
  DXVA2_SampleFlag_PlanarAlpha_Changed = DXVA2_SampleFlags($00000010);
  DXVA2_SampleFlag_RFF                 = DXVA2_SampleFlags($00010000);
  DXVA2_SampleFlag_TFF                 = DXVA2_SampleFlags($00020000);
  DXVA2_SampleFlag_RFF_TFF_Present     = DXVA2_SampleFlags($00040000);
  DXVA2_SampleFlagsMask                = DXVA2_SampleFlags($FFFF001F);


type
  PDXVA2_DestinationFlags = ^DXVA2_DestinationFlags;
  _DXVA2_DestinationFlags = DWord;
  {$EXTERNALSYM _DXVA2_DestinationFlags}
  DXVA2_DestinationFlags = _DXVA2_DestinationFlags;
  {$EXTERNALSYM DXVA2_DestinationFlags}
const
  DXVA2_DestinationFlag_Background_Changed = DXVA2_DestinationFlags($00000001);
  DXVA2_DestinationFlag_TargetRect_Changed = DXVA2_DestinationFlags($00000002);
  DXVA2_DestinationFlag_ColorData_Changed  = DXVA2_DestinationFlags($00000004);
  DXVA2_DestinationFlag_Alpha_Changed      = DXVA2_DestinationFlags($00000008);
  DXVA2_DestinationFlag_RFF                = DXVA2_DestinationFlags($00010000);
  DXVA2_DestinationFlag_TFF                = DXVA2_DestinationFlags($00020000);
  DXVA2_DestinationFlag_RFF_TFF_Present    = DXVA2_DestinationFlags($00040000);
  DXVA2_DestinationFlagMask                = DXVA2_DestinationFlags($FFFF000F);

type

  PDXVA2_VIDEOSAMPLE = ^DXVA2_VIDEOSAMPLE;
  _DXVA2_VIDEOSAMPLE = record
    Start: REFERENCE_TIME;
    _End: REFERENCE_TIME;
    SampleFormat: DXVA2_ExtendedFormat;
    SampleFlags: UINT;
    SrcResource: Pointer;
    SrcRect: TRect;
    DstRect: TRect;
    Pal: array[0..15] of DXVA2_AYUVSample8;
    PlanarAlpha: DXVA2_Fixed32;
  end;
  {$EXTERNALSYM _DXVA2_VIDEOSAMPLE}
  DXVA2_VIDEOSAMPLE = _DXVA2_VIDEOSAMPLE;
  {$EXTERNALSYM DXVA2_VIDEOSAMPLE}


  PDXVA2_VIDEOPROCESSBLT = ^DXVA2_VIDEOPROCESSBLT;
  _DXVA2_VIDEOPROCESSBLT = record
    TargetFrame: REFERENCE_TIME;
    TargetRect: TRect;
    ConstrictionSize: SIZE;
    StreamingFlags: UINT;
    BackgroundColor: DXVA2_AYUVSample16;
    DestFormat: DXVA2_ExtendedFormat;
    DestFlags: UINT;
    ProcAmpValues: DXVA2_ProcAmpValues;
    Alpha: DXVA2_Fixed32;
    NoiseFilterLuma: DXVA2_FilterValues;
    NoiseFilterChroma: DXVA2_FilterValues;
    DetailFilterLuma: DXVA2_FilterValues;
    DetailFilterChroma: DXVA2_FilterValues;
    pSrcSurfaces: PDXVA2_VIDEOSAMPLE;
    NumSrcSurfaces: UINT;
  end;
  {$EXTERNALSYM _DXVA2_VIDEOPROCESSBLT}
  DXVA2_VIDEOPROCESSBLT = _DXVA2_VIDEOPROCESSBLT;
  {$EXTERNALSYM DXVA2_VIDEOPROCESSBLT}

// #if defined(_D3D9_H_) || defined(_d3d9P_H_)

  PDXVA2SW_GETVIDEOPROCESSORRENDERTARGETCOUNT = function(pVideoDesc: DXVA2_VideoDesc;
                                                         var pCount: UINT): HResult; stdcall;

  PDXVA2SW_GETVIDEOPROCESSORRENDERTARGETS = function(pVideoDesc: DXVA2_VideoDesc;
                                                     Count: UINT;
                                                     var pFormats: PD3DFORMAT): HResult; stdcall;

  PDXVA2SW_GETVIDEOPROCESSORCAPS = function(pVideoDesc: DXVA2_VideoDesc;
                                            RenderTargetFormat: D3DFORMAT;
                                            var pCaps: DXVA2_VideoProcessorCaps): HResult; stdcall;

  PDXVA2SW_GETVIDEOPROCESSORSUBSTREAMFORMATCOUNT = function(pVideoDesc: DXVA2_VideoDesc;
                                                            RenderTargetFormat: D3DFORMAT;
                                                            var pCount: UINT): HResult; stdcall;

  PDXVA2SW_GETVIDEOPROCESSORSUBSTREAMFORMATS = function(pVideoDesc: DXVA2_VideoDesc;
                                                        RenderTargetFormat: D3DFORMAT;
                                                        Count: UINT;
                                                        var pFormats: PD3DFORMAT): HResult; stdcall;

  PDXVA2SW_GETPROCAMPRANGE = function(pVideoDesc: DXVA2_VideoDesc;
                                      RenderTargetFormat: D3DFORMAT;
                                      ProcAmpCap: UINT;
                                      var pRange: DXVA2_ValueRange): HResult; stdcall;

  PDXVA2SW_GETFILTERPROPERTYRANGE = function(pVideoDesc: DXVA2_VideoDesc;
                                             RenderTargetFormat: D3DFORMAT;
                                             FilterSetting: UINT;
                                             var pRange: DXVA2_ValueRange): HResult; stdcall;

  PDXVA2SW_CREATEVIDEOPROCESSDEVICE = function(pD3DD9: IDirect3DDevice9;
                                               pVideoDesc: DXVA2_VideoDesc;
                                               RenderTargetFormat: D3DFORMAT;
                                               MaxSubStreams: UINT;
                                               var phDevice: THandle): HResult; stdcall;

  PDXVA2SW_DESTROYVIDEOPROCESSDEVICE = function(hDevice: THandle): HResult; stdcall;


  PDXVA2SW_VIDEOPROCESSBEGINFRAME = function(hDevice: THANDLE): HResult; stdcall;

  PDXVA2SW_VIDEOPROCESSENDFRAME = function(hDevice: THANDLE;
                                           var pHandleComplete: THandle): HResult; stdcall;

  PDXVA2SW_VIDEOPROCESSSETRENDERTARGET = function(hDevice: THandle;
                                                  pRenderTarget: IDirect3DSurface9): HResult; stdcall;

  PDXVA2SW_VIDEOPROCESSBLT = function(hDevice: THandle;
                                      pBlt: DXVA2_VIDEOPROCESSBLT): HResult; stdcall;


  PDXVA2SW_CALLBACKS = ^DXVA2SW_CALLBACKS;
  {$EXTERNALSYM PDXVA2SW_CALLBACKS}
  _DXVA2SW_CALLBACKS = record
    Size: UINT;
    GetVideoProcessorRenderTargetCount:    PDXVA2SW_GETVIDEOPROCESSORRENDERTARGETCOUNT;
    GetVideoProcessorRenderTargets:        PDXVA2SW_GETVIDEOPROCESSORRENDERTARGETS;
    GetVideoProcessorCaps:                 PDXVA2SW_GETVIDEOPROCESSORCAPS;
    GetVideoProcessorSubStreamFormatCount: PDXVA2SW_GETVIDEOPROCESSORSUBSTREAMFORMATCOUNT;
    GetVideoProcessorSubStreamFormats:     PDXVA2SW_GETVIDEOPROCESSORSUBSTREAMFORMATS;
    GetProcAmpRange:                       PDXVA2SW_GETPROCAMPRANGE;
    GetFilterPropertyRange:                PDXVA2SW_GETFILTERPROPERTYRANGE;
    CreateVideoProcessDevice:              PDXVA2SW_CREATEVIDEOPROCESSDEVICE;
    DestroyVideoProcessDevice:             PDXVA2SW_DESTROYVIDEOPROCESSDEVICE;
    VideoProcessBeginFrame:                PDXVA2SW_VIDEOPROCESSBEGINFRAME;
    VideoProcessEndFrame:                  PDXVA2SW_VIDEOPROCESSENDFRAME;
    VideoProcessSetRenderTarget:           PDXVA2SW_VIDEOPROCESSSETRENDERTARGET;
    VideoProcessBlt:                       PDXVA2SW_VIDEOPROCESSBLT;
  end;
  {$EXTERNALSYM _DXVA2SW_CALLBACKS}
  DXVA2SW_CALLBACKS = _DXVA2SW_CALLBACKS;
  {$EXTERNALSYM DXVA2SW_CALLBACKS}


  // Additional Prototypes for ALL interfaces

  // End of Additional Prototypes

implementation

  // Implement Additional functions here.

end.
