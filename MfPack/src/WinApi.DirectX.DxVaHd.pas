// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: WinApi.DirectX - DirectX
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.DirectX.DXVAHd.pas
// Kind: Pascal / Delphi unit
// Release date: 23-07-2012
// Language: ENU
//
// Revision Version: 3.0.0
// Description: Contains interface definitions for DirectX Video Acceleration for HD API.
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
// Source: dxvahd.h
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
unit WinApi.DirectX.DXVAHd;

  {$HPPEMIT '#include "dxvahd.h"'}

interface

uses

  {WinApi}
  WinApi.Windows,
  {DirectX}
  {Use WinApi, Clootie Dx or MfPack}
  WinApi.DirectX.D3D9;

  {$WEAKPACKAGEUNIT ON}
  {$MINENUMSIZE 4}

  {$IFDEF WIN32}
    {$ALIGN 1}
  {$ELSE}
    {$ALIGN 8} // Win64
  {$ENDIF}

  {$WARN SYMBOL_PLATFORM OFF}

// ver WIN32

const
  DxVaHdLib = 'Dxva2.dll';
  {$EXTERNALSYM DxVaHdLib}

  //
  DXVAHD_STREAM_STATE_PRIVATE_IVTC       : TGUID = '{9c601e3c-0f33-414c-a739-99540ee42da5}';
  {$EXTERNALSYM DXVAHD_STREAM_STATE_PRIVATE_IVTC}

  //
  DXVAHDControlGuid                      : TGUID = '{a0386e75-f70c-464c-a9ce-33c44e091623}';
  {$EXTERNALSYM DXVAHDControlGuid}
  DXVAHDETWGUID_CREATEVIDEOPROCESSOR     : TGUID = '{681e3d1e-5674-4fb3-a503-2f2055e91f60}';
  {$EXTERNALSYM DXVAHDETWGUID_CREATEVIDEOPROCESSOR}
  DXVAHDETWGUID_VIDEOPROCESSBLTSTATE     : TGUID = '{76c94b5a-193f-4692-9484-a4d999da81a8}';
  {$EXTERNALSYM DXVAHDETWGUID_VIDEOPROCESSBLTSTATE}
  DXVAHDETWGUID_VIDEOPROCESSSTREAMSTATE  : TGUID = '{262c0b02-209d-47ed-94d8-82ae02b84aa7}';
  {$EXTERNALSYM DXVAHDETWGUID_VIDEOPROCESSSTREAMSTATE}
  DXVAHDETWGUID_VIDEOPROCESSBLTHD        : TGUID = '{bef3d435-78c7-4de3-9707-cd1b083b160a}';
  {$EXTERNALSYM DXVAHDETWGUID_VIDEOPROCESSBLTHD}
  DXVAHDETWGUID_VIDEOPROCESSBLTHD_STREAM : TGUID = '{27ae473e-a5fc-4be5-b4e3-f24994d3c495}';
  {$EXTERNALSYM DXVAHDETWGUID_VIDEOPROCESSBLTHD_STREAM}
  DXVAHDETWGUID_DESTROYVIDEOPROCESSOR    : TGUID = '{f943f0a0-3f16-43e0-8093-105a986aa5f1}';
  {$EXTERNALSYM DXVAHDETWGUID_DESTROYVIDEOPROCESSOR}

type

  DXVAHDSW_PluginCallbacks = Pointer;
  {$EXTERNALSYM DXVAHDSW_PluginCallbacks}

  LPDIRECT3DDEVICE9EX = ^IDirect3DDevice9Ex;
  {$EXTERNALSYM LPDIRECT3DDEVICE9EX}
  PDIRECT3DDEVICE9EX = ^IDirect3DDevice9Ex;
  PIDirect3DDevice9Ex = ^IDirect3DDevice9Ex;

  LPDIRECT3DSURFACE9 = ^IDirect3DSurface9;
  {$EXTERNALSYM LPDIRECT3DSURFACE9}
  PDIRECT3DSURFACE9 = ^IDirect3DSurface9;
  PIDirect3DSurface9 = ^IDirect3DSurface9;

  PD3DCOLOR = ^D3DCOLOR;
  D3DCOLOR = DWORD;
  {$EXTERNALSYM D3DCOLOR}
  PD3DFORMAT = ^D3DFORMAT;
  D3DFORMAT = DWORD;
  {$EXTERNALSYM D3DFORMAT}
  PD3DPOOL = ^D3DPOOL;
  {$EXTERNALSYM D3DFORMAT}
  D3DPOOL = DWORD;
  {$EXTERNALSYM D3DPOOL}


  PDXVAHD_FRAME_FORMAT = ^_DXVAHD_FRAME_FORMAT;
  _DXVAHD_FRAME_FORMAT                                = (
    DXVAHD_FRAME_FORMAT_PROGRESSIVE                   = 0,
    DXVAHD_FRAME_FORMAT_INTERLACED_TOP_FIELD_FIRST    = 1,
    DXVAHD_FRAME_FORMAT_INTERLACED_BOTTOM_FIELD_FIRST = 2
  );
  {$EXTERNALSYM _DXVAHD_FRAME_FORMAT}
  DXVAHD_FRAME_FORMAT = _DXVAHD_FRAME_FORMAT;
  {$EXTERNALSYM DXVAHD_FRAME_FORMAT}

  PDXVAHD_DEVICE_USAGE = ^_DXVAHD_DEVICE_USAGE;
  _DXVAHD_DEVICE_USAGE                  = (
    DXVAHD_DEVICE_USAGE_PLAYBACK_NORMAL = 0,
    DXVAHD_DEVICE_USAGE_OPTIMAL_SPEED   = 1,
    DXVAHD_DEVICE_USAGE_OPTIMAL_QUALITY = 2
  );
  {$EXTERNALSYM _DXVAHD_DEVICE_USAGE}
  DXVAHD_DEVICE_USAGE = _DXVAHD_DEVICE_USAGE;
  {$EXTERNALSYM DXVAHD_DEVICE_USAGE}

  PDXVAHD_SURFACE_TYPE = ^_DXVAHD_SURFACE_TYPE;
  _DXVAHD_SURFACE_TYPE                      = (
    DXVAHD_SURFACE_TYPE_VIDEO_INPUT         = 0,
    DXVAHD_SURFACE_TYPE_VIDEO_INPUT_PRIVATE = 1,
    DXVAHD_SURFACE_TYPE_VIDEO_OUTPUT        = 2
  );
  {$EXTERNALSYM _DXVAHD_SURFACE_TYPE}
  DXVAHD_SURFACE_TYPE = _DXVAHD_SURFACE_TYPE;
  {$EXTERNALSYM DXVAHD_SURFACE_TYPE}

  PDXVAHD_DEVICE_TYPE = ^_DXVAHD_DEVICE_TYPE;
  _DXVAHD_DEVICE_TYPE            = (
    DXVAHD_DEVICE_TYPE_HARDWARE  = 0,
    DXVAHD_DEVICE_TYPE_SOFTWARE  = 1,
    DXVAHD_DEVICE_TYPE_REFERENCE = 2,
    DXVAHD_DEVICE_TYPE_OTHER     = 3
  );
  {$EXTERNALSYM _DXVAHD_DEVICE_TYPE}
  DXVAHD_DEVICE_TYPE = _DXVAHD_DEVICE_TYPE;
  {$EXTERNALSYM DXVAHD_DEVICE_TYPE}

type
  PDXVAHD_DEVICE_CAPS = ^_DXVAHD_DEVICE_CAPS;
  _DXVAHD_DEVICE_CAPS = DWord;
  {$EXTERNALSYM _DXVAHD_DEVICE_CAPS}
  DXVAHD_DEVICE_CAPS = _DXVAHD_DEVICE_CAPS;
  {$EXTERNALSYM DXVAHD_DEVICE_CAPS}
const
  DXVAHD_DEVICE_CAPS_LINEAR_SPACE            = DXVAHD_DEVICE_CAPS($1);
  DXVAHD_DEVICE_CAPS_xvYCC                   = DXVAHD_DEVICE_CAPS($2);
  DXVAHD_DEVICE_CAPS_RGB_RANGE_CONVERSION    = DXVAHD_DEVICE_CAPS($4);
  DXVAHD_DEVICE_CAPS_YCbCr_MATRIX_CONVERSION = DXVAHD_DEVICE_CAPS($8);


type
  PDXVAHD_FEATURE_CAPS = ^_DXVAHD_FEATURE_CAPS;
  _DXVAHD_FEATURE_CAPS = DWord;
  {$EXTERNALSYM _DXVAHD_FEATURE_CAPS}
  DXVAHD_FEATURE_CAPS = _DXVAHD_FEATURE_CAPS;
  {$EXTERNALSYM DXVAHD_FEATURE_CAPS}
const
  DXVAHD_FEATURE_CAPS_ALPHA_FILL    = DXVAHD_FEATURE_CAPS($1);
  DXVAHD_FEATURE_CAPS_CONSTRICTION  = DXVAHD_FEATURE_CAPS($2);
  DXVAHD_FEATURE_CAPS_LUMA_KEY      = DXVAHD_FEATURE_CAPS($4);
  DXVAHD_FEATURE_CAPS_ALPHA_PALETTE = DXVAHD_FEATURE_CAPS($8);


type
  PDXVAHD_FILTER_CAPS = ^_DXVAHD_FILTER_CAPS;
  _DXVAHD_FILTER_CAPS = DWord;
  {$EXTERNALSYM _DXVAHD_FILTER_CAPS}
  DXVAHD_FILTER_CAPS = _DXVAHD_FILTER_CAPS;
  {$EXTERNALSYM DXVAHD_FILTER_CAPS}
const
  DXVAHD_FILTER_CAPS_BRIGHTNESS         = DXVAHD_FILTER_CAPS($1);
  DXVAHD_FILTER_CAPS_CONTRAST           = DXVAHD_FILTER_CAPS($2);
  DXVAHD_FILTER_CAPS_HUE                = DXVAHD_FILTER_CAPS($4);
  DXVAHD_FILTER_CAPS_SATURATION         = DXVAHD_FILTER_CAPS($8);
  DXVAHD_FILTER_CAPS_NOISE_REDUCTION    = DXVAHD_FILTER_CAPS($10);
  DXVAHD_FILTER_CAPS_EDGE_ENHANCEMENT   = DXVAHD_FILTER_CAPS($20);
  DXVAHD_FILTER_CAPS_ANAMORPHIC_SCALING = DXVAHD_FILTER_CAPS($40);

type
  PDXVAHD_INPUT_FORMAT_CAPS = ^_DXVAHD_INPUT_FORMAT_CAPS;
  _DXVAHD_INPUT_FORMAT_CAPS = DWord;
  {$EXTERNALSYM _DXVAHD_INPUT_FORMAT_CAPS}
  DXVAHD_INPUT_FORMAT_CAPS = _DXVAHD_INPUT_FORMAT_CAPS;
  {$EXTERNALSYM DXVAHD_INPUT_FORMAT_CAPS}
const
  DXVAHD_INPUT_FORMAT_CAPS_RGB_INTERLACED     = DXVAHD_INPUT_FORMAT_CAPS($1);
  DXVAHD_INPUT_FORMAT_CAPS_RGB_PROCAMP        = DXVAHD_INPUT_FORMAT_CAPS($2);
  DXVAHD_INPUT_FORMAT_CAPS_RGB_LUMA_KEY       = DXVAHD_INPUT_FORMAT_CAPS($4);
  DXVAHD_INPUT_FORMAT_CAPS_PALETTE_INTERLACED = DXVAHD_INPUT_FORMAT_CAPS($8);

type
  PDXVAHD_PROCESSOR_CAPS = ^_DXVAHD_PROCESSOR_CAPS;
  _DXVAHD_PROCESSOR_CAPS = DWord;
  {$EXTERNALSYM _DXVAHD_PROCESSOR_CAPS}
  DXVAHD_PROCESSOR_CAPS = _DXVAHD_PROCESSOR_CAPS;
  {$EXTERNALSYM DXVAHD_PROCESSOR_CAPS}
const
  DXVAHD_PROCESSOR_CAPS_DEINTERLACE_BLEND               = DXVAHD_PROCESSOR_CAPS($1);
  DXVAHD_PROCESSOR_CAPS_DEINTERLACE_BOB                 = DXVAHD_PROCESSOR_CAPS($2);
  DXVAHD_PROCESSOR_CAPS_DEINTERLACE_ADAPTIVE            = DXVAHD_PROCESSOR_CAPS($4);
  DXVAHD_PROCESSOR_CAPS_DEINTERLACE_MOTION_COMPENSATION = DXVAHD_PROCESSOR_CAPS($8);
  DXVAHD_PROCESSOR_CAPS_INVERSE_TELECINE                = DXVAHD_PROCESSOR_CAPS($10);
  DXVAHD_PROCESSOR_CAPS_FRAME_RATE_CONVERSION           = DXVAHD_PROCESSOR_CAPS($20);

type

  PDXVAHD_ITELECINE_CAPS = ^_DXVAHD_ITELECINE_CAPS;
  {$EXTERNALSYM _DXVAHD_ITELECINE_CAPS}
  _DXVAHD_ITELECINE_CAPS               = (
    DXVAHD_ITELECINE_CAPS_32           = $1,
    DXVAHD_ITELECINE_CAPS_22           = $2,
    DXVAHD_ITELECINE_CAPS_2224         = $4,
    DXVAHD_ITELECINE_CAPS_2332         = $8,
    DXVAHD_ITELECINE_CAPS_32322        = $10,
    DXVAHD_ITELECINE_CAPS_55           = $20,
    DXVAHD_ITELECINE_CAPS_64           = $40,
    DXVAHD_ITELECINE_CAPS_87           = $80,
    DXVAHD_ITELECINE_CAPS_222222222223 = $100,
    DXVAHD_ITELECINE_CAPS_OTHER        = $80000000 -1);
  {$EXTERNALSYM DXVAHD_ITELECINE_CAPS}
  DXVAHD_ITELECINE_CAPS = _DXVAHD_ITELECINE_CAPS;

  PDXVAHD_FILTER = ^_DXVAHD_FILTER;
  {$EXTERNALSYM _DXVAHD_FILTER}
  _DXVAHD_FILTER                     = (
    DXVAHD_FILTER_BRIGHTNESS         = 0,
    DXVAHD_FILTER_CONTRAST           = 1,
    DXVAHD_FILTER_HUE                = 2,
    DXVAHD_FILTER_SATURATION         = 3,
    DXVAHD_FILTER_NOISE_REDUCTION    = 4,
    DXVAHD_FILTER_EDGE_ENHANCEMENT   = 5,
    DXVAHD_FILTER_ANAMORPHIC_SCALING = 6);
  {$EXTERNALSYM DXVAHD_FILTER}
  DXVAHD_FILTER = _DXVAHD_FILTER;

  PDXVAHD_BLT_STATE = ^_DXVAHD_BLT_STATE;
  {$EXTERNALSYM _DXVAHD_BLT_STATE}
  _DXVAHD_BLT_STATE                     = (
    DXVAHD_BLT_STATE_TARGET_RECT        = 0,
    DXVAHD_BLT_STATE_BACKGROUND_COLOR   = 1,
    DXVAHD_BLT_STATE_OUTPUT_COLOR_SPACE = 2,
    DXVAHD_BLT_STATE_ALPHA_FILL         = 3,
    DXVAHD_BLT_STATE_CONSTRICTION       = 4,
    DXVAHD_BLT_STATE_PRIVATE            = 1000);
  {$EXTERNALSYM DXVAHD_BLT_STATE}
  DXVAHD_BLT_STATE = _DXVAHD_BLT_STATE;

  PDXVAHD_ALPHA_FILL_MODE = ^_DXVAHD_ALPHA_FILL_MODE;
  {$EXTERNALSYM _DXVAHD_ALPHA_FILL_MODE}
  _DXVAHD_ALPHA_FILL_MODE                = (
    DXVAHD_ALPHA_FILL_MODE_OPAQUE        = 0,
    DXVAHD_ALPHA_FILL_MODE_BACKGROUND    = 1,
    DXVAHD_ALPHA_FILL_MODE_DESTINATION   = 2,
    DXVAHD_ALPHA_FILL_MODE_SOURCE_STREAM = 3);
  {$EXTERNALSYM DXVAHD_ALPHA_FILL_MODE}
  DXVAHD_ALPHA_FILL_MODE = _DXVAHD_ALPHA_FILL_MODE;

  PDXVAHD_STREAM_STATE = ^_DXVAHD_STREAM_STATE;
  {$EXTERNALSYM _DXVAHD_STREAM_STATE}
  _DXVAHD_STREAM_STATE                            = (
    DXVAHD_STREAM_STATE_D3DFORMAT                 = 0,
    DXVAHD_STREAM_STATE_FRAME_FORMAT              = 1,
    DXVAHD_STREAM_STATE_INPUT_COLOR_SPACE         = 2,
    DXVAHD_STREAM_STATE_OUTPUT_RATE               = 3,
    DXVAHD_STREAM_STATE_SOURCE_RECT               = 4,
    DXVAHD_STREAM_STATE_DESTINATION_RECT          = 5,
    DXVAHD_STREAM_STATE_ALPHA                     = 6,
    DXVAHD_STREAM_STATE_PALETTE                   = 7,
    DXVAHD_STREAM_STATE_LUMA_KEY                  = 8,
    DXVAHD_STREAM_STATE_ASPECT_RATIO              = 9,
    DXVAHD_STREAM_STATE_FILTER_BRIGHTNESS         = 100,
    DXVAHD_STREAM_STATE_FILTER_CONTRAST           = 101,
    DXVAHD_STREAM_STATE_FILTER_HUE                = 102,
    DXVAHD_STREAM_STATE_FILTER_SATURATION         = 103,
    DXVAHD_STREAM_STATE_FILTER_NOISE_REDUCTION    = 104,
    DXVAHD_STREAM_STATE_FILTER_EDGE_ENHANCEMENT   = 105,
    DXVAHD_STREAM_STATE_FILTER_ANAMORPHIC_SCALING = 106,
    DXVAHD_STREAM_STATE_PRIVATE                   = 1000);
  {$EXTERNALSYM DXVAHD_STREAM_STATE}
  DXVAHD_STREAM_STATE = _DXVAHD_STREAM_STATE;

  PDXVAHD_OUTPUT_RATE = ^_DXVAHD_OUTPUT_RATE;
  {$EXTERNALSYM _DXVAHD_OUTPUT_RATE}
  _DXVAHD_OUTPUT_RATE         = (
    DXVAHD_OUTPUT_RATE_NORMAL = 0,
    DXVAHD_OUTPUT_RATE_HALF   = 1,
    DXVAHD_OUTPUT_RATE_CUSTOM = 2);
  {$EXTERNALSYM DXVAHD_OUTPUT_RATE}
  DXVAHD_OUTPUT_RATE = _DXVAHD_OUTPUT_RATE;

  PDXVAHD_RATIONAL = ^_DXVAHD_RATIONAL;
  {$EXTERNALSYM _DXVAHD_RATIONAL}
  _DXVAHD_RATIONAL = record
    Numerator: UINT;
    Denominator: UINT;
  end;
  {$EXTERNALSYM DXVAHD_RATIONAL}
  DXVAHD_RATIONAL = _DXVAHD_RATIONAL;

  PDXVAHD_COLOR_RGBA = ^_DXVAHD_COLOR_RGBA;
  {$EXTERNALSYM _DXVAHD_COLOR_RGBA}
  _DXVAHD_COLOR_RGBA = record
    R: Single;
    G: Single;
    B: Single;
    A: Single;
  end;
  {$EXTERNALSYM DXVAHD_COLOR_RGBA}
  DXVAHD_COLOR_RGBA = _DXVAHD_COLOR_RGBA;

  PDXVAHD_COLOR_YCbCrA = ^_DXVAHD_COLOR_YCbCrA;
  {$EXTERNALSYM _DXVAHD_COLOR_YCbCrA}
  _DXVAHD_COLOR_YCbCrA = record
    Y: Single;
    Cb: Single;
    Cr: Single;
    A: Single;
  end;
  {$EXTERNALSYM DXVAHD_COLOR_YCbCrA}
  DXVAHD_COLOR_YCbCrA = _DXVAHD_COLOR_YCbCrA;

  PDXVAHD_COLOR = ^_DXVAHD_COLOR;
  {$EXTERNALSYM _DXVAHD_COLOR}
  _DXVAHD_COLOR = record
    RGB: DXVAHD_COLOR_RGBA;
    YCbCr: DXVAHD_COLOR_YCbCrA;
  end;
  {$EXTERNALSYM DXVAHD_COLOR}
  DXVAHD_COLOR = _DXVAHD_COLOR;

  PDXVAHD_CONTENT_DESC = ^_DXVAHD_CONTENT_DESC;
  {$EXTERNALSYM _DXVAHD_CONTENT_DESC}
  _DXVAHD_CONTENT_DESC = record
    InputFrameFormat: DXVAHD_FRAME_FORMAT;
    InputFrameRate: DXVAHD_RATIONAL;
    InputWidth: UINT;
    InputHeight: UINT;
    OutputFrameRate: DXVAHD_RATIONAL;
    OutputWidth: UINT;
    OutputHeight: UINT;
  end;
  {$EXTERNALSYM DXVAHD_CONTENT_DESC}
  DXVAHD_CONTENT_DESC = _DXVAHD_CONTENT_DESC;

  PDXVAHD_VPDEVCAPS = ^_DXVAHD_VPDEVCAPS;
  {$EXTERNALSYM _DXVAHD_VPDEVCAPS}
  _DXVAHD_VPDEVCAPS = record
    DeviceType: DXVAHD_DEVICE_TYPE;
    DeviceCaps: UINT;
    FeatureCaps: UINT;
    FilterCaps: UINT;
    InputFormatCaps: UINT;
    InputPool: D3DPOOL;
    OutputFormatCount: UINT;
    InputFormatCount: UINT;
    VideoProcessorCount: UINT;
    MaxInputStreams: UINT;
    MaxStreamStates: UINT;
  end;
  {$EXTERNALSYM DXVAHD_VPDEVCAPS}
  DXVAHD_VPDEVCAPS = _DXVAHD_VPDEVCAPS;

  PDXVAHD_VPCAPS = ^_DXVAHD_VPCAPS;
  {$EXTERNALSYM _DXVAHD_VPCAPS}
  _DXVAHD_VPCAPS = record
    VPGuid: TGUID;
    PastFrames: UINT;
    FutureFrames: UINT;
    ProcessorCaps: UINT;
    ITelecineCaps: UINT;
    CustomRateCount: UINT;
  end;
  {$EXTERNALSYM DXVAHD_VPCAPS}
  DXVAHD_VPCAPS = _DXVAHD_VPCAPS;

  PDXVAHD_CUSTOM_RATE_DATA = ^_DXVAHD_CUSTOM_RATE_DATA;
  {$EXTERNALSYM _DXVAHD_CUSTOM_RATE_DATA}
  _DXVAHD_CUSTOM_RATE_DATA = record
    CustomRate: DXVAHD_RATIONAL;
    OutputFrames: UINT;
    InputInterlaced: BOOL;
    InputFramesOrFields: UINT;
  end;
  {$EXTERNALSYM DXVAHD_CUSTOM_RATE_DATA}
  DXVAHD_CUSTOM_RATE_DATA = _DXVAHD_CUSTOM_RATE_DATA;

  PDXVAHD_FILTER_RANGE_DATA = ^_DXVAHD_FILTER_RANGE_DATA;
  {$EXTERNALSYM _DXVAHD_FILTER_RANGE_DATA}
  _DXVAHD_FILTER_RANGE_DATA = record
    Minimum: Integer;
    Maximum: Integer;
    Default: Integer;
    Multiplier: Single;
  end;
  {$EXTERNALSYM DXVAHD_FILTER_RANGE_DATA}
  DXVAHD_FILTER_RANGE_DATA = _DXVAHD_FILTER_RANGE_DATA;

  PDXVAHD_BLT_STATE_TARGET_RECT_DATA = ^_DXVAHD_BLT_STATE_TARGET_RECT_DATA;
  {$EXTERNALSYM _DXVAHD_BLT_STATE_TARGET_RECT_DATA}
  _DXVAHD_BLT_STATE_TARGET_RECT_DATA = record
    Enable: BOOL;
    TargetRect: TRect;
  end;
  {$EXTERNALSYM DXVAHD_BLT_STATE_TARGET_RECT_DATA}
  DXVAHD_BLT_STATE_TARGET_RECT_DATA = _DXVAHD_BLT_STATE_TARGET_RECT_DATA;

  PDXVAHD_BLT_STATE_BACKGROUND_COLOR_DATA = ^_DXVAHD_BLT_STATE_BACKGROUND_COLOR_DATA;
  {$EXTERNALSYM _DXVAHD_BLT_STATE_BACKGROUND_COLOR_DATA}
  _DXVAHD_BLT_STATE_BACKGROUND_COLOR_DATA = record
    YCbCr: BOOL;
    BackgroundColor: DXVAHD_COLOR;
  end;
  {$EXTERNALSYM DXVAHD_BLT_STATE_BACKGROUND_COLOR_DATA}
  DXVAHD_BLT_STATE_BACKGROUND_COLOR_DATA = _DXVAHD_BLT_STATE_BACKGROUND_COLOR_DATA;


//  _DXVAHD_BLT_STATE_OUTPUT_COLOR_SPACE_DATA = record
//    {
//    union
//        struct
//            {
//            Usage: UINT 1;
//            RGB_Range: UINT 1;
//            YCbCr_Matrix: UINT 1;
//            YCbCr_xvYCC: UINT 1;
//            Reserved: UINT 28;
//            }
//        Value: UINT;
//   end;
//   DXVAHD_BLT_STATE_OUTPUT_COLOR_SPACE_DATA = _DXVAHD_BLT_STATE_OUTPUT_COLOR_SPACE_DATA;

  //
  PDXVAHD_BLT_STATE_OUTPUT_COLOR_SPACE_DATA = ^_DXVAHD_BLT_STATE_OUTPUT_COLOR_SPACE_DATA;
  {$EXTERNALSYM _DXVAHD_BLT_STATE_OUTPUT_COLOR_SPACE_DATA}
  _DXVAHD_BLT_STATE_OUTPUT_COLOR_SPACE_DATA = record
  public
    Value: UINT; // Same as 'flags' used to acces all bitvalues
  private
    function ReadBits(const aIndex: Integer): Integer;
    procedure WriteBits(const aIndex: Integer; const aValue: Integer);
  public
    property Usage: Integer index $0001 read ReadBits write WriteBits;        // 1 bit at offset 0
    property RGB_Range: Integer index $0101 read ReadBits write WriteBits;    // 1 bit at offset 1
    property YCbCr_Matrix: Integer index $0201 read ReadBits write WriteBits; // 1 bit at offset 2
    property YCbCr_xvYCC: Integer index $0301 read ReadBits write WriteBits;  // 1 bit at offset 3
    property Reserved: Integer index $0428 read ReadBits write WriteBits;     // 28 bits at offset 4
  end;
  {$EXTERNALSYM DXVAHD_BLT_STATE_OUTPUT_COLOR_SPACE_DATA}
  DXVAHD_BLT_STATE_OUTPUT_COLOR_SPACE_DATA = _DXVAHD_BLT_STATE_OUTPUT_COLOR_SPACE_DATA;

  PDXVAHD_BLT_STATE_ALPHA_FILL_DATA = ^_DXVAHD_BLT_STATE_ALPHA_FILL_DATA;
  {$EXTERNALSYM _DXVAHD_BLT_STATE_ALPHA_FILL_DATA}
  _DXVAHD_BLT_STATE_ALPHA_FILL_DATA = record
    Mode: DXVAHD_ALPHA_FILL_MODE;
    StreamNumber: UINT;
  end;
  {$EXTERNALSYM DXVAHD_BLT_STATE_ALPHA_FILL_DATA}
  DXVAHD_BLT_STATE_ALPHA_FILL_DATA = _DXVAHD_BLT_STATE_ALPHA_FILL_DATA;

  PDXVAHD_BLT_STATE_CONSTRICTION_DATA = ^_DXVAHD_BLT_STATE_CONSTRICTION_DATA;
  {$EXTERNALSYM _DXVAHD_BLT_STATE_CONSTRICTION_DATA}
  _DXVAHD_BLT_STATE_CONSTRICTION_DATA = record
    Enable: BOOL;
    Size: SIZE;
  end;
  {$EXTERNALSYM DXVAHD_BLT_STATE_CONSTRICTION_DATA}
  DXVAHD_BLT_STATE_CONSTRICTION_DATA = _DXVAHD_BLT_STATE_CONSTRICTION_DATA;

  PDXVAHD_BLT_STATE_PRIVATE_DATA = ^_DXVAHD_BLT_STATE_PRIVATE_DATA;
  {$EXTERNALSYM _DXVAHD_BLT_STATE_PRIVATE_DATA}
  _DXVAHD_BLT_STATE_PRIVATE_DATA = record
    Guid: TGUID;
    DataSize: UINT;
    pData: Pointer;
  end;
  {$EXTERNALSYM DXVAHD_BLT_STATE_PRIVATE_DATA}
  DXVAHD_BLT_STATE_PRIVATE_DATA = _DXVAHD_BLT_STATE_PRIVATE_DATA;

  PDXVAHD_STREAM_STATE_D3DFORMAT_DATA = ^_DXVAHD_STREAM_STATE_D3DFORMAT_DATA;
  {$EXTERNALSYM _DXVAHD_STREAM_STATE_D3DFORMAT_DATA}
  _DXVAHD_STREAM_STATE_D3DFORMAT_DATA = record
    Format: D3DFORMAT;
  end;
  {$EXTERNALSYM DXVAHD_STREAM_STATE_D3DFORMAT_DATA}
  DXVAHD_STREAM_STATE_D3DFORMAT_DATA = _DXVAHD_STREAM_STATE_D3DFORMAT_DATA;

  PDXVAHD_STREAM_STATE_FRAME_FORMAT_DATA = ^_DXVAHD_STREAM_STATE_FRAME_FORMAT_DATA;
  {$EXTERNALSYM _DXVAHD_STREAM_STATE_FRAME_FORMAT_DATA}
  _DXVAHD_STREAM_STATE_FRAME_FORMAT_DATA = record
    FrameFormat: DXVAHD_FRAME_FORMAT;
  end;
  {$EXTERNALSYM DXVAHD_STREAM_STATE_FRAME_FORMAT_DATA}
  DXVAHD_STREAM_STATE_FRAME_FORMAT_DATA = _DXVAHD_STREAM_STATE_FRAME_FORMAT_DATA;


//typedef struct _DXVAHD_STREAM_STATE_INPUT_COLOR_SPACE_DATA
//    {
//    union
//        {
//        struct
//            {
//            UINT Type : 1;
//            UINT RGB_Range  : 1;
//            UINT YCbCr_Matrix : 1;
//            UINT YCbCr_xvYCC  : 1;
//            UINT Reserved : 28;
//            }   ;
//        UINT Value;
//        }   ;
//    }   DXVAHD_STREAM_STATE_INPUT_COLOR_SPACE_DATA;

  PDXVAHD_STREAM_STATE_INPUT_COLOR_SPACE_DATA = ^_DXVAHD_STREAM_STATE_INPUT_COLOR_SPACE_DATA;
  {$EXTERNALSYM _DXVAHD_STREAM_STATE_INPUT_COLOR_SPACE_DATA}
  _DXVAHD_STREAM_STATE_INPUT_COLOR_SPACE_DATA = record
  public
    Value: UINT; // Same as 'flags' used to acces all bitvalues
  private
    function ReadBits(const aIndex: Integer): Integer;
    procedure WriteBits(const aIndex: Integer; const aValue: Integer);
  public
    property _Type: Integer index $0001 read ReadBits write WriteBits;        // 1 bit at offset 0
    property RGB_Range: Integer index $0101 read ReadBits write WriteBits;    // 1 bit at offset 1
    property YCbCr_Matrix: Integer index $0201 read ReadBits write WriteBits; // 1 bit at offset 2
    property YCbCr_xvYCC: Integer index $0301 read ReadBits write WriteBits;  // 1 bit at offset 3
    property Reserved: Integer index $0428 read ReadBits write WriteBits;     // 28 bits at offset 4
  end;
  {$EXTERNALSYM DXVAHD_STREAM_STATE_INPUT_COLOR_SPACE_DATA}
  DXVAHD_STREAM_STATE_INPUT_COLOR_SPACE_DATA = _DXVAHD_STREAM_STATE_INPUT_COLOR_SPACE_DATA;


  PDXVAHD_STREAM_STATE_OUTPUT_RATE_DATA = ^_DXVAHD_STREAM_STATE_OUTPUT_RATE_DATA;
  {$EXTERNALSYM _DXVAHD_STREAM_STATE_OUTPUT_RATE_DATA}
  _DXVAHD_STREAM_STATE_OUTPUT_RATE_DATA = record
    RepeatFrame: BOOL;
    OutputRate: DXVAHD_OUTPUT_RATE;
    CustomRate: DXVAHD_RATIONAL;
  end;
  {$EXTERNALSYM DXVAHD_STREAM_STATE_OUTPUT_RATE_DATA}
  DXVAHD_STREAM_STATE_OUTPUT_RATE_DATA = _DXVAHD_STREAM_STATE_OUTPUT_RATE_DATA;

  PDXVAHD_STREAM_STATE_SOURCE_RECT_DATA = ^_DXVAHD_STREAM_STATE_SOURCE_RECT_DATA;
  {$EXTERNALSYM _DXVAHD_STREAM_STATE_SOURCE_RECT_DATA}
  _DXVAHD_STREAM_STATE_SOURCE_RECT_DATA = record
    Enable: BOOL;
    SourceRect: TRect;
  end;
  {$EXTERNALSYM DXVAHD_STREAM_STATE_SOURCE_RECT_DATA}
  DXVAHD_STREAM_STATE_SOURCE_RECT_DATA = _DXVAHD_STREAM_STATE_SOURCE_RECT_DATA;

  PDXVAHD_STREAM_STATE_DESTINATION_RECT_DATA = ^_DXVAHD_STREAM_STATE_DESTINATION_RECT_DATA;
  {$EXTERNALSYM _DXVAHD_STREAM_STATE_DESTINATION_RECT_DATA}
  _DXVAHD_STREAM_STATE_DESTINATION_RECT_DATA = record
    Enable: BOOL;
    DestinationRect: TRect;
  end;
  {$EXTERNALSYM DXVAHD_STREAM_STATE_DESTINATION_RECT_DATA}
  DXVAHD_STREAM_STATE_DESTINATION_RECT_DATA = _DXVAHD_STREAM_STATE_DESTINATION_RECT_DATA;

  PDXVAHD_STREAM_STATE_ALPHA_DATA = ^_DXVAHD_STREAM_STATE_ALPHA_DATA;
  {$EXTERNALSYM _DXVAHD_STREAM_STATE_ALPHA_DATA}
  _DXVAHD_STREAM_STATE_ALPHA_DATA = record
    Enable: BOOL;
    Alpha: Single;
  end;
  {$EXTERNALSYM DXVAHD_STREAM_STATE_ALPHA_DATA}
  DXVAHD_STREAM_STATE_ALPHA_DATA = _DXVAHD_STREAM_STATE_ALPHA_DATA;

  PDXVAHD_STREAM_STATE_PALETTE_DATA = ^_DXVAHD_STREAM_STATE_PALETTE_DATA;
  {$EXTERNALSYM _DXVAHD_STREAM_STATE_PALETTE_DATA}
  _DXVAHD_STREAM_STATE_PALETTE_DATA = record
    Count: UINT;
    pEntries: ^D3DCOLOR;
  end;
  {$EXTERNALSYM DXVAHD_STREAM_STATE_PALETTE_DATA}
  DXVAHD_STREAM_STATE_PALETTE_DATA = _DXVAHD_STREAM_STATE_PALETTE_DATA;

  PDXVAHD_STREAM_STATE_LUMA_KEY_DATA = ^_DXVAHD_STREAM_STATE_LUMA_KEY_DATA;
  {$EXTERNALSYM _DXVAHD_STREAM_STATE_LUMA_KEY_DATA}
  _DXVAHD_STREAM_STATE_LUMA_KEY_DATA = record
    Enable: BOOL;
    Lower: Single;
    Upper: Single;
  end;
  {$EXTERNALSYM DXVAHD_STREAM_STATE_LUMA_KEY_DATA}
  DXVAHD_STREAM_STATE_LUMA_KEY_DATA = _DXVAHD_STREAM_STATE_LUMA_KEY_DATA;

  PDXVAHD_STREAM_STATE_ASPECT_RATIO_DATA = ^_DXVAHD_STREAM_STATE_ASPECT_RATIO_DATA;
  {$EXTERNALSYM _DXVAHD_STREAM_STATE_ASPECT_RATIO_DATA}
  _DXVAHD_STREAM_STATE_ASPECT_RATIO_DATA = record
    Enable: BOOL;
    SourceAspectRatio: DXVAHD_RATIONAL;
    DestinationAspectRatio: DXVAHD_RATIONAL;
  end;
  {$EXTERNALSYM DXVAHD_STREAM_STATE_ASPECT_RATIO_DATA}
  DXVAHD_STREAM_STATE_ASPECT_RATIO_DATA = _DXVAHD_STREAM_STATE_ASPECT_RATIO_DATA;

  PDXVAHD_STREAM_STATE_FILTER_DATA = ^_DXVAHD_STREAM_STATE_FILTER_DATA;
  {$EXTERNALSYM _DXVAHD_STREAM_STATE_FILTER_DATA}
  _DXVAHD_STREAM_STATE_FILTER_DATA = record
    Enable: BOOL;
    Level: Integer;
  end;
  {$EXTERNALSYM DXVAHD_STREAM_STATE_FILTER_DATA}
  DXVAHD_STREAM_STATE_FILTER_DATA = _DXVAHD_STREAM_STATE_FILTER_DATA;

  PDXVAHD_STREAM_STATE_PRIVATE_DATA = ^_DXVAHD_STREAM_STATE_PRIVATE_DATA;
  {$EXTERNALSYM _DXVAHD_STREAM_STATE_PRIVATE_DATA}
  _DXVAHD_STREAM_STATE_PRIVATE_DATA = record
    Guid: TGUID;
    DataSize: UINT;
    pData: Pointer;
  end;
  {$EXTERNALSYM DXVAHD_STREAM_STATE_PRIVATE_DATA}
  DXVAHD_STREAM_STATE_PRIVATE_DATA = _DXVAHD_STREAM_STATE_PRIVATE_DATA;

  PDXVAHD_STREAM_DATA = ^_DXVAHD_STREAM_DATA;
  {$EXTERNALSYM _DXVAHD_STREAM_DATA}
  _DXVAHD_STREAM_DATA = record
    Enable: BOOL;
    OutputIndex: UINT;
    InputFrameOrField: UINT;
    PastFrames: UINT;
    FutureFrames: UINT;
    ppPastSurfaces: ^IDirect3DSurface9;
    pInputSurface: ^IDirect3DSurface9;
    ppFutureSurfaces: ^IDirect3DSurface9;
  end;
  {$EXTERNALSYM DXVAHD_STREAM_DATA}
  DXVAHD_STREAM_DATA = _DXVAHD_STREAM_DATA;

  PDXVAHD_STREAM_STATE_PRIVATE_IVTC_DATA = ^_DXVAHD_STREAM_STATE_PRIVATE_IVTC_DATA;
  {$EXTERNALSYM _DXVAHD_STREAM_STATE_PRIVATE_IVTC_DATA}
  _DXVAHD_STREAM_STATE_PRIVATE_IVTC_DATA = record
    Enable: BOOL;
    ITelecineFlags: UINT;
    Frames: UINT;
    InputField: UINT;
  end;
  {$EXTERNALSYM DXVAHD_STREAM_STATE_PRIVATE_IVTC_DATA}
  DXVAHD_STREAM_STATE_PRIVATE_IVTC_DATA = _DXVAHD_STREAM_STATE_PRIVATE_IVTC_DATA;




  // Callback functions
  // ==================

  {$EXTERNALSYM PDXVAHDSW_CreateDevice}
  PDXVAHDSW_CreateDevice = function(pD3DDevice: IDirect3DDevice9Ex;
                                    out phDevice: PHandle): HResult; stdcall;

  {$EXTERNALSYM PDXVAHDSW_ProposeVideoPrivateFormat}
  PDXVAHDSW_ProposeVideoPrivateFormat = function(hDevice: THandle;
                                                 var pFormat: D3DFORMAT): HResult; stdcall;

  {$EXTERNALSYM PDXVAHDSW_GetVideoProcessorDeviceCaps}
  PDXVAHDSW_GetVideoProcessorDeviceCaps = function(hDevice: THandle;
                                                   pContentDesc: DXVAHD_CONTENT_DESC;
                                                   Usage: DXVAHD_DEVICE_USAGE;
                                                   out pCaps: DXVAHD_VPDEVCAPS): HResult; stdcall;

  {$EXTERNALSYM PDXVAHDSW_GetVideoProcessorOutputFormats}
  PDXVAHDSW_GetVideoProcessorOutputFormats = function(hDevice: THandle;
                                                      pContentDesc: DXVAHD_CONTENT_DESC;
                                                      Usage: DXVAHD_DEVICE_USAGE;
                                                      Count: UINT;
                                                      out pFormats: D3DFORMAT): HResult; stdcall;

  {$EXTERNALSYM PDXVAHDSW_GetVideoProcessorInputFormats}
  PDXVAHDSW_GetVideoProcessorInputFormats = function(hDevice: THandle;
                                                     pContentDesc: DXVAHD_CONTENT_DESC;
                                                     Usage: DXVAHD_DEVICE_USAGE;
                                                     Count: UINT;
                                                     out pFormats: D3DFORMAT): HResult; stdcall;

  {$EXTERNALSYM PDXVAHDSW_GetVideoProcessorCaps}
  PDXVAHDSW_GetVideoProcessorCaps = function(hDevice: THandle;
                                             pContentDesc: DXVAHD_CONTENT_DESC;
                                             Usage: DXVAHD_DEVICE_USAGE;
                                             Count: UINT;
                                             out pCaps: DXVAHD_VPCAPS): HResult; stdcall;

  {$EXTERNALSYM PDXVAHDSW_GetVideoProcessorCustomRates}
  PDXVAHDSW_GetVideoProcessorCustomRates = function(hDevice: THandle;
                                                    const pVPGuid: TGUID;
                                                    Count: UINT;
                                                    out pRates: DXVAHD_CUSTOM_RATE_DATA): HResult; stdcall;

  {$EXTERNALSYM PDXVAHDSW_GetVideoProcessorFilterRange}
  PDXVAHDSW_GetVideoProcessorFilterRange = function(hDevice: THandle;
                                                    Filter: DXVAHD_FILTER;
                                                    out pRange: DXVAHD_FILTER_RANGE_DATA): HResult; stdcall;

  {$EXTERNALSYM PDXVAHDSW_DestroyDevice}
  PDXVAHDSW_DestroyDevice = function(const hDevice: THandle): HResult; stdcall;

  {$EXTERNALSYM PDXVAHDSW_CreateVideoProcessor}
  PDXVAHDSW_CreateVideoProcessor = function(hDevice: THandle;
                                            const pVPGuid: TGUID;
                                            out phVideoProcessor: THandle): HResult; stdcall;

  {$EXTERNALSYM PDXVAHDSW_SetVideoProcessBltState}
  PDXVAHDSW_SetVideoProcessBltState = function(hVideoProcessor: THandle;
                                               State: DXVAHD_BLT_STATE;
                                               DataSize: UINT;
                                               pData: Pointer): HResult; stdcall;

  {$EXTERNALSYM PDXVAHDSW_GetVideoProcessBltStatePrivate}
  PDXVAHDSW_GetVideoProcessBltStatePrivate = function(hVideoProcessor: THandle;
                                                      var pData: DXVAHD_BLT_STATE_PRIVATE_DATA): HResult; stdcall;

  {$EXTERNALSYM PDXVAHDSW_SetVideoProcessStreamState}
  PDXVAHDSW_SetVideoProcessStreamState = function(hVideoProcessor: THandle;
                                                  StreamNumber: UINT;
                                                  State: DXVAHD_STREAM_STATE;
                                                  DataSize: UINT;
                                                  pData: Pointer): HResult; stdcall;

  {$EXTERNALSYM PDXVAHDSW_GetVideoProcessStreamStatePrivate}
  PDXVAHDSW_GetVideoProcessStreamStatePrivate = function(hVideoProcessor: THandle;
                                                         StreamNumber: UINT;
                                                         var pData: DXVAHD_STREAM_STATE_PRIVATE_DATA): HResult; stdcall;

  {$EXTERNALSYM PDXVAHDSW_VideoProcessBltHD}
  PDXVAHDSW_VideoProcessBltHD = function(hVideoProcessor: THandle;
                                         pOutputSurface: IDirect3DSurface9;
                                         OutputFrame: UINT;
                                         StreamCount: UINT;
                                         pStreams: DXVAHD_STREAM_DATA): HResult; stdcall;

  {$EXTERNALSYM PDXVAHDSW_DestroyVideoProcessor}
  PDXVAHDSW_DestroyVideoProcessor = function(hVideoProcessor: THandle): HResult; stdcall;


  // Callback record
type
  PDXVAHDSW_CALLBACKS = ^DXVAHDSW_CALLBACKS;
  {$EXTERNALSYM DXVAHDSW_CALLBACKS}
  DXVAHDSW_CALLBACKS = record
    CreateDevice: PDXVAHDSW_CreateDevice;
    ProposeVideoPrivateFormat: PDXVAHDSW_ProposeVideoPrivateFormat;
    GetVideoProcessorDeviceCaps: PDXVAHDSW_GetVideoProcessorDeviceCaps;
    GetVideoProcessorOutputFormats: PDXVAHDSW_GetVideoProcessorOutputFormats;
    GetVideoProcessorInputFormats: PDXVAHDSW_GetVideoProcessorInputFormats;
    GetVideoProcessorCaps: PDXVAHDSW_GetVideoProcessorCaps;
    GetVideoProcessorCustomRates: PDXVAHDSW_GetVideoProcessorCustomRates;
    GetVideoProcessorFilterRange: PDXVAHDSW_GetVideoProcessorFilterRange;
    DestroyDevice: PDXVAHDSW_DestroyDevice;
    CreateVideoProcessor: PDXVAHDSW_CreateVideoProcessor;
    SetVideoProcessBltState: PDXVAHDSW_SetVideoProcessBltState;
    GetVideoProcessBltStatePrivate: PDXVAHDSW_GetVideoProcessBltStatePrivate;
    SetVideoProcessStreamState: PDXVAHDSW_SetVideoProcessStreamState;
    GetVideoProcessStreamStatePrivate: PDXVAHDSW_GetVideoProcessStreamStatePrivate;
    VideoProcessBltHD: PDXVAHDSW_VideoProcessBltHD;
    DestroyVideoProcessor: PDXVAHDSW_DestroyVideoProcessor;
  end;


  //
  {$EXTERNALSYM PDXVAHDSW_Plugin}
  function PDXVAHDSW_Plugin(Size: UINT;
                            out pCallbacks: Pointer): HResult; stdcall;


type

  PDXVAHDETW_CREATEVIDEOPROCESSOR = ^_DXVAHDETW_CREATEVIDEOPROCESSOR;
  {$EXTERNALSYM _DXVAHDETW_CREATEVIDEOPROCESSOR}
  _DXVAHDETW_CREATEVIDEOPROCESSOR = record
    pObject: ULONGLONG;
    pD3D9Ex: ULONGLONG;
    VPGuid: TGUID;
  end;
  {$EXTERNALSYM DXVAHDETW_CREATEVIDEOPROCESSOR}
  DXVAHDETW_CREATEVIDEOPROCESSOR = _DXVAHDETW_CREATEVIDEOPROCESSOR;

  PDXVAHDETW_VIDEOPROCESSBLTSTATE = ^_DXVAHDETW_VIDEOPROCESSBLTSTATE;
  {$EXTERNALSYM _DXVAHDETW_VIDEOPROCESSBLTSTATE}
  _DXVAHDETW_VIDEOPROCESSBLTSTATE = record
    pObject: ULONGLONG;
    State: DXVAHD_BLT_STATE;
    DataSize: UINT;
    SetState: BOOL;
  end;
  {$EXTERNALSYM DXVAHDETW_VIDEOPROCESSBLTSTATE}
  DXVAHDETW_VIDEOPROCESSBLTSTATE = _DXVAHDETW_VIDEOPROCESSBLTSTATE;

  PDXVAHDETW_VIDEOPROCESSSTREAMSTATE = ^_DXVAHDETW_VIDEOPROCESSSTREAMSTATE;
  {$EXTERNALSYM _DXVAHDETW_VIDEOPROCESSSTREAMSTATE}
  _DXVAHDETW_VIDEOPROCESSSTREAMSTATE = record
    pObject: ULONGLONG;
    StreamNumber: UINT;
    State: DXVAHD_STREAM_STATE;
    DataSize: UINT;
    SetState: BOOL;
  end;
  {$EXTERNALSYM DXVAHDETW_VIDEOPROCESSSTREAMSTATE}
  DXVAHDETW_VIDEOPROCESSSTREAMSTATE = _DXVAHDETW_VIDEOPROCESSSTREAMSTATE;

  PDXVAHDETW_VIDEOPROCESSBLTHD = ^_DXVAHDETW_VIDEOPROCESSBLTHD;
  {$EXTERNALSYM _DXVAHDETW_VIDEOPROCESSBLTHD}
  _DXVAHDETW_VIDEOPROCESSBLTHD = record
    pObject: ULONGLONG;
    pOutputSurface: ULONGLONG;
    TargetRect: TRect;
    OutputFormat: D3DFORMAT;
    ColorSpace: UINT;
    OutputFrame: UINT;
    StreamCount: UINT;
    Enter: BOOL;
  end;
  {$EXTERNALSYM DXVAHDETW_VIDEOPROCESSBLTHD}
  DXVAHDETW_VIDEOPROCESSBLTHD = _DXVAHDETW_VIDEOPROCESSBLTHD;

  PDXVAHDETW_VIDEOPROCESSBLTHD_STREAM = ^_DXVAHDETW_VIDEOPROCESSBLTHD_STREAM;
  {$EXTERNALSYM _DXVAHDETW_VIDEOPROCESSBLTHD_STREAM}
  _DXVAHDETW_VIDEOPROCESSBLTHD_STREAM = record
    pObject: ULONGLONG;
    pInputSurface: ULONGLONG;
    SourceRect: TRect;
    DestinationRect: TRect;
    InputFormat: D3DFORMAT;
    FrameFormat: DXVAHD_FRAME_FORMAT;
    ColorSpace: UINT;
    StreamNumber: UINT;
    OutputIndex: UINT;
    InputFrameOrField: UINT;
    PastFrames: UINT;
    FutureFrames: UINT;
  end;
  {$EXTERNALSYM DXVAHDETW_VIDEOPROCESSBLTHD_STREAM}
  DXVAHDETW_VIDEOPROCESSBLTHD_STREAM = _DXVAHDETW_VIDEOPROCESSBLTHD_STREAM;

  PDXVAHDETW_DESTROYVIDEOPROCESSOR = ^_DXVAHDETW_DESTROYVIDEOPROCESSOR;
  {$EXTERNALSYM _DXVAHDETW_DESTROYVIDEOPROCESSOR}
  _DXVAHDETW_DESTROYVIDEOPROCESSOR = record
    pObject: ULONGLONG;
  end;
  {$EXTERNALSYM DXVAHDETW_DESTROYVIDEOPROCESSOR}
  DXVAHDETW_DESTROYVIDEOPROCESSOR = _DXVAHDETW_DESTROYVIDEOPROCESSOR;


  // Forward Interfaces Declarations

  PIDXVAHD_Device = ^IDXVAHD_Device;
  IDXVAHD_Device = interface;

  PIDXVAHD_VideoProcessor = ^IDXVAHD_VideoProcessor;
  IDXVAHD_VideoProcessor = interface;

   // INTERFACES ///////////////////////////////////////////////////////////////


  // Interface IDXVAHD_Device
  // ========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDXVAHD_Device);'}
  {$EXTERNALSYM IDXVAHD_Device}
  IDXVAHD_Device = interface(IUnknown)
  ['{95f12dfd-d77e-49be-815f-57d579634d6d}']

    function CreateVideoSurface(Width: UINT; const Height: UINT; const Format: D3DFORMAT;
                                Pool: D3DPOOL; const Usage: DWORD; const tType: DXVAHD_SURFACE_TYPE;
                                NumSurfaces: UINT; out ppSurfaces: IDirect3DSurface9; var pSharedHandle: THandle): HResult; stdcall;

    function GetVideoProcessorDeviceCaps(out pCaps: DXVAHD_VPDEVCAPS): HResult; stdcall;

    function GetVideoProcessorOutputFormats(Count: UINT;
                                            out pFormats: D3DFORMAT): HResult; stdcall;

    function GetVideoProcessorInputFormats(Count: UINT;
                                           out pFormats: D3DFORMAT): HResult; stdcall;

    function GetVideoProcessorCaps(Count: UINT;
                                   out pCaps: DXVAHD_VPCAPS): HResult; stdcall;

    function GetVideoProcessorCustomRates(const pVPGuid: TGUID;
                                          Count: UINT;
                                          out pRates: DXVAHD_CUSTOM_RATE_DATA): HResult; stdcall;

    function GetVideoProcessorFilterRange(Filter: DXVAHD_FILTER;
                                          out pRange: DXVAHD_FILTER_RANGE_DATA): HResult; stdcall;

    function CreateVideoProcessor(const pVPGuid: TGUID;
                                  out ppVideoProcessor: IDXVAHD_VideoProcessor): HResult; stdcall;

  end;
  IID_IDXVAHD_Device = IDXVAHD_Device;
  {$EXTERNALSYM IID_IDXVAHD_Device}



  // Interface IDXVAHD_VideoProcessor
  // ================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDXVAHD_VideoProcessor);'}
  {$EXTERNALSYM IDXVAHD_VideoProcessor}
  IDXVAHD_VideoProcessor = interface(IUnknown)
  ['{95f4edf4-6e03-4cd7-be1b-3075d665aa52}']

    function SetVideoProcessBltState(State: DXVAHD_BLT_STATE;
                                     const DataSize: UINT;
                                     pData: Pointer): HResult; stdcall;

    function GetVideoProcessBltState(State: DXVAHD_BLT_STATE;
                                     DataSize: UINT;
                                     out pData: Pointer): HResult; stdcall;

    function SetVideoProcessStreamState(StreamNumber: UINT;
                                        State: DXVAHD_STREAM_STATE;
                                        DataSize: UINT;
                                        pData: Pointer): HResult; stdcall;

    function GetVideoProcessStreamState(StreamNumber: UINT;
                                        State: DXVAHD_STREAM_STATE;
                                        DataSize: UINT;
                                        out pData: Pointer): HResult; stdcall;

    function VideoProcessBltHD(pOutputSurface: IDirect3DSurface9;
                               OutputFrame: UINT;
                               StreamCount: UINT;
                               pStreams: DXVAHD_STREAM_DATA): HResult; stdcall;

  end;
  IID_IDXVAHD_VideoProcessor = IDXVAHD_VideoProcessor;
  {$EXTERNALSYM IID_IDXVAHD_VideoProcessor}


  {$EXTERNALSYM DXVAHD_CreateDevice}
  function DXVAHD_CreateDevice(pD3DDevice: IDirect3DDevice9Ex;
                               pContentDesc: DXVAHD_CONTENT_DESC;
                               Usage: DXVAHD_DEVICE_USAGE;
                               pPlugin: DXVAHDSW_PluginCallbacks;
                               out ppDevice: IDXVAHD_Device): HResult; stdcall;

  {$EXTERNALSYM PDXVAHD_CreateDevice}
  function PDXVAHD_CreateDevice(pD3DDevice: IDirect3DDevice9Ex;
                                pContentDesc: DXVAHD_CONTENT_DESC;
                                Usage: DXVAHD_DEVICE_USAGE;
                                pPlugin: DXVAHDSW_PluginCallbacks;
                                out ppDevice: IDXVAHD_Device): HResult; stdcall;


  //Additional Prototypes for ALL interfaces

  //end of Additional Prototypes

implementation

  // Implement Additional Prototypes here.

  // DXVAHDSW_CALLBACKS = _DXVAHDSW_CALLBACKS;

{$WARN SYMBOL_PLATFORM OFF}

  function DXVAHD_CreateDevice; external DxVaHdLib name 'DXVAHD_CreateDevice' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function PDXVAHD_CreateDevice; external DxVaHdLib name 'PDXVAHD_CreateDevice' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function PDXVAHDSW_Plugin; external DxVaHdLib name 'PDXVAHDSW_Plugin' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};

{$WARN SYMBOL_PLATFORM ON}

///////// _DXVAHD_BLT_STATE_OUTPUT_COLOR_SPACE_DATA ////////////////////////////

{$OPTIMIZATION ON}
{$OVERFLOWCHECKS OFF}

  function _DXVAHD_BLT_STATE_OUTPUT_COLOR_SPACE_DATA.ReadBits(const aIndex: Integer): Integer;
  var
    Offset: Integer;
    NrBits: Integer;
    Mask: Integer;
  begin
    NrBits := aIndex and $FF;
    Offset := aIndex shr 8;
    Mask := ((1 shl NrBits) - 1);
    Result := (Value shr Offset) and Mask;
  end;

  procedure _DXVAHD_BLT_STATE_OUTPUT_COLOR_SPACE_DATA.WriteBits(const aIndex: Integer; const aValue: Integer);
  var
    Offset: Integer;
    NrBits: Integer;
    Mask: Integer;
  begin
    NrBits := aIndex and $FF;
    Offset := aIndex shr 8;
    Mask := ((1 shl NrBits) - 1);
    Assert(aValue <= Mask);
    Value := (Value and (not (UINT(Mask) shl UINT(Offset)))) or (UINT(aValue) shl UINT(Offset));
  end;
// END _DXVAHD_BLT_STATE_OUTPUT_COLOR_SPACE_DATA ///////////////////////////////

///////// _DXVAHD_STREAM_STATE_INPUT_COLOR_SPACE_DATA //////////////////////////
  function _DXVAHD_STREAM_STATE_INPUT_COLOR_SPACE_DATA.ReadBits(const aIndex: Integer): Integer;
  var
    Offset: Integer;
    NrBits: Integer;
    Mask: Integer;
  begin
    NrBits := aIndex and $FF;
    Offset := aIndex shr 8;
    Mask := ((1 shl NrBits) - 1);
    Result := (Value shr Offset) and Mask;
  end;

  procedure _DXVAHD_STREAM_STATE_INPUT_COLOR_SPACE_DATA.WriteBits(const aIndex: Integer; const aValue: Integer);
  var
    Offset: Integer;
    NrBits: Integer;
    Mask: Integer;
  begin
    NrBits := aIndex and $FF;
    Offset := aIndex shr 8;
    Mask := ((1 shl NrBits) - 1);
    Assert(aValue <= Mask);
    Value := (Value and (not (UINT(Mask) shl UINT(Offset)))) or (UINT(aValue) shl UINT(Offset));
  end;


end.
