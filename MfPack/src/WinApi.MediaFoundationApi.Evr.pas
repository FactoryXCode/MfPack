// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - MediaFoundation
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.MediaFoundationApi.Evr.pas
// Kind: Pascal / Delphi unit
// Release date: 07-07-2012
// Language: ENU
//
// Revision Version: 3.0.0
// Description: The enhanced video renderer (EVR) is a component that displays video on the user's
//              monitor.
//              Two versions of the EVR exist:
//                - The EVR media sink, for Media Foundation applications.
//                - The EVR filter, for DirectShow applications.
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX),
//                 Peter Larson (ozships),
//                 (TopPlay),
//                 Eric.C.Fortier,
//                 Ramyses De Macedo Rodrigues
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
//         Delphi : The IUnknown entries of functions should be casted like this:
//         IUnknown(Pointer), IUnknown(Object), IUnknown(Nil) etc.
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
// Source: evr.h
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
unit WinApi.MediaFoundationApi.Evr;

  {$HPPEMIT '#include "evr.h"'}

interface

uses
  {WinApi}
  WinApi.Windows,
  WinApi.WinApiTypes,
  WinApi.MediaObj,
  WinApi.StrMif,
  {ActiveX}
  WinApi.ActiveX.PropIdl,
  {System}
  System.Types,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfIdl,
  WinApi.MediaFoundationApi.MfTransform,
  WinApi.MediaFoundationApi.MfObjects;

  {$WEAKPACKAGEUNIT ON}
  {$MINENUMSIZE 4}

  {$IFDEF WIN32}
    {$ALIGN 1}
  {$ELSE}
    {$ALIGN 8} // Win64
  {$ENDIF}

  {$I 'WinApiTypes.inc'}
  {$WARN BOUNDS_ERROR OFF}

  // MAKEFOURCC definition
  //function MAKEFOURCC(ch0, ch1, ch2, ch3): DWord;

type
  PPD3dformat = ^D3DFORMAT;
  _D3DFORMAT = Dword;
  {$EXTERNALSYM _D3DFORMAT}
  D3DFORMAT = _D3DFORMAT;
  {$EXTERNALSYM D3DFORMAT}
const
  D3DFMT_UNKNOWN      = D3DFORMAT(0);
  {$EXTERNALSYM D3DFMT_UNKNOWN}
  D3DFMT_R8G8B8       = D3DFORMAT(20);
  {$EXTERNALSYM D3DFMT_R8G8B8}
  D3DFMT_A8R8G8B8     = D3DFORMAT(21);
  {$EXTERNALSYM D3DFMT_A8R8G8B8}
  D3DFMT_X8R8G8B8     = D3DFORMAT(22);
  {$EXTERNALSYM D3DFMT_X8R8G8B8}
  D3DFMT_R5G6B5       = D3DFORMAT(23);
  {$EXTERNALSYM D3DFMT_R5G6B5}
  D3DFMT_X1R5G5B5     = D3DFORMAT(24);
  {$EXTERNALSYM D3DFMT_X1R5G5B5}
  D3DFMT_A1R5G5B5     = D3DFORMAT(25);
  {$EXTERNALSYM D3DFMT_A1R5G5B5}
  D3DFMT_A4R4G4B4     = D3DFORMAT(26);
  {$EXTERNALSYM D3DFMT_A4R4G4B4}
  D3DFMT_R3G3B2       = D3DFORMAT(27);
  {$EXTERNALSYM D3DFMT_R3G3B2}
  D3DFMT_A8           = D3DFORMAT(28);
  {$EXTERNALSYM D3DFMT_A8}
  D3DFMT_A8R3G3B2     = D3DFORMAT(29);
  {$EXTERNALSYM D3DFMT_A8R3G3B2}
  D3DFMT_X4R4G4B4     = D3DFORMAT(30);
  {$EXTERNALSYM D3DFMT_X4R4G4B4}
  D3DFMT_A2B10G10R10  = D3DFORMAT(31);
  {$EXTERNALSYM D3DFMT_A2B10G10R10}
  D3DFMT_G16R16       = D3DFORMAT(34);
  {$EXTERNALSYM D3DFMT_G16R16}
  D3DFMT_A8P8         = D3DFORMAT(40);
  {$EXTERNALSYM D3DFMT_A8P8}
  D3DFMT_P8           = D3DFORMAT(41);
  {$EXTERNALSYM D3DFMT_P8}
  D3DFMT_L8           = D3DFORMAT(50);
  {$EXTERNALSYM D3DFMT_L8}
  D3DFMT_A8L8         = D3DFORMAT(51);
  {$EXTERNALSYM D3DFMT_A8L8}
  D3DFMT_A4L4         = D3DFORMAT(52);
  {$EXTERNALSYM D3DFMT_A4L4}
  D3DFMT_V8U8         = D3DFORMAT(60);
  {$EXTERNALSYM D3DFMT_V8U8}
  D3DFMT_L6V5U5       = D3DFORMAT(61);
  {$EXTERNALSYM D3DFMT_L6V5U5}
  D3DFMT_X8L8V8U8     = D3DFORMAT(62);
  {$EXTERNALSYM D3DFMT_X8L8V8U8}
  D3DFMT_Q8W8V8U8     = D3DFORMAT(63);
  {$EXTERNALSYM D3DFMT_Q8W8V8U8}
  D3DFMT_V16U16       = D3DFORMAT(64);
  {$EXTERNALSYM D3DFMT_V16U16}
  D3DFMT_W11V11U10    = D3DFORMAT(65);
  {$EXTERNALSYM D3DFMT_W11V11U10}
  D3DFMT_A2W10V10U10  = D3DFORMAT(67);
  {$EXTERNALSYM D3DFMT_A2W10V10U10}
  D3DFMT_D16_LOCKABLE = D3DFORMAT(70);
  {$EXTERNALSYM D3DFMT_D16_LOCKABLE}
  D3DFMT_D32          = D3DFORMAT(71);
  {$EXTERNALSYM D3DFMT_D32}
  D3DFMT_D15S1        = D3DFORMAT(73);
  {$EXTERNALSYM D3DFMT_D15S1}
  D3DFMT_D24S8        = D3DFORMAT(75);
  {$EXTERNALSYM D3DFMT_D24S8}
  D3DFMT_D24X8        = D3DFORMAT(77);
  {$EXTERNALSYM D3DFMT_D24X8}
  D3DFMT_D24X4S4      = D3DFORMAT(79);
  {$EXTERNALSYM D3DFMT_D24X4S4}
  D3DFMT_D16          = D3DFORMAT(80);
  {$EXTERNALSYM D3DFMT_D16}
  D3DFMT_VERTEXDATA   = D3DFORMAT(100);
  {$EXTERNALSYM D3DFMT_VERTEXDATA}
  D3DFMT_INDEX16      = D3DFORMAT(101);
  {$EXTERNALSYM D3DFMT_INDEX16}
  D3DFMT_INDEX32      = D3DFORMAT(102);
  {$EXTERNALSYM D3DFMT_INDEX32}
  //D3DFMT_FORCE_DWORD  = FORCEDWORD;

const
  MR_VIDEO_RENDER_SERVICE           : TGuid =  '{1092a86c-ab1a-459a-a336-831fbc4d11ff}';
  {$EXTERNALSYM MR_VIDEO_RENDER_SERVICE}
  MR_VIDEO_MIXER_SERVICE            : TGuid =  '{073cd2fc-6cf4-40b7-8859-e89552c841f8}';
  {$EXTERNALSYM MR_VIDEO_MIXER_SERVICE}
  MR_VIDEO_ACCELERATION_SERVICE     : TGuid =  '{efef5175-5c7d-4ce2-bbbd-34ff8bca6554}';
  {$EXTERNALSYM MR_VIDEO_ACCELERATION_SERVICE}
  MR_BUFFER_SERVICE                 : TGuid =  '{a562248c-9ac6-4ffc-9fba-3af8f8ad1a4d}';
  {$EXTERNALSYM MR_BUFFER_SERVICE}
  VIDEO_ZOOM_RECT                   : TGuid =  '{7aaa1638-1b7f-4c93-bd89-5b9c9fb6fcf0}';
  {$EXTERNALSYM VIDEO_ZOOM_RECT}


type
  PMFVideoAspectRatioMode = ^MFVideoAspectRatioMode;
  MFVideoAspectRatioMode = DWord;
  {$EXTERNALSYM MFVideoAspectRatioMode}
const
  MFVideoARMode_None             = MFVideoAspectRatioMode($0);
  {$EXTERNALSYM MFVideoARMode_None}
  MFVideoARMode_PreservePicture  = MFVideoAspectRatioMode($1);
  {$EXTERNALSYM MFVideoARMode_PreservePicture}
  MFVideoARMode_PreservePixel    = MFVideoAspectRatioMode($2);
  {$EXTERNALSYM MFVideoARMode_PreservePixel}
  MFVideoARMode_NonLinearStretch = MFVideoAspectRatioMode($4);
  {$EXTERNALSYM MFVideoARMode_NonLinearStretch}
  MFVideoARMode_Mask             = MFVideoAspectRatioMode($7);
  {$EXTERNALSYM MFVideoARMode_Mask}

type
  PMFVideoRenderPrefs = ^MFVideoRenderPrefs;
  MFVideoRenderPrefs = DWord;
  {$EXTERNALSYM MFVideoRenderPrefs}
const
  MFVideoRenderPrefs_DoNotRenderBorder     = MFVideoRenderPrefs($1);
  {$EXTERNALSYM MFVideoRenderPrefs_DoNotRenderBorder}
  MFVideoRenderPrefs_DoNotClipToDevice     = MFVideoRenderPrefs($2);
  {$EXTERNALSYM MFVideoRenderPrefs_DoNotClipToDevice}
  MFVideoRenderPrefs_AllowOutputThrottling = MFVideoRenderPrefs($4);
  {$EXTERNALSYM MFVideoRenderPrefs_AllowOutputThrottling}
  MFVideoRenderPrefs_ForceOutputThrottling = MFVideoRenderPrefs($8);
  {$EXTERNALSYM MFVideoRenderPrefs_ForceOutputThrottling}
  MFVideoRenderPrefs_ForceBatching         = MFVideoRenderPrefs($10);
  {$EXTERNALSYM MFVideoRenderPrefs_ForceBatching}
  MFVideoRenderPrefs_AllowBatching         = MFVideoRenderPrefs($20);
  {$EXTERNALSYM MFVideoRenderPrefs_AllowBatching}
  MFVideoRenderPrefs_ForceScaling          = MFVideoRenderPrefs($40);
  {$EXTERNALSYM MFVideoRenderPrefs_ForceScaling}
  MFVideoRenderPrefs_AllowScaling          = MFVideoRenderPrefs($80);
  {$EXTERNALSYM MFVideoRenderPrefs_AllowScaling}
  MFVideoRenderPrefs_DoNotRepaintOnStop    = MFVideoRenderPrefs($100);
  {$EXTERNALSYM MFVideoRenderPrefs_DoNotRepaintOnStop}
  MFVideoRenderPrefs_Mask                  = MFVideoRenderPrefs($1FF);
  {$EXTERNALSYM MFVideoRenderPrefs_Mask}


type
  // MFVideoNormalizedRect
  //======================
  // Defines a normalized rectangle, which is used to specify sub-rectangles in a video rectangle.
  // When a rectangle N is normalized relative to some other rectangle R, it means the following:

  //   The coordinate (0.0, 0.0) on N is mapped to the upper-left corner of R.

  //   The coordinate (1.0, 1.0) on N is mapped to the lower-right corner of R.

  // Any coordinates of N that fall outside the range [0...1] are mapped to positions outside the rectangle R.
  // A normalized rectangle can be used to specify a region within a video rectangle without knowing the resolution or
  // even the aspect ratio of the video.
  // For example, the upper-left quadrant is defined as {0.0, 0.0, 0.5, 0.5}.
  //
  // Note: in Delphi this type is defined as TRectF.
  //       This struct is also defined in MfPack.MfpTypes

  // PMFVideoNormalizedRect = ^MFVideoNormalizedRect;
  // MFVideoNormalizedRect = record
  //   left: FLOAT;
  //   top: FLOAT;
  //   right: FLOAT;
  //   bottom: FLOAT;
  // end;

{$IFNDEF MFVideoNormalizedRect}
  PMFVideoNormalizedRect = PRectF;
  MFVideoNormalizedRect = TRectF;
  {$EXTERNALSYM MFVideoNormalizedRect}
{$DEFINE MFVideoNormalizedRect}
{$ENDIF}


  PMfvpMessageType = ^MFVP_MESSAGE_TYPE;
  PMFVP_MESSAGE_TYPE = ^MFVP_MESSAGE_TYPE;
  MFVP_MESSAGE_TYPE                  = (
    MFVP_MESSAGE_FLUSH               = 0,
    MFVP_MESSAGE_INVALIDATEMEDIATYPE = $1,
    MFVP_MESSAGE_PROCESSINPUTNOTIFY  = $2,
    MFVP_MESSAGE_BEGINSTREAMING      = $3,
    MFVP_MESSAGE_ENDSTREAMING        = $4,
    MFVP_MESSAGE_ENDOFSTREAM         = $5,
    MFVP_MESSAGE_STEP                = $6,
    MFVP_MESSAGE_CANCELSTEP          = $7
  );
  {$EXTERNALSYM MFVP_MESSAGE_TYPE}

type
  PMFVideoMixPrefs = ^MFVideoMixPrefs;
  _MFVideoMixPrefs = DWord;
  {$EXTERNALSYM _MFVideoMixPrefs}
  MFVideoMixPrefs = _MFVideoMixPrefs;
  {$EXTERNALSYM MFVideoMixPrefs}
const
  MFVideoMixPrefs_ForceHalfInterlace       = MFVideoMixPrefs($1);
  {$EXTERNALSYM MFVideoMixPrefs_ForceHalfInterlace}
  MFVideoMixPrefs_AllowDropToHalfInterlace = MFVideoMixPrefs($2);
  {$EXTERNALSYM MFVideoMixPrefs_AllowDropToHalfInterlace}
  MFVideoMixPrefs_AllowDropToBob           = MFVideoMixPrefs($4);
  {$EXTERNALSYM MFVideoMixPrefs_AllowDropToBob}
  MFVideoMixPrefs_ForceBob                 = MFVideoMixPrefs($8);
  {$EXTERNALSYM MFVideoMixPrefs_ForceBob}
  MFVideoMixPrefs_Mask                     = MFVideoMixPrefs($F);
  {$EXTERNALSYM MFVideoMixPrefs_Mask}

type
  PEVRFilterConfigPrefs = ^EVRFilterConfigPrefs;
  _EVRFilterConfig_Prefs = DWord;
  {$EXTERNALSYM _EVRFilterConfig_Prefs}
  EVRFilterConfigPrefs = _EVRFilterConfig_Prefs;
  {$EXTERNALSYM EVRFilterConfigPrefs}
const
  EVRFilterConfigPrefs_EnableQoS = EVRFilterConfigPrefs($1);
  {$EXTERNALSYM EVRFilterConfigPrefs_EnableQoS}
  EVRFilterConfigPrefs_Mask      = EVRFilterConfigPrefs($1);
  {$EXTERNALSYM EVRFilterConfigPrefs_Mask}


type
  PMfServiceLookupType = ^MF_SERVICE_LOOKUP_TYPE;
  PMF_SERVICE_LOOKUP_TYPE = ^MF_SERVICE_LOOKUP_TYPE;
  _MF_SERVICE_LOOKUP_TYPE               = (
    MF_SERVICE_LOOKUP_UPSTREAM          = 0,
    MF_SERVICE_LOOKUP_UPSTREAM_DIRECT   = (MF_SERVICE_LOOKUP_UPSTREAM + 1),
    MF_SERVICE_LOOKUP_DOWNSTREAM        = (MF_SERVICE_LOOKUP_UPSTREAM_DIRECT + 1),
    MF_SERVICE_LOOKUP_DOWNSTREAM_DIRECT = (MF_SERVICE_LOOKUP_DOWNSTREAM + 1),
    MF_SERVICE_LOOKUP_ALL               = (MF_SERVICE_LOOKUP_DOWNSTREAM_DIRECT + 1),
    MF_SERVICE_LOOKUP_GLOBAL            = (MF_SERVICE_LOOKUP_ALL + 1)
  );
  {$EXTERNALSYM _MF_SERVICE_LOOKUP_TYPE}
  MF_SERVICE_LOOKUP_TYPE = _MF_SERVICE_LOOKUP_TYPE;
  {$EXTERNALSYM MF_SERVICE_LOOKUP_TYPE}


type
  // INTERFACES  ///////////////////////////////////////////////////////////////


  // Interface IMFVideoPositionMapper
  // ================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFVideoPositionMapper);'}
  {$EXTERNALSYM IMFVideoPositionMapper}
  IMFVideoPositionMapper = interface(IUnknown)
  ['{1F6A9F17-E70B-4e24-8AE4-0B2C3BA7A4AE}']

    function MapOutputCoordinateToInputStream(xOut: Single;
                                              yOut: Single;
                                              dwOutputStreamIndex: DWord;
                                              dwInputStreamIndex: DWord;
                                              out pxIn: Single;
                                              out pyIn: Single): HRESULT; stdcall;
  end;
  IID_IMFVideoPositionMapper = IMFVideoPositionMapper;
  {$EXTERNALSYM IID_IMFVideoPositionMapper}



  // Interface IMFVideoDeviceID
  // ==========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFVideoDeviceID);'}
  {$EXTERNALSYM IMFVideoDeviceID}
  IMFVideoDeviceID = interface(IUnknown)
  ['{A38D9567-5A9C-4f3c-B293-8EB415B279BA}']

    function GetDeviceID(out pDeviceID: IID): HRESULT; stdcall;

  end;
  IID_IMFVideoDeviceID = IMFVideoDeviceID;
  {$EXTERNALSYM IID_IMFVideoDeviceID}



  // Interface IMFVideoDisplayControl
  // ================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFVideoDisplayControl);'}
  {$EXTERNALSYM IMFVideoDisplayControl}
  IMFVideoDisplayControl = interface(IUnknown)
  ['{a490b1e4-ab84-4d31-a1b2-181e03b1077a}']
    function GetNativeVideoSize(var pszVideo: SIZE;
                                var pszARVideo: SIZE): HRESULT; stdcall;

    function GetIdealVideoSize(var pszMin: SIZE;
                               var pszMax: SIZE): HRESULT; stdcall;

    function SetVideoPosition(pnrcSource: PMFVideoNormalizedRect;
                              prcDest: LPRECT): HRESULT; stdcall;

    function GetVideoPosition(out pnrcSource: MFVideoNormalizedRect;
                              out prcDest: TRect): HRESULT; stdcall;

    function SetAspectRatioMode(dwAspectRatioMode: DWORD): HRESULT; stdcall;

    function GetAspectRatioMode(out pdwAspectRatioMode: DWORD): HRESULT; stdcall;

    function SetVideoWindow(const hwndVideo: HWND): HRESULT; stdcall;

    function GetVideoWindow(out phwndVideo: HWND): HRESULT; stdcall;

    function RepaintVideo(): HRESULT; stdcall;

    function GetCurrentImage(var pBih: BITMAPINFOHEADER;
                             out pDib: PByte;
                             out pcbDib: DWORD;
                             var pTimeStamp: MFTIME): HRESULT; stdcall;

    function SetBorderColor(const Clr: COLORREF): HRESULT; stdcall;

    function GetBorderColor(out pClr: COLORREF): HRESULT; stdcall;

    function SetRenderingPrefs(dwRenderFlags: DWORD): HRESULT; stdcall;

    function GetRenderingPrefs(out pdwRenderFlags: DWORD): HRESULT; stdcall;

    // This API is not supported anymore.
    // Note: No longer available since SDK version RedStone 5
    function SetFullscreen(fFullscreen: BOOL): HRESULT; stdcall; deprecated 'This API is not longer available since SDK version RedStone 5';

    // Queries whether the enhanced video renderer (EVR) is currently in full-screen mode.
    // This API is not supported anymore.
    // Note: No longer available since SDK version RedStone 5
    function GetFullscreen(out pfFullscreen: BOOL): HRESULT; stdcall; deprecated 'This API is not longer available since SDK version RedStone 5';

  end;
  IID_IMFVideoDisplayControl = IMFVideoDisplayControl;
  {$EXTERNALSYM IID_IMFVideoDisplayControl}


  // Interface IMFVideoPresenter
  // ===========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFVideoPresenter);'}
  {$EXTERNALSYM IMFVideoPresenter}
  IMFVideoPresenter = interface(IMFClockStateSink)
  ['{29AFF080-182A-4a5d-AF3B-448F3A6346CB}']

    function ProcessMessage(eMessage: MFVP_MESSAGE_TYPE;
                            ulParam: ULONG_PTR): HRESULT; stdcall;

    function GetCurrentMediaType(out ppMediaType: IMFVideoMediaType): HRESULT; stdcall;

  end;
  IID_IMFVideoPresenter = IMFVideoPresenter;
  {$EXTERNALSYM IID_IMFVideoPresenter}


  // Interface IMFDesiredSample
  // ==========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFDesiredSample);'}
  {$EXTERNALSYM IMFDesiredSample}
  IMFDesiredSample = interface(IUnknown)
  ['{56C294D0-753E-4260-8D61-A3D8820B1D54}']

    function GetDesiredSampleTimeAndDuration(out phnsSampleTime: LONGLONG;
                                             out phnsSampleDuration: LONGLONG): HRESULT; stdcall;

    procedure SetDesiredSampleTimeAndDuration(hnsSampleTime: LONGLONG;
                                              hnsSampleDuration: LONGLONG); stdcall;

    procedure Clear(); stdcall;

  end;
  IID_IMFDesiredSample = IMFDesiredSample;
  {$EXTERNALSYM IID_IMFDesiredSample}


  // Interface IMFTrackedSample
  // ==========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFTrackedSample);'}
  {$EXTERNALSYM IMFTrackedSample}
  IMFTrackedSample = interface(IUnknown)
  ['{245BF8E9-0755-40f7-88A5-AE0F18D55E17}']

    function SetAllocator(pSampleAllocator: IMFAsyncCallback;
                          const pUnkState: IUnknown): HRESULT; stdcall;

  end;
  IID_IMFTrackedSample = IMFTrackedSample;
  {$EXTERNALSYM IID_IMFTrackedSample}


  // Interface IMFVideoMixerControl
  // ==============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFVideoMixerControl);'}
  {$EXTERNALSYM IMFVideoMixerControl}
  IMFVideoMixerControl = interface(IUnknown)
  ['{245BF8E9-0755-40f7-88A5-AE0F18D55E17}']

    function SetStreamZOrder(dwStreamID: DWORD;
                             dwZ: DWORD): HRESULT; stdcall;

    function GetStreamZOrder(dwStreamID: DWORD;
                             out pdwZ: PDWORD): HRESULT; stdcall;

    function SetStreamOutputRect(dwStreamID: DWORD;
                                 const pnrcOutput: MFVideoNormalizedRect): HRESULT; stdcall;

    function GetStreamOutputRect(dwStreamID: DWORD;
                                 out pnrcOutput: MFVideoNormalizedRect): HRESULT; stdcall;
  end;
  IID_IMFVideoMixerControl = IMFVideoMixerControl;
  {$EXTERNALSYM IID_IMFVideoMixerControl}


  // Interface IMFVideoMixerControl2
  // ===============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFVideoMixerControl2);'}
  {$EXTERNALSYM IMFVideoMixerControl2}
  IMFVideoMixerControl2 = interface(IMFVideoMixerControl)
  ['{8459616d-966e-4930-b658-54fa7e5a16d3}']

    function SetMixingPrefs(dwMixFlags: DWORD): HRESULT; stdcall;

    function GetMixingPrefs(out pdwMixFlags: DWORD): HRESULT; stdcall;

  end;
  IID_IMFVideoMixerControl2 = IMFVideoMixerControl2;
  {$EXTERNALSYM IID_IMFVideoMixerControl2}


  // Interface IMFVideoRenderer
  // ==========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFVideoRenderer);'}
  {$EXTERNALSYM IMFVideoRenderer}
  IMFVideoRenderer = interface(IUnknown)
  ['{DFDFD197-A9CA-43d8-B341-6AF3503792CD}']

    function InitializeRenderer(pVideoMixer: IMFTransform;
                                pVideoPresenter: IMFVideoPresenter): HRESULT; stdcall;

  end;
  IID_IMFVideoRenderer = IMFVideoRenderer;
  {$EXTERNALSYM IID_IMFVideoRenderer}


  // Interface IEVRFilterConfig
  // ==========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IEVRFilterConfig);'}
  {$EXTERNALSYM IEVRFilterConfig}
  IEVRFilterConfig = interface(IUnknown)
  ['{DFDFD197-A9CA-43d8-B341-6AF3503792CD}']

    function SetNumberOfStreams(dwMaxStreams: DWORD): HRESULT; stdcall;

    function GetNumberOfStreams(out pdwMaxStreams: DWORD): HRESULT; stdcall;

  end;
  IID_IEVRFilterConfig = IEVRFilterConfig;
  {$EXTERNALSYM IID_IEVRFilterConfig}


  // Interface IEVRFilterConfigEx
  // ============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IEVRFilterConfigEx);'}
  {$EXTERNALSYM IEVRFilterConfigEx}
  IEVRFilterConfigEx = interface(IUnknown)
  ['{aea36028-796d-454f-beee-b48071e24304}']

    function SetConfigPrefs(dwConfigFlags: DWORD): HRESULT; stdcall;

    function GetConfigPrefs(out pdwConfigFlags: DWORD): HRESULT; stdcall;

  end;
  IID_IEVRFilterConfigEx = IEVRFilterConfigEx;
  {$EXTERNALSYM IID_IEVRFilterConfigEx}


  // Interface IMFTopologyServiceLookup
  // ==================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFTopologyServiceLookup);'}
  {$EXTERNALSYM IMFTopologyServiceLookup}
  IMFTopologyServiceLookup = interface(IUnknown)
  ['{fa993889-4383-415a-a930-dd472a8cf6f7}']

    function LookupService(aType: MF_SERVICE_LOOKUP_TYPE;
                           dwIndex: DWORD;
                           const guidService: REFGUID;
                           const riid: REFIID;
                           out ppvObjects: PPointer;
                           pnObjects: PDWORD): HRESULT; stdcall;

  end;
  IID_IMFTopologyServiceLookup = IMFTopologyServiceLookup;
  {$EXTERNALSYM IID_IMFTopologyServiceLookup}


  // Interface IMFTopologyServiceLookupClient
  // ========================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFTopologyServiceLookupClient);'}
  {$EXTERNALSYM IMFTopologyServiceLookupClient}
  IMFTopologyServiceLookupClient = interface(IUnknown)
  ['{fa99388a-4383-415a-a930-dd472a8cf6f7}']

    function InitServicePointers(pLookup: IMFTopologyServiceLookup): HRESULT; stdcall;

    function ReleaseServicePointers(): HRESULT; stdcall;

  end;
  IID_IMFTopologyServiceLookupClient = IMFTopologyServiceLookupClient;
  {$EXTERNALSYM IID_IMFTopologyServiceLookupClient}


  // Interface IEVRTrustedVideoPlugin
  // ================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IEVRTrustedVideoPlugin);'}
  {$EXTERNALSYM IEVRTrustedVideoPlugin}
  IEVRTrustedVideoPlugin = interface(IUnknown)
  ['{83A4CE40-7710-494b-A893-A472049AF630}']

    function IsInTrustedVideoMode(out pYes: BOOL): HRESULT; stdcall;

    function CanConstrict(out pYes: BOOL): HRESULT; stdcall;

    function SetConstriction(dwKPix: DWORD): HRESULT; stdcall;

    function DisableImageExport(bDisable: BOOL): HRESULT; stdcall;

  end;
  IID_IEVRTrustedVideoPlugin = IEVRTrustedVideoPlugin;
  {$EXTERNALSYM IID_IEVRTrustedVideoPlugin}


  //function MFCreateVideoPresenter
  //Creates the default video presenter for the enhanced video renderer (EVR).
  //NOTES:
  //pOwner :  Pointer to the owner of the object.
  //          If the object is aggregated, pass a pointer to the aggregating object's IUnknown interface.
  //          Otherwise, set this parameter to nil.
  //riidDevice : Interface identifier (IID) of the video device interface that will be used for processing the video.
  //             Currently the only supported value is IID_IDirect3DDevice9.
  //riid : IID of the requested interface on the video presenter. The video presenter exposes the IMFVideoPresenter interface.
  //ppVideoPresenter : Receives a pointer to the requested interface on the video presenter.
  //                   The caller must release the interface.
  function MFCreateVideoPresenter(pOwner: IUnknown;
                                  const riidDevice: REFIID;
                                  const riid: REFIID;
                                  out ppVideoPresenter: IMFVideoPresenter): HRESULT; stdcall;
  {$EXTERNALSYM MFCreateVideoPresenter}


  //function MFCreateVideoMixer
  //Creates the default video mixer for the enhanced video renderer (EVR).
  //NOTES:
  //pOwner :  Pointer to the owner of this object.
  //          If the object is aggregated, pass a pointer to the aggregating object's IUnknown interface.
  //          Otherwise, set this parameter to Nil ( IUnknown(nil) ).
  //riidDevice :  Interface identifier (IID) of the video device interface that will be used for processing the video.
  //              Currently the only supported value is IID_IDirect3DDevice9.
  //riid : IID of the requested interface on the video mixer. The video mixer exposes the IMFTransform interface.
  //ppVideoMixer : Receives a pointer to the requested interface. The caller must release the interface.
  function MFCreateVideoMixer(pOwner: IUnknown;
                              const riidDevice: REFIID;
                              const riid: REFIID;
                              out ppVideoMixer: IUnknown): HRESULT; stdcall;
  {$EXTERNALSYM MFCreateVideoMixer}


  //function MFCreateVideoMixerAndPresenter
  //Creates the default video mixer and video presenter for the enhanced video renderer (EVR).
  //NOTES:
  //pMixerOwner : Pointer to the owner of the video mixer.
  //              If the mixer is aggregated, pass a pointer to the aggregating object's IUnknown interface.
  //              Otherwise, set this parameter to Nil ( IUnknown(nil) ).
  //pPresenterOwner : Pointer to the owner of the video presenter.
  //                  If the presenter is aggregated, pass a pointer to the aggregating object's IUnknown interface.
  //                  Otherwise, set this parameter to Nil ( IUnknown(nil) ).
  //riidMixer : Interface identifier (IID) of the requested interface on the video mixer. The video mixer exposes the IMFTransform interface.
  //ppvVideoMixer : Receives a pointer to the requested interface on the video mixer. The caller must release the interface.
  //riidPresenter : IID of the requested interface on the video presenter. The video presenter exposes the IMFVideoPresenter interface.
  //ppvVideoPresenter : Receives a pointer to the requested interface on the video presenter. The caller must release the interface.
  function MFCreateVideoMixerAndPresenter(pMixerOwner: IUnknown;
                                          pPresenterOwner: IUnknown;
                                          const riidMixer: REFIID;
                                          out ppvVideoMixer: IUnknown;
                                          const riidPresenter: REFIID;
                                          out ppvVideoPresenter: IUnknown): HRESULT; stdcall;
  {$EXTERNALSYM MFCreateVideoMixerAndPresenter}


  //function MFCreateVideoRenderer
  //Creates an instance of the enhanced video renderer (EVR) media sink.
  //NOTES:
  //riidRenderer : Interface identifier (IID) of the requested interface on the EVR.
  //ppVideoRenderer : Receives a pointer to the requested interface. The caller must release the interface.
  function MFCreateVideoRenderer(const riidRenderer: REFIID;
                                 out ppVideoRenderer: IUnknown): HRESULT; stdcall;
  {$EXTERNALSYM MFCreateVideoRenderer}


  //function MFCreateVideoSampleFromSurface
  //Creates a media sample that manages a Direct3D surface.
  //NOTES:
  //pUnkSurface : A pointer to the IUnknown interface of the Direct3D surface.
  //              This parameter can be Nil ( IUnknown(Nil) ).
  //ppSample : Receives a pointer to the sample's IMFSample interface. The caller must release the interface.
  function MFCreateVideoSampleFromSurface(pUnkSurface: IUnknown;
                                          out ppSample: IMFSample): HRESULT; stdcall;
  {$EXTERNALSYM MFCreateVideoSampleFromSurface}


  // Additional Prototypes for ALL interfaces

  // End of Additional Prototypes

implementation
  
  // Implement Additional functions here.


const
  EvrLib = 'evr.dll';
  MfLib = 'mf.dll';

{$WARN SYMBOL_PLATFORM OFF}
  function MFCreateVideoPresenter;         external EvrLib name 'MFCreateVideoPresenter' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFCreateVideoMixer;             external EvrLib name 'MFCreateVideoMixer' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFCreateVideoMixerAndPresenter; external EvrLib name 'MFCreateVideoMixerAndPresenter' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFCreateVideoRenderer;          external MfLib name 'MFCreateVideoRenderer' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFCreateVideoSampleFromSurface; external EvrLib name 'MFCreateVideoSampleFromSurface' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
{$WARN SYMBOL_PLATFORM ON}

end.
