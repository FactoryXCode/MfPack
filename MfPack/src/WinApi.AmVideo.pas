// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - Shared
// Project location: http://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.AMVideo.pas
// Kind: Pascal / Delphi unit
// Release date: 05-10-2015
// Language: ENU
//
// Revision Version: 3.0.0
// Description: Video related definitions and interfaces for ActiveMovie. (DirectX Include unit).
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
// Remarks: This unit is also included in Winapi/DsPack.DirectShow9.pas
//          If you need DirectShow, exclude this file from your project.
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
// Source: AMVideo.h
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
unit WinApi.AmVideo;

  {$HPPEMIT '#include "AMVideo.h"'}

interface

uses

  {WinApi}
  WinApi.Windows,
  WinApi.ActiveX,
  {DirectX (Clootie) or WinApi}
  WinApi.DirectDraw,
  {System}
  System.SysUtils;

  {$WEAKPACKAGEUNIT ON}
  {$MINENUMSIZE 4}

  {$IFDEF WIN32}
    {$ALIGN 1}
  {$ELSE}
    {$ALIGN 8} // Win64
  {$ENDIF}



// DirectShow definition.
// DirectShow will be slowly phased out.

{$IFNDEF REFERENCE_TIME}
type
  PREFERENCE_TIME = ^REFERENCE_TIME;
  REFERENCE_TIME = LONGLONG;
  {$EXTERNALSYM REFERENCE_TIME}
  {$DEFINE REFERENCE_TIME}
{$ENDIF}

{$IFNDEF OLECHAR}
type
  POLECHAR = ^OLECHAR;
  {$IFDEF UNICODE}
  OLECHAR = WideChar;
  {$EXTERNALSYM OLECHAR}
  {$ELSE}
  OLECHAR = AnsiChar;
  {$EXTERNALSYM OLECHAR}
  {$ENDIF}
{$ENDIF}

{$IFNDEF BSTR}
  LPBSTR = ^BSTR;
  {$EXTERNALSYM LPBSTR}
  BSTR = ^OLECHAR;
  {$EXTERNALSYM BSTR}
  PBSTR = ^BSTR;
  {$EXTERNALSYM PBSTR}
{$ENDIF}



const

  AMDDS_NONE    = $00;        // No use for DCI/DirectDraw
  {$EXTERNALSYM AMDDS_NONE}
  AMDDS_DCIPS   = $01;        // Use DCI primary surface
  {$EXTERNALSYM AMDDS_DCIPS}
  AMDDS_PS      = $02;        // Use DirectDraw primary
  {$EXTERNALSYM AMDDS_PS}
  AMDDS_RGBOVR  = $04;        // RGB overlay surfaces
  {$EXTERNALSYM AMDDS_RGBOVR}
  AMDDS_YUVOVR  = $08;        // YUV overlay surfaces
  {$EXTERNALSYM AMDDS_YUVOVR}
  AMDDS_RGBOFF  = $10;        // RGB offscreen surfaces
  {$EXTERNALSYM AMDDS_RGBOFF}
  AMDDS_YUVOFF  = $20;        // YUV offscreen surfaces
  {$EXTERNALSYM AMDDS_YUVOFF}
  AMDDS_RGBFLP  = $40;        // RGB flipping surfaces
  {$EXTERNALSYM AMDDS_RGBFLP}
  AMDDS_YUVFLP  = $80;        // YUV flipping surfaces
  {$EXTERNALSYM AMDDS_YUVFLP}
  AMDDS_ALL     = $FF;        // ALL the previous flags
  {$EXTERNALSYM AMDDS_ALL}
  AMDDS_DEFAULT = AMDDS_ALL;   // Use all available surfaces
  {$EXTERNALSYM AMDDS_DEFAULT}

  AMDDS_YUV = AMDDS_YUVOFF or AMDDS_YUVOVR or AMDDS_YUVFLP;
  {$EXTERNALSYM AMDDS_YUV}
  AMDDS_RGB = AMDDS_RGBOFF or AMDDS_RGBOVR or AMDDS_RGBFLP;
  {$EXTERNALSYM AMDDS_RGB}
  AMDDS_PRIMARY = AMDDS_DCIPS or AMDDS_PS;
  {$EXTERNALSYM AMDDS_PRIMARY}


type


  // This is an interface on the video renderer that provides information about
  // DirectDraw with respect to its use by the renderer. For example it allows
  // an application to get details of the surface and any hardware capabilities
  // that are available. It also allows someone to adjust the surfaces that the
  // renderer should use and furthermore even set the DirectDraw instance. We
  // allow someone to set the DirectDraw instance because DirectDraw can only
  // be opened once per process so it helps resolve conflicts. There is some
  // duplication in this interface as the hardware/emulated/FOURCCs available
  // can all be found through the IDirectDraw interface, this interface allows
  // simple access to that information without calling the DirectDraw provider
  // itself. The AMDDS prefix is ActiveMovie DirectDraw Switches abbreviated.

  // Interface IDirectDrawVideo
  // ==========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDirectDrawVideo);'}
  {$EXTERNALSYM IDirectDrawVideo}
  IDirectDrawVideo = interface(IUnknown)
    ['{36D39EB0-DD75-11CE-BF0E-00AA0055595A}']
    // IUnknown methods
    // See: IUnknown interface functions, those are inherited by this interface,
    // like any other interface that is inherited from IUnknown.

    // IDirectDrawVideo methods
    function GetSwitches(out pSwitches: DWORD): HResult; stdcall;

    function SetSwitches(pSwitches: DWORD): HResult; stdcall;

    function GetCaps(out pCaps: TDDCaps): HResult; stdcall;

    function GetEmulatedCaps(out pCaps: TDDCaps): HResult; stdcall;

    function GetSurfaceDesc(out pSurfaceDesc: TDDSurfaceDesc): HResult; stdcall;

    function GetFourCCCodes(out pCount, pCodes: DWORD): HResult; stdcall;

    function SetDirectDraw(pDirectDraw: IDirectDraw): HResult; stdcall;

    function GetDirectDraw(out ppDirectDraw: IDirectDraw): HResult; stdcall;

    function GetSurfaceType(out pSurfaceType: DWORD): HResult; stdcall;

    function SetDefault(): HResult; stdcall;

    function UseScanLine(UseScanLine: LongBool): HResult; stdcall;

    function CanUseScanLine(var UseScanLine: LongBool): HResult; stdcall;

    function UseOverlayStretch(UseOverlayStretch: LongBool): HResult; stdcall;

    function CanUseOverlayStretch(var UseOverlayStretch: LongBool): HResult; stdcall;

    function UseWhenFullScreen(UseWhenFullScreen: LongBool): HResult; stdcall;

    function WillUseFullScreen(var UseWhenFullScreen: LongBool): HResult; stdcall;

  end;
  IID_IDirectDrawVideo = IDirectDrawVideo;
  {$EXTERNALSYM IID_IDirectDrawVideo}


  // Interface IQualProp
  // ===================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IQualProp);'}
  {$EXTERNALSYM IQualProp}
  IQualProp = interface(IUnknown)
    ['{1BD0ECB0-F8E2-11CE-AAC6-0020AF0B99A3}']
    (*** IQualProp methods ***)
    // Compare these with the functions in class CGargle in gargle.h
    function get_FramesDroppedInRenderer(out pcFrames: Integer): HResult; stdcall;

    function get_FramesDrawn(out pcFrames: Integer): HResult; stdcall;

    function get_AvgFrameRate(out piAvgFrameRate: Integer): HResult; stdcall;

    function get_Jitter(out iJitter: Integer): HResult; stdcall;

    function get_AvgSyncOffset(out piAvg: Integer): HResult; stdcall;

    function get_DevSyncOffset(out piDev: Integer): HResult; stdcall;

  end;
  IID_IQualProp = IQualProp;
  {$EXTERNALSYM IID_IQualProp}


  // Interface IFullScreenVideo
  // ==========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IFullScreenVideo);'}
  {$EXTERNALSYM IFullScreenVideo}
  IFullScreenVideo = interface(IUnknown)
    ['{DD1D7110-7836-11CF-BF47-00AA0055595A}']
    (*** IFullScreenVideo methods ***)
    function CountModes(out pModes: Longint): HResult; stdcall;

    function GetModeInfo(Mode: Longint;
                         out pWidth: Longint;
                         pHeight: Longint;
                         pDepth: Longint): HResult; stdcall;

    function GetCurrentMode(out pMode: Longint): HResult; stdcall;

    function IsModeAvailable(Mode: Longint): HResult; stdcall;

    function IsModeEnabled(Mode: Longint): HResult; stdcall;

    function SetEnabled(Mode: Longint;
                        bEnabled: Longint): HResult; stdcall;

    function GetClipFactor(out pClipFactor: Longint): HResult; stdcall;

    function SetClipFactor(ClipFactor: Longint): HResult; stdcall;

    function SetMessageDrain(hwnd: HWND): HResult; stdcall;

    function GetMessageDrain(out hwnd: HWND): HResult; stdcall;

    function SetMonitor(Monitor: Longint): HResult; stdcall;

    function GetMonitor(out Monitor: Longint): HResult; stdcall;

    function HideOnDeactivate(Hide: LongBool): HResult; stdcall;

    function IsHideOnDeactivate: HResult; stdcall;

    function SetCaption(const strCaption: OLECHAR): HResult; stdcall;

    function GetCaption(out pstrCaption: BSTR): HResult; stdcall;

    function SetDefault(): HResult; stdcall;

  end;
  IID_IFullScreenVideo = IFullScreenVideo;
  {$EXTERNALSYM IID_IFullScreenVideo}
  // This interface allows an application or plug-in distributor to control a
  // full screen renderer. The Modex renderer supports this interface. When
  // connected a renderer should load the display modes it has available
  // The number of modes available can be obtained through CountModes. Then
  // information on each individual mode is available by calling GetModeInfo
  // and IsModeAvailable. An application may enable and disable any modes
  // by calling the SetEnabled flag with OATRUE or OAFALSE (not C/C++ TRUE
  // and FALSE values) - the current value may be queried by IsModeEnabled

  // A more generic way of setting the modes enabled that is easier to use
  // when writing applications is the clip loss factor. This defines the
  // amount of video that can be lost when deciding which display mode to
  // use. Assuming the decoder cannot compress the video then playing an
  // MPEG file (say 352x288) into a 320x200 display will lose about 25% of
  // the image. The clip loss factor specifies the upper range permissible.
  // To allow typical MPEG video to be played in 320x200 it defaults to 25%

  // Interface IFullScreenVideoEx
  // ============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IFullScreenVideoEx);'}
  {$EXTERNALSYM IFullScreenVideoEx}
  IFullScreenVideoEx = interface(IFullScreenVideo)
    ['{53479470-F1DD-11CF-BC42-00AA00AC74F6}']

    function SetAcceleratorTable(hwnd: HWND;
                                 hAccel: HACCEL): HResult; stdcall;

    function GetAcceleratorTable(var hwnd: HWND;
                                 var hAccel: HACCEL): HResult; stdcall;

    function KeepPixelAspectRatio(KeepAspect: LongBool): HResult; stdcall;

    function IsKeepPixelAspectRatio(var pKeepAspect: LongBool): HResult; stdcall;

  end;
  IID_IFullScreenVideoEx = IFullScreenVideoEx;
  {$EXTERNALSYM IID_IFullScreenVideoEx}
  // This adds the accelerator table capabilities in fullscreen. This is being
  // added between the original runtime release and the full SDK release. We
  // cannot just add the method to IFullScreenVideo as we don't want to force
  // applications to have to ship the ActiveMovie support DLLs - this is very
  // important to applications that plan on being downloaded over the Internet


  // Interface IBaseVideoMixer
  // =========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IBaseVideoMixer);'}
  {$EXTERNALSYM IBaseVideoMixer}
  IBaseVideoMixer = interface(IUnknown)
    ['{61DED640-E912-11CE-A099-00AA00479A58}']
    (*** IBaseVideoMixer methods ***)
    function SetLeadPin(iPin: Integer): HResult; stdcall;

    function GetLeadPin(out iPin: Integer): HResult; stdcall;

    function GetInputPinCount(out piPinCount: Integer): HResult; stdcall;

    function IsUsingClock(out pbValue: Integer): HResult; stdcall;

    function SetUsingClock(bValue: Integer): HResult; stdcall;

    function GetClockPeriod(out pbValue: Integer): HResult; stdcall;

    function SetClockPeriod(bValue: Integer): HResult; stdcall;

  end;
  IID_IBaseVideoMixer = IBaseVideoMixer;
  {$EXTERNALSYM IID_IBaseVideoMixer}


  // The SDK base classes contain a base video mixer class. Video mixing in a
  // software environment is tricky because we typically have multiple streams
  // each sending data at unpredictable times. To work with this we defined a
  // pin that is the lead pin, when data arrives on this pin we do a mix. As
  // an alternative we may not want to have a lead pin but output samples at
  // predefined spaces, like one every 1/15 of a second, this interfaces also
  // supports that mode of operations (there is a working video mixer sample)

const
  // Used for true colour images that also have a palette

  iPALETTE_COLORS = 256;          // Maximum colours in palette
  {$EXTERNALSYM iPALETTE_COLORS}
  iEGA_COLORS = 16;               // Number colours in EGA palette
  {$EXTERNALSYM iEGA_COLORS}
  iMASK_COLORS = 3;               // Maximum three components
  {$EXTERNALSYM iMASK_COLORS}
  iTRUECOLOR = 16;                // Minimum true colour device
  {$EXTERNALSYM iTRUECOLOR}
  iRED = 0;                       // Index position for RED mask
  {$EXTERNALSYM iRED}
  iGREEN = 1;                     // Index position for GREEN mask
  {$EXTERNALSYM iGREEN}
  iBLUE = 2;                      // Index position for BLUE mask
  {$EXTERNALSYM iBLUE}
  iPALETTE = 8;                   // Maximum colour depth using a palette
  {$EXTERNALSYM iPALETTE}
  iMAXBITS = 8;                   // Maximum bits per colour component
  {$EXTERNALSYM iMAXBITS}



type
  PTrueColorInfo = ^TRUECOLORINFO;
  tag_TRUECOLORINFO = record
    dwBitMasks: array[0..iMASK_COLORS - 1] of DWORD;
    bmiColors: array[0..iPALETTE_COLORS - 1] of TRGBQuad;
  end;
  {$EXTERNALSYM tag_TRUECOLORINFO}
  TRUECOLORINFO = tag_TRUECOLORINFO;
  {$EXTERNALSYM TRUECOLORINFO}
  // Used for true colour images that also have a palette


  PVideoInfoHeader = ^VIDEOINFOHEADER;
  tagVIDEOINFOHEADER = record
    rcSource: TRect;                    // The bit we really want to use
    rcTarget: TRect;                    // Where the video should go
    dwBitRate: DWORD;                   // Approximate bit data rate
    dwBitErrorRate: DWORD;              // Bit error rate for this stream
    AvgTimePerFrame: REFERENCE_TIME;    // Average time per frame (100ns units)
    bmiHeader: TBitmapInfoHeader;
  end;
  {$EXTERNALSYM tagVIDEOINFOHEADER}
  VIDEOINFOHEADER = tagVIDEOINFOHEADER;
  {$EXTERNALSYM VIDEOINFOHEADER}
  // The BITMAPINFOHEADER (TBitmapInfoHeader) contains all the details about the video stream such
  // as the actual image dimensions and their pixel depth. A source filter may
  // also request that the sink take only a section of the video by providing a
  // clipping rectangle in rcSource. In the worst case where the sink filter
  // forgets to check this on connection it will simply render the whole thing
  // which isn't a disaster. Ideally a sink filter will check the rcSource and
  // if it doesn't support image extraction and the rectangle is not empty then
  // it will reject the connection. A filter should use SetRectEmpty to reset a
  // rectangle to all zeroes (and IsRectEmpty to later check the rectangle).
  // The rcTarget specifies the destination rectangle for the video, for most
  // source filters they will set this to all zeroes, a downstream filter may
  // request that the video be placed in a particular area of the buffers it
  // supplies in which case it will call QueryAccept with a non empty target




  // See: CONVERTED MACRO'S
  // make sure the pbmi is initialized before using these macros
  //
  //#define TRUECOLOR(pbmi)  ((TRUECOLORINFO *)(((LPBYTE)&((pbmi)->bmiHeader)) \
  //                                      + (pbmi)->bmiHeader.biSize))
  //#define COLORS(pbmi)    ((RGBQUAD *)(((LPBYTE)&((pbmi)->bmiHeader))     \
  //                                      + (pbmi)->bmiHeader.biSize))
  //#define BITMASKS(pbmi)  ((DWORD *)(((LPBYTE)&((pbmi)->bmiHeader))       \
  //                                      + (pbmi)->bmiHeader.biSize))
  //}

  PVideoInfo = ^VIDEOINFO;
  tagVIDEOINFO = record
    rcSource: TRect;                   // The bit we really want to use
    rcTarget: TRect;                   // Where the video should go
    dwBitRate: DWORD;                  // Approximate bit data rate
    dwBitErrorRate: DWORD;             // Bit error rate for this stream
    AvgTimePerFrame: REFERENCE_TIME;   // Average time per frame (100ns units)

    bmiHeader: TBitmapInfoHeader;

    case Integer of
      0: (bmiColors: array[0..iPALETTE_COLORS-1] of TRGBQuad);  // Colour palette
      1: (dwBitMasks: array[0..iMASK_COLORS-1] of DWORD);       // True colour masks
      2: (TrueColorInfo: TRUECOLORINFO);                        // Both of the above
  end;
  {$EXTERNALSYM tagVIDEOINFO}
  VIDEOINFO = tagVIDEOINFO;
  {$EXTERNALSYM VIDEOINFO}
  // All the image based filters use this to communicate their media types. It's
  // centred principally around the BITMAPINFO. This structure always contains a
  // BITMAPINFOHEADER followed by a number of other fields depending on what the
  // BITMAPINFOHEADER contains. If it contains details of a palettised format it
  // will be followed by one or more RGBQUADs defining the palette. If it holds
  // details of a true colour format then it may be followed by a set of three
  // DWORD bit masks that specify where the RGB data can be found in the image
  // (For more information regarding BITMAPINFOs see the Win32 documentation)
  //
  // The rcSource and rcTarget fields are not for use by filters supplying the
  // data. The destination (target) rectangle should be set to all zeroes. The
  // source may also be zero filled or set with the dimensions of the video. So
  // if the video is 352x288 pixels then set it to (0,0,352,288). These fields
  // are mainly used by downstream filters that want to ask the source filter
  // to place the image in a different position in an output buffer. So when
  // using for example the primary surface the video renderer may ask a filter
  // to place the video images in a destination position of (100,100,452,388)
  // on the display since that's where the window is positioned on the display
  //
  // !!! WARNING !!!
  // DO NOT use this structure unless you are sure that the BITMAPINFOHEADER
  // has a normal biSize = sizeof(BITMAPINFOHEADER) !
  // !!! WARNING !!!


  // These macros define some standard bitmap format sizes
const
  SIZE_EGA_PALETTE = iEGA_COLORS * SizeOf(TRGBQuad);
  {$EXTERNALSYM SIZE_EGA_PALETTE}
  SIZE_PALETTE = iPALETTE_COLORS * SizeOf(TRGBQuad);
  {$EXTERNALSYM SIZE_PALETTE}
  SIZE_MASKS = iMASK_COLORS * SizeOf(DWORD);
  {$EXTERNALSYM SIZE_MASKS}


  SIZE_PREHEADER = 48;    // offset TVideoInfoHeader.bmiHeader
  {$EXTERNALSYM SIZE_PREHEADER}
  SIZE_VIDEOHEADER = SizeOf(VIDEOINFOHEADER);
  {$EXTERNALSYM SIZE_VIDEOHEADER}


// !!! for abnormal biSizes
// #define SIZE_VIDEOHEADER(pbmi) ((pbmi)->bmiHeader.biSize + SIZE_PREHEADER)

// DIBSIZE calculates the number of bytes required by an image
{
function WIDTHBYTES(bits: Integer): DWORD;
function DIBWIDTHBYTES(const bhi: TBitmapInfoHeader): DWORD;
function _DIBSIZE(const bmi: TBitmapInfoHeader): DWORD;
function DIBSIZE(const bmi: TBitmapInfoHeader): DWORD;
{
#define WIDTHBYTES(bits) ((DWORD)(((bits)+31) & (~31)) / 8)
#define DIBWIDTHBYTES(bi) (DWORD)WIDTHBYTES((DWORD)(bi).biWidth * (DWORD)(bi).biBitCount)
#define _DIBSIZE(bi) (DIBWIDTHBYTES(bi) * (DWORD)(bi).biHeight)
#define DIBSIZE(bi) ((bi).biHeight < 0 ? (-1)*(_DIBSIZE(bi)) : _DIBSIZE(bi))
}
// This compares the bit masks between two VIDEOINFOHEADERs
{
function BIT_MASKS_MATCH(const bmi1, bmi2: TBitmapInfo): BOOL;
{
#define BIT_MASKS_MATCH(pbmi1,pbmi2)                                \
    (((pbmi1)->dwBitMasks[iRED] == (pbmi2)->dwBitMasks[iRED]) &&        \
     ((pbmi1)->dwBitMasks[iGREEN] == (pbmi2)->dwBitMasks[iGREEN]) &&    \
     ((pbmi1)->dwBitMasks[iBLUE] == (pbmi2)->dwBitMasks[iBLUE]))
}
// These zero fill different parts of the VIDEOINFOHEADER structure

// Only use these macros for pbmi's with a normal BITMAPINFOHEADER biSize
{procedure RESET_MASKS(var bmi: TBitmapInfo);
procedure RESET_HEADER(var bmi: TBitmapInfo);
procedure RESET_PALETTE(var bmi: TBitmapInfo);
{
#define RESET_MASKS(pbmi) (ZeroMemory((PVOID)(pbmi)->dwBitFields,SIZE_MASKS))
#define RESET_HEADER(pbmi) (ZeroMemory((PVOID)(pbmi),SIZE_VIDEOHEADER))
#define RESET_PALETTE(pbmi) (ZeroMemory((PVOID)(pbmi)->bmiColors,SIZE_PALETTE));
}
{
// !!! This is the right way to do it, but may break existing code
#define RESET_MASKS(pbmi) (ZeroMemory((PVOID)(((LPBYTE)(pbmi)->bmiHeader) + \
                        (pbmi)->bmiHeader.biSize,SIZE_MASKS)))
#define RESET_HEADER(pbmi) (ZeroMemory((PVOID)(pbmi), SIZE_PREHEADER +      \
                        sizeof(BITMAPINFOHEADER)))
#define RESET_PALETTE(pbmi) (ZeroMemory((PVOID)(((LPBYTE)(pbmi)->bmiHeader) + \
                        (pbmi)->bmiHeader.biSize,SIZE_PALETTE))
}

// Other (hopefully) useful bits and bobs
{
#define PALETTISED(pbmi) ((pbmi)->bmiHeader.biBitCount <= iPALETTE)
#define PALETTE_ENTRIES(pbmi) ((DWORD) 1 << (pbmi)->bmiHeader.biBitCount)

// Returns the address of the BITMAPINFOHEADER from the VIDEOINFOHEADER
#define HEADER(pVideoInfo) (&(((VIDEOINFOHEADER *) (pVideoInfo))->bmiHeader))
 }


type
  PMPEG1VideoInfo = ^tagMPEG1VIDEOINFO;
  tagMPEG1VIDEOINFO = record
    hdr: VIDEOINFOHEADER;                   // Compatible with VIDEOINFO
    dwStartTimeCode: DWORD;                 // 25-bit Group of pictures time code
                                            // at start of data
    cbSequenceHeader: DWORD;                // Length in bytes of bSequenceHeader
    bSequenceHeader: array of Byte;         // Sequence header including
                                            // quantization matrices if any
  end;
  {$EXTERNALSYM tagMPEG1VIDEOINFO}
  MPEG1VIDEOINFO = tagMPEG1VIDEOINFO;
  {$EXTERNALSYM MPEG1VIDEOINFO}

  // MPEG variant - includes a DWORD length followed by the
  // video sequence header after the video header.
  //
  // The sequence header includes the sequence header start code and the
  // quantization matrices associated with the first sequence header in the
  // stream so is a maximum of 140 bytes long.


const
  MAX_SIZE_MPEG1_SEQUENCE_INFO = 140;
  {$EXTERNALSYM MAX_SIZE_MPEG1_SEQUENCE_INFO}
{
#define SIZE_MPEG1VIDEOINFO(pv) (FIELD_OFFSET(MPEG1VIDEOINFO, bSequenceHeader[0]) + (pv)->cbSequenceHeader)
#define MPEG1_SEQUENCE_INFO(pv) ((const BYTE *)(pv)->bSequenceHeader)
}

// Analog video variant - Use this when the format is FORMAT_AnalogVideo
//
// rcSource defines the portion of the active video signal to use
// rcTarget defines the destination rectangle
//    both of the above are relative to the dwActiveWidth and dwActiveHeight fields
// dwActiveWidth is currently set to 720 for all formats (but could change for HDTV)
// dwActiveHeight is 483 for NTSC and 575 for PAL/SECAM  (but could change for HDTV)
type

  PANALOGVIDEOINFO = ^ANALOGVIDEOINFO;
  tagAnalogVideoInfo = record
    rcSource: TRect;                   // Width max is 720, height varies w/ TransmissionS
    rcTarget: TRect;                   // Where the video should go
    dwBitRate: DWORD;                  // Always 720 (CCIR-601 active samples per line)
    dwBitErrorRate: DWORD;             // 483 for NTSC, 575 for PAL/SECAM
    AvgTimePerFrame: REFERENCE_TIME;  // Normal ActiveMovie units (100 nS)
  end;
  {$EXTERNALSYM tagAnalogVideoInfo}
  ANALOGVIDEOINFO = tagAnalogVideoInfo;
  {$EXTERNALSYM ANALOGVIDEOINFO}

//
// AM_KSPROPSETID_FrameStep property set definitions
//

  PAM_PROPERTY_FRAMESTEP = ^AM_PROPERTY_FRAMESTEP;
  AM_PROPERTY_FRAMESTEP = (
    //  Step
    AM_PROPERTY_FRAMESTEP_STEP            = 1,
    AM_PROPERTY_FRAMESTEP_CANCEL          = 2 ,
    //  S_OK for these 2 means we can - S_FALSE if we can't
    AM_PROPERTY_FRAMESTEP_CANSTEP         = 3,
    AM_PROPERTY_FRAMESTEP_CANSTEPMULTIPLE = 4
  );
  {$EXTERNALSYM AM_PROPERTY_FRAMESTEP}


  PAM_FRAMESTEP_STEP = ^AM_FRAMESTEP_STEP;
  {$EXTERNALSYM _AM_FRAMESTEP_STEP}
  _AM_FRAMESTEP_STEP = record
    //  1 means step 1 frame forward
    //  0 is invalid
    //  n (n > 1) means skip n - 1 frames and show the nth
     dwFramesToStep: DWORD;
  end;
  {$EXTERNALSYM AM_FRAMESTEP_STEP}
  AM_FRAMESTEP_STEP = _AM_FRAMESTEP_STEP;

  // Additional Prototypes for ALL interfaces

  // CONVERTED MACRO'S
  // =================
  // make sure the pbmi is initialized before using these macros
  //

  function TRUECOLOR(pbmi: PVIDEOINFO): PTRUECOLORINFO;
  {$EXTERNALSYM TRUECOLOR}

  function COLORS(pbmi: PVIDEOINFO): PRGBQUAD;
  {$EXTERNALSYM COLORS}

  function BITMASKS(pbmi: PVIDEOINFO): PDWORD;
  {$EXTERNALSYM BITMASKS}

  function BIT_MASKS_MATCH(pbmi1: PVIDEOINFO;
                           pbmi2: PVIDEOINFO): BOOL;
  {$EXTERNALSYM BIT_MASKS_MATCH}

  // End of Additional Prototypes

implementation

  // Implement Additional functions here.

  {$POINTERMATH ON}

  //Converted macro's
  //=================

  //#define TRUECOLOR(pbmi)  ((TRUECOLORINFO *)(((LPBYTE)&((pbmi)->bmiHeader)) \
  //        + (pbmi)->bmiHeader.biSize))
  function TRUECOLOR(pbmi: PVIDEOINFO): PTRUECOLORINFO;
    begin
      Result:= PTRUECOLORINFO(SizeOf(LPBYTE) + SizeOf(pbmi.bmiHeader) + pbmi^.bmiHeader.biSize);
    end;
  // The TRUECOLOR macro returns the TRUECOLORINFO structure from a VIDEOINFO structure.
  // The TRUECOLORINFO structure contains color bitmasks followed by palette information,
  // and is used for true-color images (16 bit or higher) that contain palettes.
  // Parameters
  // pbmi
  //    Pointer to a VIDEOINFO structure.
  // Return value
  //    Returns a pointer to a TRUECOLORINFO structure.
  // Remarks
  //    This macro calculates the address as an offset from the start of the
  //    BITMAPINFOHEADER structure, using the value of bmiHeader.biSize.
  //    Make sure to initialize the VIDEOINFO structure before calling this macro.


  //#define COLORS(pbmi)    ((RGBQUAD *)(((LPBYTE)&((pbmi)->bmiHeader))     \
  //                                      + (pbmi)->bmiHeader.biSize))
  function COLORS(pbmi: PVIDEOINFO): PRGBQUAD;
  begin
    Result:= PRGBQUAD(SizeOf(LPBYTE) + SizeOf(pbmi.bmiHeader) + pbmi^.bmiHeader.biSize);
  end;
  // RGBQUAD* COLORS(pbmi);
  //
  // The COLORS macro retrieves the palette entries from a VIDEOINFO structure.
  // Parameters
  // pbmi
  //    Pointer to a VIDEOINFO structure.
  // Return value
  //    Returns a pointer to an array of RGBQUAD structures.  For Delphi you should turn {$POINTERMATH ON}
  // Remarks
  //  This macro calculates the address as an offset from the start of the
  //  BITMAPINFOHEADER structure, using the value of bmiHeader.biSize.
  //  Make sure to initialize the VIDEOINFO structure before calling this macro.


  //#define BITMASKS(pbmi)  ((DWORD *)(((LPBYTE)&((pbmi)->bmiHeader))       \
  //                                      + (pbmi)->bmiHeader.biSize))
  function BITMASKS(pbmi: PVIDEOINFO): PDWORD;
    Begin
      Result:= PDWORD(SizeOf(LPBYTE) + SizeOf(pbmi.bmiHeader) + pbmi^.bmiHeader.biSize);
    End;
  // The BITMASKS macro retrieves the color masks from a VIDEOINFO structure.
  // Parameters
  // pbmi
  //    Pointer to a VIDEOINFO structure.
  // Return value
  //    Returns a DWORD pointer value that is the address of the dwBitMasks member of
  //    the VIDEOINFO structure.
  // Remarks
  //    This macro calculates the address as an offset from the start of the BITMAPINFOHEADER structure,
  //    using the value of bmiHeader.biSize.
  //    Make sure to initialize the VIDEOINFO structure before calling this macro.
  //
  //    You can access the color masks in the array using the following constants:
  //      #define iRED   0
  //      #define iGREEN 1
  //      #define iBLUE  2
  //
  // Examples (D)
  //    pVi: PVIDEOINFO;
  //    // Initialize pVi (not shown).
  //
  //    DWORD(dwRed):= BITMASKS(pVi, 0);    //Red
  //    DWORD(dwGreen):= BITMASKS(pVi, 1);  //Green
  //    DWORD(dwBlue):= BITMASKS(pVi, 2);   //Blue


  //
  function BIT_MASKS_MATCH(pbmi1: PVIDEOINFO;
                           pbmi2: PVIDEOINFO): BOOL;
    begin
      Result:= BOOL(CompareMem(pbmi1,
                               pbmi2,
                               SizeOf(pbmi1)));
    end;
  //Parameters
  // pbmi1
  //    Pointer to the first VIDEOINFO structure.
  // pbmi2
  //    Pointer to the second VIDEOINFO structure.
  // Return value
  //    Returns TRUE if the color masks are identical, or FALSE otherwise.

  {$POINTERMATH OFF}

end.
