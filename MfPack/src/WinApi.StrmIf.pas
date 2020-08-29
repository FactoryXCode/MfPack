// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - Shared
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.StrmIf.pas
// Kind: Pascal / Delphi unit
// Release date: 12/12/2015
// Language: ENU
//
// Revision Version: 3.0.0
// Description: ActiveMovie interface definitions.
// also included with DirectShow.pas,
// s code is not up to date.
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
// Source: strmif.h
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
unit WinApi.StrmIf;

  {$HPPEMIT '#include "strmif.h"'}

interface

uses
  {WinApi}
  WinApi.Windows,
  WinApi.MMSystem,
  WinApi.WinApiTypes,
  {WinApi.ActiveX}
  WinApi.ActiveX.ObjIdl,
  WinApi.ActiveX.ObjIdlbase,
  WinApi.ActiveX.OaIdl,
  {Use WinApi or Clootie Dx}
  WinApi.DirectDraw,
  {System}
  System.SyncObjs,
  System.Win.ComObj;


  {$MINENUMSIZE 4}

  {$IFDEF WIN32}
    {$ALIGN 1}
  {$ELSE}
    {$ALIGN 8} // Win64
  {$ENDIF}

  {$I 'WinApiTypes.inc'}

const

  CHARS_IN_GUID   = 39;
  {$EXTERNALSYM CHARS_IN_GUID}

  MAX_PIN_NAME    = 128;
  {$EXTERNALSYM MAX_PIN_NAME}
  MAX_FILTER_NAME = 128;
  {$EXTERNALSYM MAX_FILTER_NAME}


  // Use those key's for the COLORKEY struct.
  // Key type. Can be CK_NOCOLORKEY, CK_INDEX, or CK_RGB. CK_INDEX and CK_RGB can be logically combined.
type
  __MIDL___MIDL_itf_strmif_0000_0026_0001 = (
    CK_NOCOLORKEY = 0,
    CK_INDEX      = $1,
    CK_RGB        = $2);
  {$EXTERNALSYM __MIDL___MIDL_itf_strmif_0000_0026_0001}

const

  CDEF_CLASS_DEFAULT                  = $0001;
  {$EXTERNALSYM CDEF_CLASS_DEFAULT}
  CDEF_BYPASS_CLASS_MANAGER           = $0002;
  {$EXTERNALSYM CDEF_BYPASS_CLASS_MANAGER}
  CDEF_MERIT_ABOVE_DO_NOT_USE         = $0008;
  {$EXTERNALSYM CDEF_MERIT_ABOVE_DO_NOT_USE}
  CDEF_DEVMON_CMGR_DEVICE             = $0010;
  {$EXTERNALSYM CDEF_DEVMON_CMGR_DEVICE}
  CDEF_DEVMON_DMO                     = $0020;
  {$EXTERNALSYM CDEF_DEVMON_DMO}
  CDEF_DEVMON_PNP_DEVICE              = $0040;
  {$EXTERNALSYM CDEF_DEVMON_PNP_DEVICE}
  CDEF_DEVMON_FILTER                  = $0080;
  {$EXTERNALSYM CDEF_DEVMON_FILTER}
  CDEF_DEVMON_SELECTIVE_MASK          = $00F0;
  {$EXTERNALSYM CDEF_DEVMON_SELECTIVE_MASK}


  // IMemAllocator
  AM_GBF_PREVFRAMESKIPPED = 1;
  {$EXTERNALSYM AM_GBF_PREVFRAMESKIPPED}
  AM_GBF_NOTASYNCPOINT    = 2;
  {$EXTERNALSYM AM_GBF_NOTASYNCPOINT}
  AM_GBF_NOWAIT           = 4;
  {$EXTERNALSYM AM_GBF_NOWAIT}
  AM_GBF_NODDSURFACELOCK  = 8;
  {$EXTERNALSYM AM_GBF_NODDSURFACELOCK}


  // IAMAudioInputMixer
  AMF_AUTOMATICGAIN = -1;
  {$EXTERNALSYM AMF_AUTOMATICGAIN}


  // IAMAnalogVideoDecoder
  AnalogVideo_NTSC_Mask               = $00000007;
  {$EXTERNALSYM AnalogVideo_NTSC_Mask}
  AnalogVideo_PAL_Mask                = $00100FF0;
  {$EXTERNALSYM AnalogVideo_PAL_Mask}
  AnalogVideo_SECAM_Mask              = $000FF000;
  {$EXTERNALSYM AnalogVideo_SECAM_Mask}


  // IKsPropertySet
  KSPROPERTY_SUPPORT_GET = 1;
  {$EXTERNALSYM KSPROPERTY_SUPPORT_GET}
  KSPROPERTY_SUPPORT_SET = 2;
  {$EXTERNALSYM KSPROPERTY_SUPPORT_SET}


  // IEnumStreamIdMap
  MPEG2_PROGRAM_STREAM_MAP           = $00000000;
  {$EXTERNALSYM MPEG2_PROGRAM_STREAM_MAP}
  MPEG2_PROGRAM_ELEMENTARY_STREAM    = $00000001;
  {$EXTERNALSYM MPEG2_PROGRAM_ELEMENTARY_STREAM}
  MPEG2_PROGRAM_DIRECTORY_PES_PACKET = $00000002;
  {$EXTERNALSYM MPEG2_PROGRAM_DIRECTORY_PES_PACKET}
  MPEG2_PROGRAM_PACK_HEADER          = $00000003;
  {$EXTERNALSYM MPEG2_PROGRAM_PACK_HEADER}
  MPEG2_PROGRAM_PES_STREAM           = $00000004;
  {$EXTERNALSYM MPEG2_PROGRAM_PES_STREAM}
  MPEG2_PROGRAM_SYSTEM_HEADER        = $00000005;
  {$EXTERNALSYM MPEG2_PROGRAM_SYSTEM_HEADER}
  SUBSTREAM_FILTER_VAL_NONE          = $10000000;
  {$EXTERNALSYM SUBSTREAM_FILTER_VAL_NONE}


  // IAMDecoderCaps
  AM_GETDECODERCAP_QUERY_VMR_SUPPORT  = $00000001;
  {$EXTERNALSYM AM_GETDECODERCAP_QUERY_VMR_SUPPORT}
  VMR_NOTSUPPORTED                    = $00000000;
  {$EXTERNALSYM VMR_NOTSUPPORTED}
  VMR_SUPPORTED                       = $00000001;
  {$EXTERNALSYM VMR_SUPPORTED}
  AM_QUERY_DECODER_VMR_SUPPORT        = $00000001;
  {$EXTERNALSYM AM_QUERY_DECODER_VMR_SUPPORT}
  AM_QUERY_DECODER_DXVA_1_SUPPORT     = $00000002;
  {$EXTERNALSYM AM_QUERY_DECODER_DXVA_1_SUPPORT}
  AM_QUERY_DECODER_DVD_SUPPORT        = $00000003;
  {$EXTERNALSYM AM_QUERY_DECODER_DVD_SUPPORT}
  AM_QUERY_DECODER_ATSC_SD_SUPPORT    = $00000004;
  {$EXTERNALSYM AM_QUERY_DECODER_ATSC_SD_SUPPORT}
  AM_QUERY_DECODER_ATSC_HD_SUPPORT    = $00000005;
  {$EXTERNALSYM AM_QUERY_DECODER_ATSC_HD_SUPPORT}
  AM_GETDECODERCAP_QUERY_VMR9_SUPPORT = $00000006;
  {$EXTERNALSYM AM_GETDECODERCAP_QUERY_VMR9_SUPPORT}
  AM_GETDECODERCAP_QUERY_EVR_SUPPORT  = $00000007;
  {$EXTERNALSYM AM_GETDECODERCAP_QUERY_EVR_SUPPORT}
  DECODER_CAP_NOTSUPPORTED            = $00000000;
  {$EXTERNALSYM DECODER_CAP_NOTSUPPORTED}
  DECODER_CAP_SUPPORTED               = $00000001;
  {$EXTERNALSYM DECODER_CAP_SUPPORTED}


  // IVMRMixerBitmap
  VMRBITMAP_DISABLE                   = $00000001;
  {$EXTERNALSYM VMRBITMAP_DISABLE}
  VMRBITMAP_HDC                       = $00000002;
  {$EXTERNALSYM VMRBITMAP_HDC}
  VMRBITMAP_ENTIREDDS                 = $00000004;
  {$EXTERNALSYM VMRBITMAP_ENTIREDDS}
  VMRBITMAP_SRCCOLORKEY               = $00000008;
  {$EXTERNALSYM VMRBITMAP_SRCCOLORKEY}
  VMRBITMAP_SRCRECT                   = $00000010;
  {$EXTERNALSYM VMRBITMAP_SRCRECT}


  // IDvdInfo2
  DVD_TITLE_MENU                      = $000;
  {$EXTERNALSYM DVD_TITLE_MENU}
  DVD_STREAM_DATA_CURRENT             = $800;
  {$EXTERNALSYM DVD_STREAM_DATA_CURRENT}
  DVD_STREAM_DATA_VMGM                = $400;
  {$EXTERNALSYM DVD_STREAM_DATA_VMGM}
  DVD_STREAM_DATA_VTSM                = $401;
  {$EXTERNALSYM DVD_STREAM_DATA_VTSM}
  DVD_DEFAULT_AUDIO_STREAM            = $0F;
  {$EXTERNALSYM DVD_DEFAULT_AUDIO_STREAM}

  DVD_AUDIO_CAPS_AC3                  = $00000001;
  {$EXTERNALSYM DVD_AUDIO_CAPS_AC3}
  DVD_AUDIO_CAPS_MPEG2                = $00000002;
  {$EXTERNALSYM DVD_AUDIO_CAPS_MPEG2}
  DVD_AUDIO_CAPS_LPCM                 = $00000004;
  {$EXTERNALSYM DVD_AUDIO_CAPS_LPCM}
  DVD_AUDIO_CAPS_DTS                  = $00000008;
  {$EXTERNALSYM DVD_AUDIO_CAPS_DTS}
  DVD_AUDIO_CAPS_SDDS                 = $00000010;
  {$EXTERNALSYM DVD_AUDIO_CAPS_SDDS}


  // IOverlay
const
  //Enum __MIDL___MIDL_itf_strmif_0000_0026_0002
  ADVISE_NONE           = DWord($0);
  {$EXTERNALSYM ADVISE_NONE}
  ADVISE_CLIPPING       = DWord($1);
  {$EXTERNALSYM ADVISE_CLIPPING}
  ADVISE_PALETTE        = DWord($2);
  {$EXTERNALSYM ADVISE_PALETTE}
  ADVISE_COLORKEY       = DWord($4);
  {$EXTERNALSYM ADVISE_COLORKEY}
  ADVISE_POSITION       = DWord($8);
  {$EXTERNALSYM ADVISE_POSITION}
  ADVISE_DISPLAY_CHANGE = DWord($10);
  {$EXTERNALSYM ADVISE_DISPLAY_CHANGE}

  ADVISE_ALL  = (ADVISE_CLIPPING or
                 ADVISE_PALETTE or
                 ADVISE_COLORKEY or
                 ADVISE_POSITION);
  {$EXTERNALSYM ADVISE_ALL}

  ADVISE_ALL2 = (ADVISE_ALL or
                 ADVISE_DISPLAY_CHANGE);
  {$EXTERNALSYM ADVISE_ALL2}


const

  AnalogVideo_None          = 0;
  {$EXTERNALSYM AnalogVideo_None}
  AnalogVideo_NTSC_M        = $1;
  {$EXTERNALSYM AnalogVideo_NTSC_M}
  AnalogVideo_NTSC_M_J      = $2;
  {$EXTERNALSYM AnalogVideo_NTSC_M_J}
  AnalogVideo_NTSC_433      = $4;
  {$EXTERNALSYM AnalogVideo_NTSC_433}
  AnalogVideo_PAL_B         = $10;
  {$EXTERNALSYM AnalogVideo_PAL_B}
  AnalogVideo_PAL_D         = $20;
  {$EXTERNALSYM AnalogVideo_PAL_D}
  AnalogVideo_PAL_G         = $40;
  {$EXTERNALSYM AnalogVideo_PAL_G}
  AnalogVideo_PAL_H         = $80;
  {$EXTERNALSYM AnalogVideo_PAL_H}
  AnalogVideo_PAL_I         = $100;
  {$EXTERNALSYM AnalogVideo_PAL_I}
  AnalogVideo_PAL_M         = $200;
  {$EXTERNALSYM AnalogVideo_PAL_M}
  AnalogVideo_PAL_N         = $400;
  {$EXTERNALSYM AnalogVideo_PAL_N}
  AnalogVideo_PAL_60        = $800;
  {$EXTERNALSYM AnalogVideo_PAL_60}
  AnalogVideo_SECAM_B       = $1000;
  {$EXTERNALSYM AnalogVideo_SECAM_B}
  AnalogVideo_SECAM_D       = $2000;
  {$EXTERNALSYM AnalogVideo_SECAM_D}
  AnalogVideo_SECAM_G       = $4000;
  {$EXTERNALSYM AnalogVideo_SECAM_G}
  AnalogVideo_SECAM_H       = $8000;
  {$EXTERNALSYM AnalogVideo_SECAM_H}
  AnalogVideo_SECAM_K       = $10000;
  {$EXTERNALSYM AnalogVideo_SECAM_K}
  AnalogVideo_SECAM_K1      = $20000;
  {$EXTERNALSYM AnalogVideo_SECAM_K1}
  AnalogVideo_SECAM_L       = $40000;
  {$EXTERNALSYM AnalogVideo_SECAM_L}
  AnalogVideo_SECAM_L1      = $80000;
  {$EXTERNALSYM AnalogVideo_SECAM_L1}
  AnalogVideo_PAL_N_COMBO   = $100000;
  {$EXTERNALSYM AnalogVideo_PAL_N_COMBO}

  AnalogVideoMask_MCE_NTSC  = ((((((AnalogVideo_NTSC_M or
                                    AnalogVideo_NTSC_M_J) or
                                    AnalogVideo_NTSC_433) or
                                    AnalogVideo_PAL_M) or
                                    AnalogVideo_PAL_N) or
                                    AnalogVideo_PAL_60) or
                                    AnalogVideo_PAL_N_COMBO);
  {$EXTERNALSYM AnalogVideoMask_MCE_NTSC}

  AnalogVideoMask_MCE_PAL   = ((((AnalogVideo_PAL_B or
                                  AnalogVideo_PAL_D) or
                                  AnalogVideo_PAL_G) or
                                  AnalogVideo_PAL_H) or
                                  AnalogVideo_PAL_I);
  {$EXTERNALSYM AnalogVideoMask_MCE_PAL}

  AnalogVideoMask_MCE_SECAM = ((((((( AnalogVideo_SECAM_B or
                                      AnalogVideo_SECAM_D) or
                                      AnalogVideo_SECAM_G) or
                                      AnalogVideo_SECAM_H) or
                                      AnalogVideo_SECAM_K) or
                                      AnalogVideo_SECAM_K1) or
                                      AnalogVideo_SECAM_L) or
                                      AnalogVideo_SECAM_L1);
  {$EXTERNALSYM AnalogVideoMask_MCE_SECAM}

const
  // Merit
  MERIT_PREFERRED      = $800000;
  {$EXTERNALSYM MERIT_PREFERRED}
  MERIT_NORMAL         = $600000;
  {$EXTERNALSYM MERIT_NORMAL}
  MERIT_UNLIKELY       = $400000;
  {$EXTERNALSYM MERIT_UNLIKELY}
  MERIT_DO_NOT_USE     = $200000;
  {$EXTERNALSYM MERIT_DO_NOT_USE}
  MERIT_SW_COMPRESSOR  = $100000;
  {$EXTERNALSYM MERIT_SW_COMPRESSOR}
  MERIT_HW_COMPRESSOR  = $100050;
  {$EXTERNALSYM MERIT_HW_COMPRESSOR}


  // __MIDL___MIDL_itf_strmif_0000_0023_0001
  REG_PINFLAG_B_ZERO     = $1;
  {$EXTERNALSYM REG_PINFLAG_B_ZERO}
  REG_PINFLAG_B_RENDERER = $2;
  {$EXTERNALSYM REG_PINFLAG_B_RENDERER}
  REG_PINFLAG_B_MANY     = $4;
  {$EXTERNALSYM REG_PINFLAG_B_MANY}
  REG_PINFLAG_B_OUTPUT   = $8;
  {$EXTERNALSYM REG_PINFLAG_B_OUTPUT}

  // __MIDL___MIDL_itf_strmif_0000_0122_0001
  MAX_NUMBER_OF_STREAMS   = 16;
  {$EXTERNALSYM MAX_NUMBER_OF_STREAMS}

  // Interface Identifiers

  STATIC_IID_IKsPropertySet          : TGUID = '{31EFAC30-515C-11d0-A9AA-00AA0061BE93}';
  {$EXTERNALSYM STATIC_IID_IKsPropertySet}

type
  PAM_SAMPLE_PROPERTY_FLAGS = ^tagAM_SAMPLE_PROPERTY_FLAGS;
  tagAM_SAMPLE_PROPERTY_FLAGS = DWord;
  {$EXTERNALSYM tagAM_SAMPLE_PROPERTY_FLAGS}
  AM_SAMPLE_PROPERTY_FLAGS = tagAM_SAMPLE_PROPERTY_FLAGS;
  {$EXTERNALSYM AM_SAMPLE_PROPERTY_FLAGS}
const
  AM_SAMPLE_SPLICEPOINT         = AM_SAMPLE_PROPERTY_FLAGS($1);
  {$EXTERNALSYM AM_SAMPLE_SPLICEPOINT}
  AM_SAMPLE_PREROLL             = AM_SAMPLE_PROPERTY_FLAGS($2);
  {$EXTERNALSYM AM_SAMPLE_PREROLL}
  AM_SAMPLE_DATADISCONTINUITY   = AM_SAMPLE_PROPERTY_FLAGS($4);
  {$EXTERNALSYM AM_SAMPLE_DATADISCONTINUITY}
  AM_SAMPLE_TYPECHANGED         = AM_SAMPLE_PROPERTY_FLAGS($8);
  {$EXTERNALSYM AM_SAMPLE_TYPECHANGED}
  AM_SAMPLE_TIMEVALID           = AM_SAMPLE_PROPERTY_FLAGS($10);
  {$EXTERNALSYM AM_SAMPLE_TIMEVALID}
  AM_SAMPLE_TIMEDISCONTINUITY   = AM_SAMPLE_PROPERTY_FLAGS($40);
  {$EXTERNALSYM AM_SAMPLE_TIMEDISCONTINUITY}
  AM_SAMPLE_FLUSH_ON_PAUSE      = AM_SAMPLE_PROPERTY_FLAGS($80);
  {$EXTERNALSYM AM_SAMPLE_FLUSH_ON_PAUSE}
  AM_SAMPLE_STOPVALID           = AM_SAMPLE_PROPERTY_FLAGS($100);
  {$EXTERNALSYM AM_SAMPLE_STOPVALID}
  AM_SAMPLE_ENDOFSTREAM         = AM_SAMPLE_PROPERTY_FLAGS($200);
  {$EXTERNALSYM AM_SAMPLE_ENDOFSTREAM}
  AM_STREAM_MEDIA               = AM_SAMPLE_PROPERTY_FLAGS(0);
  {$EXTERNALSYM AM_STREAM_MEDIA}
  AM_STREAM_CONTROL             = AM_SAMPLE_PROPERTY_FLAGS(1);
  {$EXTERNALSYM AM_STREAM_CONTROL}


type
  PAM_SEEKING_SEEKING_FLAGS = ^AM_SEEKING_SEEKING_FLAGS;
  AM_SEEKING_SeekingFlags             = DWord;
  {$EXTERNALSYM AM_SEEKING_SeekingFlags}
  AM_SEEKING_SEEKING_FLAGS = AM_SEEKING_SeekingFlags;
  {$EXTERNALSYM AM_SEEKING_SEEKING_FLAGS}
const
  AM_SEEKING_NoPositioning          = AM_SEEKING_SEEKING_FLAGS(0);
  {$EXTERNALSYM AM_SEEKING_NoPositioning}
  AM_SEEKING_AbsolutePositioning    = AM_SEEKING_SEEKING_FLAGS($1);
  {$EXTERNALSYM AM_SEEKING_AbsolutePositioning}
  AM_SEEKING_RelativePositioning    = AM_SEEKING_SEEKING_FLAGS($2);
  {$EXTERNALSYM AM_SEEKING_RelativePositioning}
  AM_SEEKING_IncrementalPositioning = AM_SEEKING_SEEKING_FLAGS($3);
  {$EXTERNALSYM AM_SEEKING_IncrementalPositioning}
  AM_SEEKING_PositioningBitsMask    = AM_SEEKING_SEEKING_FLAGS($3);
  {$EXTERNALSYM AM_SEEKING_PositioningBitsMask}

  AM_SEEKING_SeekToKeyFrame         = AM_SEEKING_SEEKING_FLAGS($4);
  {$EXTERNALSYM AM_SEEKING_SeekToKeyFrame}
  AM_SEEKING_ReturnTime             = AM_SEEKING_SEEKING_FLAGS($8);
  {$EXTERNALSYM AM_SEEKING_ReturnTime}
  AM_SEEKING_Segment                = AM_SEEKING_SEEKING_FLAGS($10);
  {$EXTERNALSYM AM_SEEKING_Segment}
  AM_SEEKING_NoFlush                = AM_SEEKING_SEEKING_FLAGS($20);
  {$EXTERNALSYM AM_SEEKING_NoFlush}


type
  PAM_SEEKING_SEEKING_CAPABILITIES = ^AM_SEEKING_SEEKING_CAPABILITIES;
  AM_SEEKING_SeekingCapabilities = DWord;
  {$EXTERNALSYM AM_SEEKING_SeekingCapabilities}
  AM_SEEKING_SEEKING_CAPABILITIES = AM_SEEKING_SeekingCapabilities;
  {$EXTERNALSYM AM_SEEKING_SEEKING_CAPABILITIES}
const
  AM_SEEKING_CanSeekAbsolute  = AM_SEEKING_SEEKING_CAPABILITIES($1);
  {$EXTERNALSYM AM_SEEKING_CanSeekAbsolute}
  AM_SEEKING_CanSeekForwards  = AM_SEEKING_SEEKING_CAPABILITIES($2);
  {$EXTERNALSYM AM_SEEKING_CanSeekForwards}
  AM_SEEKING_CanSeekBackwards = AM_SEEKING_SEEKING_CAPABILITIES($4);
  {$EXTERNALSYM AM_SEEKING_CanSeekBackwards}
  AM_SEEKING_CanGetCurrentPos = AM_SEEKING_SEEKING_CAPABILITIES($8);
  {$EXTERNALSYM AM_SEEKING_CanGetCurrentPos}
  AM_SEEKING_CanGetStopPos    = AM_SEEKING_SEEKING_CAPABILITIES($10);
  {$EXTERNALSYM AM_SEEKING_CanGetStopPos}
  AM_SEEKING_CanGetDuration   = AM_SEEKING_SEEKING_CAPABILITIES($20);
  {$EXTERNALSYM AM_SEEKING_CanGetDuration}
  AM_SEEKING_CanPlayBackwards = AM_SEEKING_SEEKING_CAPABILITIES($40);
  {$EXTERNALSYM AM_SEEKING_CanPlayBackwards}
  AM_SEEKING_CanDoSegments    = AM_SEEKING_SEEKING_CAPABILITIES($80);
  {$EXTERNALSYM AM_SEEKING_CanDoSegments}
  AM_SEEKING_Source           = AM_SEEKING_SEEKING_CAPABILITIES($100);
  {$EXTERNALSYM AM_SEEKING_Source}

type
  PAM_STREAM_INFO_FLAGS = ^AM_STREAM_INFO_FLAGS;
  AM_STREAM_INFO_FLAGS = DWord;
  {$EXTERNALSYM AM_STREAM_INFO_FLAGS}
const
  AM_STREAM_INFO_START_DEFINED   = AM_STREAM_INFO_FLAGS($1);
  {$EXTERNALSYM AM_STREAM_INFO_START_DEFINED}
  AM_STREAM_INFO_STOP_DEFINED    = AM_STREAM_INFO_FLAGS($2);
  {$EXTERNALSYM AM_STREAM_INFO_STOP_DEFINED}
  AM_STREAM_INFO_DISCARDING      = AM_STREAM_INFO_FLAGS($4);
  {$EXTERNALSYM AM_STREAM_INFO_DISCARDING}
  AM_STREAM_INFO_STOP_SEND_EXTRA = AM_STREAM_INFO_FLAGS($10);
  {$EXTERNALSYM AM_STREAM_INFO_STOP_SEND_EXTRA}


type

  // Forward interface declarations

  {from OBJIDL.H / ActiveX.pas}
  //PIEnumMoniker = ^IEnumMoniker;

  PIEnumMediaTypes = ^IEnumMediaTypes;
  IEnumMediaTypes = interface;

  PIEnumFilters = ^IEnumFilters;
  IEnumFilters = interface;

  PIBaseFilter = ^IBaseFilter;
  IBaseFilter = interface;

  PIReferenceClock = ^IReferenceClock;
  IReferenceClock = interface;

  PIMemAllocatorNotifyCallbackTemp = ^IMemAllocatorNotifyCallbackTemp;
  IMemAllocatorNotifyCallbackTemp = interface;

  PIAMCopyCaptureFileProgress = ^IAMCopyCaptureFileProgress;
  IAMCopyCaptureFileProgress = interface;

  PIGraphConfigCallback = ^IGraphConfigCallback;
  IGraphConfigCallback = interface;

  PIVMRSurfaceAllocatorNotify = ^IVMRSurfaceAllocatorNotify;
  IVMRSurfaceAllocatorNotify = interface;

  PIVMRImageCompositor = ^IVMRImageCompositor;
  IVMRImageCompositor = interface;

  PIDDrawExclModeVideoCallback = ^IDDrawExclModeVideoCallback;
  IDDrawExclModeVideoCallback = interface;


////////////////////////////////////////////////////////////////////////////////


 // Interfaces and Structs


  // Interface ICreateDevEnum
  // ========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ICreateDevEnum);'}
  {$EXTERNALSYM ICreateDevEnum}
  ICreateDevEnum = interface(IUnknown)
    ['{29840822-5B84-11D0-BD3B-00A0C911CE86}']
    function CreateClassEnumerator(const clsidDeviceClass: REFCLSID;
                                   out ppEnumMoniker: PIEnumMoniker;
                                   const dwFlags: DWORD): HResult; stdcall;

  end;
  IID_ICreateDevEnum = ICreateDevEnum;
  {$EXTERNALSYM IID_ICreateDevEnum}


{#IfnDef PAM_MEDIA_TYPE}
{#EndIf}
  PAM_MEDIA_TYPE = ^AM_MEDIA_TYPE;
  _AMMediaType = record
    majortype: TGUID;
    subtype: TGUID;
    bFixedSizeSamples: BOOL;
    bTemporalCompression: BOOL;
    lSampleSize: ULONG;
    formattype: TGUID;
    pUnk: IUnknown;
    cbFormat: ULONG;
    pbFormat: Pointer;
  end;
  {$EXTERNALSYM _AMMediaType}
  AM_MEDIA_TYPE = _AMMediaType;
  {$EXTERNALSYM AM_MEDIA_TYPE}


  PPIN_DIRECTION = ^PIN_DIRECTION;
  _PinDirection   = (
    PINDIR_INPUT  = 0,
    PINDIR_OUTPUT = (PINDIR_INPUT + 1)
  );
  {$EXTERNALSYM _PinDirection}
  PIN_DIRECTION = _PinDirection;
  {$EXTERNALSYM PIN_DIRECTION}


  // The REFERENCE_TIME data type defines the units for reference times in DirectShow.
  // Each unit of reference time is 100 nanoseconds.
  PREFERENCE_TIME = ^REFERENCE_TIME;
  REFERENCE_TIME = LONGLONG;
  {$EXTERNALSYM REFERENCE_TIME}
  PReferenceTime = ^ReferenceTime;
  ReferenceTime = REFERENCE_TIME;
  {$EXTERNALSYM ReferenceTime}


{#IfnDef REFTIME}
  PREFTIME = ^REFTIME;
  REFTIME = Double;
  {$EXTERNALSYM REFTIME}
{#EndIf}

{#IfnDef HSEMAPHORE}
  PHSEMAPHORE = ^HSEMAPHORE;
  HSEMAPHORE = DWORD_PTR;
  {$EXTERNALSYM HSEMAPHORE}
{#EndIf}

{#IfnDef HEVENT}
  PHEVENT = ^HEVENT;
  HEVENT = DWORD_PTR;
  {$EXTERNALSYM HEVENT}
{#EndIf}


  PALLOCATOR_PROPERTIES = ^ALLOCATOR_PROPERTIES;
  _AllocatorProperties = record
    cBuffers: Longint;
    cbBuffer: Longint;
    cbAlign: Longint;
    cbPrefix: Longint;
  end;
  {$EXTERNALSYM _AllocatorProperties}
  ALLOCATOR_PROPERTIES = _AllocatorProperties;
  {$EXTERNALSYM ALLOCATOR_PROPERTIES}


  PPIN_INFO = ^PIN_INFO;
  {$EXTERNALSYM _PinInfo}
  _PinInfo = record
    pFilter: PIBaseFilter;
    dir: PIN_DIRECTION;
    achName: array[0..127] of WChar;
  end;
  {$EXTERNALSYM PIN_INFO}
  PIN_INFO = _PinInfo;



  // Interface IPin
  // ==============
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IPin);'}
  {$EXTERNALSYM IPin}
  IPin = interface(IUnknown)
    ['{56a86891-0ad4-11ce-b03a-0020af0ba770}']

      function Connect(pReceivePin: IPin;
                       const pmt: AM_MEDIA_TYPE): HResult; stdcall;

      function ReceiveConnection(pConnector: IPin;
                                 pmt: AM_MEDIA_TYPE): HResult; stdcall;

      function Disconnect: HResult; stdcall;

      function ConnectedTo(out pPin: IPin): HResult; stdcall;

      function ConnectionMediaType(out pmt: AM_MEDIA_TYPE): HResult; stdcall;

      function QueryPinInfo(out pInfo: PIN_INFO): HResult; stdcall;

      function QueryDirection(out pPinDir: PIN_DIRECTION): HResult; stdcall;

      function QueryId(out Id: LPWSTR): HResult; stdcall;

      function QueryAccept(pmt: AM_MEDIA_TYPE): HResult; stdcall;

      function EnumMediaTypes(out ppEnum: IEnumMediaTypes): HResult; stdcall;

      function QueryInternalConnections(out apPin: IPin;
                                        var nPin: ULONG): HResult; stdcall;

      function EndOfStream: HResult; stdcall;

      function BeginFlush: HResult; stdcall;

      function EndFlush: HResult; stdcall;

      function NewSegment(tStart: REFERENCE_TIME;
                          tStop: REFERENCE_TIME;
                          dRate: double): HResult; stdcall;

  end;
  IID_IPin = IPin;
  {$EXTERNALSYM IID_IPin}



  // Interface IEnumPins
  // ===================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IEnumPins);'}
  {$EXTERNALSYM IEnumPins}
  IEnumPins = interface(IUnknown)
    ['{56a86892-0ad4-11ce-b03a-0020af0ba770}']

      function Next(cPins: ULONG;
                    out ppPins: IPin;
                    pcFetched: PULONG): HResult; stdcall;

      function Skip(const cPins: ULONG): HResult; stdcall;

      function Reset: HResult; stdcall;

      function Clone(out ppEnum: IEnumPins): HResult; stdcall;

  end;
  IID_IEnumPins = IEnumPins;
  {$EXTERNALSYM IID_IEnumPins}



  // Interface IEnumMediaTypes
  // =========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IEnumMediaTypes);'}
  {$EXTERNALSYM IEnumMediaTypes}
  IEnumMediaTypes = interface(IUnknown)
    ['{89c31040-846b-11ce-97d3-00aa0055595a}']

    function Next(cMediaTypes: ULONG;
                  out ppMediaTypes: PAM_MEDIA_TYPE;
                  pcFetched: PULONG): HResult; stdcall;

    function Skip(cMediaTypes: ULONG): HResult; stdcall;

    function Reset: HResult; stdcall;

    function Clone(out ppEnum: IEnumMediaTypes): HResult; stdcall;

  end;
  IID_IEnumMediaTypes = IEnumMediaTypes;
  {$EXTERNALSYM IID_IEnumMediaTypes}



  // Interface IFilterGraph
  // ======================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IFilterGraph);'}
  {$EXTERNALSYM IFilterGraph}
  IFilterGraph = interface(IUnknown)
    ['{56A8689F-0AD4-11CE-B03A-0020AF0BA770}']

    function AddFilter(pFilter: IBaseFilter;
                       pName: LPCWSTR): HResult; stdcall;

    function RemoveFilter(pFilter: IBaseFilter): HResult; stdcall;

    function EnumFilters(out ppEnum: IEnumFilters): HResult; stdcall;

    function FindFilterByName(pName: LPCWSTR;
                              out ppFilter: IBaseFilter): HResult; stdcall;

    function ConnectDirect(ppinOut: IPin;
                           ppinIn: IPin;
                           pmt: AM_MEDIA_TYPE): HResult; stdcall;

    function Reconnect(ppin: IPin): HResult; stdcall;

    function Disconnect(ppin: IPin): HResult; stdcall;

    function SetDefaultSyncSource(): HResult; stdcall;

  end;
  IID_IFilterGraph = IFilterGraph;
  {$EXTERNALSYM IID_IFilterGraph}



  // Interface IEnumFilters
  // ======================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IEnumFilters);'}
  {$EXTERNALSYM IEnumFilters}
  IEnumFilters = interface(IUnknown)
    ['{56a86893-0ad4-11ce-b03a-0020af0ba770}']

      function Next(cFilters: ULONG;
                    out ppFilter: PIBaseFilter;
                    out pcFetched: PULONG): HResult; stdcall;

      function Skip(cFilters: ULONG): HResult; stdcall;

      function Reset: HResult; stdcall;

      function Clone(out ppEnum: IEnumFilters): HResult; stdcall;

  end;
  IID_IEnumFilters = IEnumFilters;
  {$EXTERNALSYM IID_IEnumFilters}


  PFILTER_STATE = ^FILTER_STATE;
  _FilterState    = (
    State_Stopped = 0,
    State_Paused  = (State_Stopped + 1),
    State_Running = (State_Paused + 1)
  );
  {$EXTERNALSYM _FilterState}
  FILTER_STATE = _FilterState;
  {$EXTERNALSYM FILTER_STATE}



  // Interface IMediaFilter
  // ======================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMediaFilter);'}
  {$EXTERNALSYM IMediaFilter}
  IMediaFilter = interface(IPersist)
    ['{56a86899-0ad4-11ce-b03a-0020af0ba770}']

      function Stop: HResult; stdcall;

      function Pause: HResult; stdcall;

      function Run(tStart: REFERENCE_TIME): HResult; stdcall;

      function GetState(dwMilliSecsTimeout: DWORD;
                        out State: FILTER_STATE): HResult; stdcall;

      function SetSyncSource(pClock: IReferenceClock): HResult; stdcall;

      function GetSyncSource(out pClock: IReferenceClock): HResult; stdcall;

  end;
  IID_IMediaFilter = IMediaFilter;
  {$EXTERNALSYM IID_IMediaFilter}


  PFilterInfo = ^FILTER_INFO;
  _FilterInfo = record
    achName: array[0..127] of WChar;
    pGraph: IFilterGraph;
  end;
  {$EXTERNALSYM _FilterInfo}
  FILTER_INFO = _FilterInfo;
  {$EXTERNALSYM FILTER_INFO}


  // Interface IBaseFilter
  // =====================
  // The IBaseFilter interface is the primary interface for DirectShow filters.
  // All DirectShow filters must expose this interface.
  // The Filter Graph Manager uses this interface to control filters.
  // Applications can use this interface to enumerate pins and query for filter information,
  // but should not use it to change the state of a filter.
  // Instead, use the IMediaControl interface on the Filter Graph Manager.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IBaseFilter);'}
  {$EXTERNALSYM IBaseFilter}
  IBaseFilter = interface(IMediaFilter)
    ['{56a86895-0ad4-11ce-b03a-0020af0ba770}']

      function EnumPins(out ppEnum: IEnumPins): HResult; stdcall;
      //ppEnum [out]
      //  Address of a variable that receives a pointer to the IEnumPins interface.

      function FindPin(Id: LPCWSTR;
                       out ppPin: IPin): HResult; stdcall;
      // Id [in]
      //  Pointer to a constant wide-character string that identifies the pin.
      //  Call the IPin.QueryId method to retrieve a pin's identifier.
      // ppPin [out]
      //  Address of a variable that receives a pointer to the pin's IPin interface.
      //  If the method fails, ppPin is set to NIL.

      function QueryFilterInfo(out pInfo: FILTER_INFO): HResult; stdcall;
      // pInfo [out]
      //  Pointer to a FILTER_INFO structure.

      function JoinFilterGraph(pGraph: IFilterGraph;
                               const pName: LPCWSTR): HResult; stdcall;
      // pGraph [in]
      //  Pointer to the Filter Graph Manager's IFilterGraph interface,
      //  or NIL if the filter is leaving the graph.
      // pName [in]
      //  Pointer to a wide-character string that specifies a name for the filter.
      //

      function QueryVendorInfo(out pVendorInfo: LPCWSTR): HResult; stdcall;
      // pVendorInfo [out]
      //  Address of a variable that receives a pointer to a wide-character string
      //  containing the vendor information.

  end;
  IID_IBaseFilter = IBaseFilter;
  {$EXTERNALSYM IID_IBaseFilter}


  // Interface IReferenceClock
  // =========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IReferenceClock);'}
  {$EXTERNALSYM IReferenceClock}
  IReferenceClock = interface(IUnknown)
    ['{56a86897-0ad4-11ce-b03a-0020af0ba770}']

      function GetTime(out pTime: REFERENCE_TIME): HResult; stdcall;

      function AdviseTime(baseTime: REFERENCE_TIME;
                          streamTime: REFERENCE_TIME;
                          hEvent: HEVENT;
                          out pdwAdviseCookie: DWORD_PTR): HResult; stdcall;

      function AdvisePeriodic(startTime: REFERENCE_TIME;
                              periodTime: REFERENCE_TIME;
                              hSemaphore: HSEMAPHORE;
                              out pdwAdviseCookie: DWORD_PTR): HResult; stdcall;

      function Unadvise(dwAdviseCookie: DWORD_PTR): HResult; stdcall;

  end;
  IID_IReferenceClock = IReferenceClock;
  {$EXTERNALSYM IID_IReferenceClock}


  // Interface IReferenceClockTimerControl
  // =====================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IReferenceClockTimerControl);'}
  {$EXTERNALSYM IReferenceClockTimerControl}

  IReferenceClockTimerControl = interface(IUnknown)
    ['{ebec459c-2eca-4d42-a8af-30df557614b8}']

      function SetDefaultTimerResolution(timerResolution: REFERENCE_TIME): HResult; stdcall;

      function GetDefaultTimerResolution(out pTimerResolution: REFERENCE_TIME): HResult; stdcall;

  end;


  // Interface IReferenceClock2
  // ==========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IReferenceClock2);'}
  {$EXTERNALSYM IReferenceClock2}
  IReferenceClock2 = interface(IReferenceClock)
    ['{36b73885-c2c8-11cf-8b46-00805f6cef60}']

  end;
  IID_IReferenceClockTimerControl = IReferenceClockTimerControl;
  {$EXTERNALSYM IID_IReferenceClockTimerControl}



  // Interface IMediaSample
  // ======================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMediaSample);'}
  {$EXTERNALSYM IMediaSample}
  IMediaSample = interface(IUnknown)
    ['{56a8689a-0ad4-11ce-b03a-0020af0ba770}']

      function GetPointer(out ppBuffer: PBYTE): HResult; stdcall;

      function GetSize(): Longint; stdcall;

      function GetTime(out pTimeStart: REFERENCE_TIME;
                       out pTimeEnd: REFERENCE_TIME): HResult; stdcall;

      function SetTime(pTimeStart: REFERENCE_TIME;
                       pTimeEnd: REFERENCE_TIME): HResult; stdcall;

      function IsSyncPoint(): HResult; stdcall;

      function SetSyncPoint(bIsSyncPoint: BOOL): HResult; stdcall;

      function IsPreroll(): HResult; stdcall;

      function SetPreroll(bIsPreroll: BOOL): HResult; stdcall;

      function GetActualDataLength(): Longint; stdcall;

      function SetActualDataLength(lLen: Longint): HResult; stdcall;

      function GetMediaType(out ppMediaType: PAM_MEDIA_TYPE): HResult; stdcall;

      function SetMediaType(pMediaType: AM_MEDIA_TYPE): HResult; stdcall;

      function IsDiscontinuity(): HResult; stdcall;

      function SetDiscontinuity(const bDiscontinuity: BOOL): HResult; stdcall;

      function GetMediaTime(out pTimeStart: int64;
                            out pTimeEnd: int64): HResult; stdcall;

      function SetMediaTime(pTimeStart: int64;
                            pTimeEnd: int64): HResult; stdcall;

  end;
  IID_IMediaSample = IMediaSample;
  {$EXTERNALSYM IID_IMediaSample}


  PAmSample2Properties = ^AM_SAMPLE2_PROPERTIES;
  tagAM_SAMPLE2_PROPERTIES = record
    cbData: DWORD;
    dwTypeSpecificFlags: DWORD;
    dwSampleFlags: DWORD;
    lActual: LONG;
    tStart: REFERENCE_TIME;
    tStop: REFERENCE_TIME;
    dwStreamId: DWORD;
    pMediaType: AM_MEDIA_TYPE;
    pbBuffer: PByte;
    cbBuffer: LONG;
  end;
  {$EXTERNALSYM tagAM_SAMPLE2_PROPERTIES}
  AM_SAMPLE2_PROPERTIES = tagAM_SAMPLE2_PROPERTIES;
  {$EXTERNALSYM AM_SAMPLE2_PROPERTIES}



  // Interface IMediaSample2
  // =======================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMediaSample2);'}
  {$EXTERNALSYM IMediaSample2}
  IMediaSample2 = interface(IMediaSample)
    ['{36b73884-c2c8-11cf-8b46-00805f6cef60}']

      function GetProperties(const cbProperties: DWORD;
                             out pbProperties: BYTE): HResult; stdcall;

      function SetProperties(const cbProperties: DWORD;
                             const pbProperties: BYTE): HResult; stdcall;

  end;
  IID_IMediaSample2 = IMediaSample2;
  {$EXTERNALSYM IID_IMediaSample2}



  // Interface IMediaSample2Config
  // =============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMediaSample2Config);'}
  {$EXTERNALSYM IMediaSample2Config}
  IMediaSample2Config = interface(IUnknown)
    ['{68961E68-832B-41ea-BC91-63593F3E70E3}']

      function GetSurface(out ppDirect3DSurface9: IUnknown): HResult; stdcall;

  end;
  IID_IMediaSample2Config = IMediaSample2Config;
  {$EXTERNALSYM IID_IMediaSample2Config}


  // Interface IMemAllocator
  // =======================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMemAllocator);'}
  {$EXTERNALSYM IMemAllocator}
  IMemAllocator = interface(IUnknown)
    ['{56A8689C-0AD4-11CE-B03A-0020AF0BA770}']

      function SetProperties(var pRequest: ALLOCATOR_PROPERTIES;
                             out pActual: ALLOCATOR_PROPERTIES): HResult; stdcall;

      function GetProperties(out pProps: ALLOCATOR_PROPERTIES): HResult; stdcall;

      function Commit: HResult; stdcall;

      function Decommit: HResult; stdcall;

      function GetBuffer(out ppBuffer: IMediaSample;
                         pStartTime: REFERENCE_TIME;
                         pEndTime: REFERENCE_TIME;
                         dwFlags: DWORD): HResult; stdcall;

      function ReleaseBuffer(pBuffer: IMediaSample): HResult; stdcall;

  end;
  IID_IMemAllocator = IMemAllocator;
  {$EXTERNALSYM IID_IMemAllocator}



  // Interface IMemAllocatorCallbackTemp
  // ===================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMemAllocatorCallbackTemp);'}
  {$EXTERNALSYM IMemAllocatorCallbackTemp}
  IMemAllocatorCallbackTemp = interface(IMemAllocator)
    ['{379a0cf0-c1de-11d2-abf5-00a0c905f375}']

      function SetNotify(pNotify: IMemAllocatorNotifyCallbackTemp): HResult; stdcall;

      function GetFreeCount(out plBuffersFree: LONG): HResult; stdcall;

  end;
  IID_IMemAllocatorCallbackTemp = IMemAllocatorCallbackTemp;
  {$EXTERNALSYM IID_IMemAllocatorCallbackTemp}


  // Interface IMemAllocatorNotifyCallbackTemp
  // =========================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMemAllocatorNotifyCallbackTemp);'}
  {$EXTERNALSYM IMemAllocatorNotifyCallbackTemp}
  IMemAllocatorNotifyCallbackTemp = interface(IUnknown)
    ['{92980b30-c1de-11d2-abf5-00a0c905f375}']

      function NotifyRelease(): HResult; stdcall;

  end;
  IID_IMemAllocatorNotifyCallbackTemp = IMemAllocatorNotifyCallbackTemp;
  {$EXTERNALSYM IID_IMemAllocatorNotifyCallbackTemp}


  // Interface IMemInputPin
  // ======================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMemInputPin);'}
  {$EXTERNALSYM IMemInputPin}
  IMemInputPin = interface(IUnknown)
    ['{56A8689D-0AD4-11CE-B03A-0020AF0BA770}']

      function GetAllocator(out ppAllocator: IMemAllocator): HResult; stdcall;

      function NotifyAllocator(pAllocator: IMemAllocator;
                               bReadOnly: BOOL): HResult; stdcall;

      function GetAllocatorRequirements(out pProps: ALLOCATOR_PROPERTIES): HResult; stdcall;

      function Receive(pSample: IMediaSample): HResult; stdcall;

      function ReceiveMultiple(pSamples: IMediaSample;
                               nSamples: Longint;
                               out nSamplesProcessed: Longint): HResult; stdcall;

      function ReceiveCanBlock(): HResult; stdcall;

  end;
  IID_IMemInputPin = IMemInputPin;
  {$EXTERNALSYM IID_IMemInputPin}


  // Interface IAMovieSetup
  // ======================
  // Note
  //  This interface has been deprecated.
  //  It is used by two obsolete functions, AMovieDllRegisterServer and AMovieDllUnregisterServer.
  //  These functions are now replaced by AMovieDllRegisterServer2,
  //  which does not require IAMovieSetup.
  //  However, IAMovieSetup will continue to be supported for backward compatibility with
  //  existing applications.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IAMovieSetup);'}
  {$EXTERNALSYM IAMovieSetup}
  IAMovieSetup = interface(IUnknown)
    ['{a3d8cec0-7e5a-11cf-bbc5-00805f6cef20}']

      function _Register(): HResult; stdcall;

      function Unregister(): HResult; stdcall;

  end;
  IID_IAMovieSetup = IAMovieSetup;
  {$EXTERNALSYM IID_IAMovieSetup}


  // Interface IMediaSeeking
  // =======================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMediaSeeking);'}
  {$EXTERNALSYM IMediaSeeking}
  IMediaSeeking = interface(IUnknown)
    ['{36b73880-c2c8-11cf-8b46-00805f6cef60}']

      function GetCapabilities(out pCapabilities: AM_SEEKING_SEEKING_CAPABILITIES): HResult; stdcall;

      function CheckCapabilities(var pCapabilities: AM_SEEKING_SEEKING_CAPABILITIES): HResult; stdcall;

      function IsFormatSupported(const pFormat: TGuid): HResult; stdcall;

      function QueryPreferredFormat(out pFormat: TGuid): HResult; stdcall;

      function GetTimeFormat(out pFormat: TGuid): HResult; stdcall;

      function IsUsingTimeFormat(const pFormat: TGuid): HResult; stdcall;

      function SetTimeFormat(const pFormat: TGuid): HResult; stdcall;

      function GetDuration(out pDuration: LONGLONG): HResult; stdcall;

      function GetStopPosition(out pStop: LONGLONG): HResult; stdcall;

      function GetCurrentPosition(out pCurrent: LONGLONG): HResult; stdcall;

      function ConvertTimeFormat(out pTarget: LONGLONG;
                                 const pTargetFormat: TGuid;
                                 Source: LONGLONG;
                                 const pSourceFormat: TGuid): HResult; stdcall;

      function SetPositions(var pCurrent: LONGLONG;
                            dwCurrentFlags: AM_SEEKING_SEEKING_FLAGS;
                            var pStop: LONGLONG;
                            dwStopFlags: AM_SEEKING_SEEKING_FLAGS): HResult; stdcall;

      function GetPositions(out pCurrent: LONGLONG;
                            out pStop: LONGLONG): HResult; stdcall;

      function GetAvailable(out pEarliest: LONGLONG;
                            out pLatest: LONGLONG): HResult; stdcall;

      function SetRate(const dRate: Double): HResult; stdcall;

      function GetRate(out pdRate: Double): HResult; stdcall;

      function GetPreroll(out pllPreroll: LONGLONG): HResult; stdcall;

  end;
  IID_IMediaSeeking = IMediaSeeking;
  {$EXTERNALSYM IID_IMediaSeeking}


  tagAM_MEDIAEVENT_FLAGS = (AM_MEDIAEVENT_NONOTIFY = $01);
  {$EXTERNALSYM tagAM_MEDIAEVENT_FLAGS}

  PCodecAPIEventData = ^CodecAPIEventData;
  CodecAPIEventData = record
    guid: TGUID;
    dataLength: DWORD;
    reserved: array [0..2] of DWORD;
  end;
  {$EXTERNALSYM CodecAPIEventData}



  // Interface ICodecAPI
  // ===================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ICodecAPI);'}
  {$EXTERNALSYM ICodecAPI}
  ICodecAPI = interface(IUnknown)
    ['{901db4c7-31ce-41a2-85dc-8fa0bf41b8da}']

      function IsSupported(const Api: TGUID): HResult; stdcall;

      function IsModifiable(const Api: TGUID): HResult; stdcall;

      function GetParameterRange(const Api: TGUID;
                                 out ValueMin: VARIANT;
                                 out ValueMax: VARIANT;
                                 out SteppingDelta: VARIANT): HResult; stdcall;

      function GetParameterValues(const Api: TGUID;
                                  out Values: PVARIANT;
                                  out ValuesCount: ULONG): HResult; stdcall;

      function GetDefaultValue(const Api: TGUID;
                               var Value: VARIANT): HResult; stdcall;

      function GetValue(const Api: TGUID;
                        out Value: VARIANT): HResult; stdcall;

      function SetValue(const Api: TGUID;
                        out Value: VARIANT): HResult; stdcall;

      function RegisterForEvent(const Api: TGUID;
                                const userData: LONG_PTR): HResult; stdcall;

      function UnregisterForEvent(const Api: TGUID): HResult; stdcall;

      function SetAllDefaults(): HResult; stdcall;

      function SetValueWithNotify(const Api: TGUID;
                                  Value: VARIANT;
                                  out ChangedParam: PGUID;
                                  out ChangedParamCount: ULONG): HResult; stdcall;

      function SetAllDefaultsWithNotify(out ChangedParam: PGUID;
                                        out ChangedParamCount: ULONG): HResult; stdcall;

      function GetAllSettings(__MIDL__ICodecAPI0000: IStream): HResult; stdcall;

      function SetAllSettings(__MIDL__ICodecAPI0001: IStream): HResult; stdcall;

      function SetAllSettingsWithNotify(__MIDL__ICodecAPI0002: IStream;
                                        out ChangedParam: PGUID;
                                        out ChangedParamCount: ULONG): HResult; stdcall;

  end;
  IID_ICodecAPI = ICodecAPI;
  {$EXTERNALSYM IID_ICodecAPI}


  PREGFILTER = ^REGFILTER;
  REGFILTER = record
    Clsid: CLSID;
    Name: LPWSTR;
  end;
  {$EXTERNALSYM REGFILTER}


  // Interface IEnumRegFilters
  // =========================
  // Note
  //   This interface has been deprecated.
  //   New applications should call IFilterMapper2.EnumMatchingFilters,
  //   which enumerates monikers and returns a pointer to the IEnumMoniker interface.
  //
  PIEnumRegFilters = ^IEnumRegFilters;
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IEnumRegFilters);'}
  {$EXTERNALSYM IEnumRegFilters}
  IEnumRegFilters = interface(IUnknown)
    ['{56a868a4-0ad4-11ce-b03a-0020af0ba770}']

      function Next(cFilters: ULONG;
                    out apRegFilter: REGFILTER;
                    out pcFetched: ULONG): HResult; stdcall;

      function Skip(cFilters: ULONG): HResult; stdcall;

      function Reset(): HResult; stdcall;

      function Clone(out ppEnum: IEnumRegFilters): HResult; stdcall;

  end;
  IID_IEnumRegFilters = IEnumRegFilters;
  {$EXTERNALSYM IID_IEnumRegFilters}


  // Interface IFilterMapper
  // =======================
  // Note  This interface has been deprecated.
  //   It will continue to be supported for backward compatibility with existing applications,
  //   but new applications should use the IFilterMapper2 interface.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IFilterMapper);'}
  {$EXTERNALSYM IFilterMapper}
  IFilterMapper = interface(IUnknown)
    ['{56a868a3-0ad4-11ce-b03a-0020af0ba770}']

    function RegisterFilter(const clsid: TGUID;
                            Name: LPCWSTR;
                            dwMerit: DWORD):HResult; stdcall;

    function RegisterFilterInstance(const clsid: TGUID;
                                    Name: LPCWSTR;
                                    out MRId: TGUID): HResult; stdcall;

    function RegisterPin(Filter: TGUID;
                         Name: LPCWSTR;
                         bRendered: BOOL;
                         bOutput: BOOL;
                         bZero: BOOL;
                         bMany: BOOL;
                         const ConnectsToFilter: TGUID;
                         ConnectsToPin: LPCWSTR): HResult; stdcall;

    function RegisterPinType(const clsFilter: TGUID;
                             strName: LPCWSTR;
                             const clsMajorType: TGUID;
                             const clsSubType: TGUID): HResult; stdcall;

    function UnregisterFilter(const Filter: TGUID): HResult; stdcall;

    function UnregisterFilterInstance(const MRId: TGUID): HResult; stdcall;

    function UnregisterPin(const Filter: TGUID;
                           Name: LPCWSTR): HResult; stdcall;

    function EnumMatchingFilters(out ppEnum: IEnumRegFilters; // Address of a pointer to the enumerator returned.
                                 dwMerit: DWORD; // Minimum merit value of filters to enumerate.
                                 bInputNeeded: BOOL; // Value indicating whether there must be at least one input pin; TRUE indicates at least one input pin is required.
                                 const clsInMaj: CLSID; // Input major type required. Set to GUID_NULL if you do not care.
                                 const clsInSub: CLSID; // Input subtype required. Set to GUID_NULL if you do not care.
                                 bRender: BOOL;  // Flag that specifies whether the filter must render the input; TRUE means that it must.
                                 bOututNeeded: BOOL; // Value indicating whether there must be at least one output pin; TRUE indicates at least one output pin is required.
                                 const clsOutMaj: CLSID; // Output major type required. Set to GUID_NULL if you do not care.
                                 const clsOutSub: CLSID  // Output subtype required. Set to GUID_NULL if you do not care.
                                 ): HResult; stdcall;

  end;
  IID_IFilterMapper = IFilterMapper;
  {$EXTERNALSYM IID_IFilterMapper}


  PREGPINTYPES = ^REGPINTYPES;
  REGPINTYPES = record
    clsMajorType: PGUID;
    clsMinorType: PGUID;
  end;
  {$EXTERNALSYM REGPINTYPES}


  PREGFILTERPINS = ^REGFILTERPINS;
  REGFILTERPINS = record
    strName: LPWSTR;
    bRendered: BOOL;
    bOutput: BOOL;
    bZero: BOOL;
    bMany: BOOL;
    clsConnectsToFilter: CLSID;
    strConnectsToPin: WCHAR;
    nMediaTypes: UINT;
    lpMediaType: PREGPINTYPES;
  end;
  {$EXTERNALSYM REGFILTERPINS}


  PREGPINMEDIUM = ^REGPINMEDIUM;
  REGPINMEDIUM = record
    clsMedium: TGUID;
    dw1: DWORD;
    dw2: DWORD;
  end;
  {$EXTERNALSYM REGPINMEDIUM}


  PREGFILTERPINS2 = ^REGFILTERPINS2;
  REGFILTERPINS2 = record
    dwFlags: DWORD;
    cInstances: UINT;
    nMediaTypes: UINT;
    lpMediaType: REGPINTYPES;
    nMediums: UINT;
    lpMedium: REGPINMEDIUM;
    clsPinCategory: CLSID;
  end;
  {$EXTERNALSYM REGFILTERPINS2}


  PREGFILTER2 = ^REGFILTER2;
  REGFILTER2 = record
    dwVersion: DWORD;
    dwMerit: DWORD;
    case integer  of            // union part
      0: (cPins: ULONG;
          rgPins: PREGFILTERPINS);
      1: (cPins2: ULONG;
          rgPins2: PREGFILTERPINS2);
    end;
  {$EXTERNALSYM REGFILTER2}



  // Interface IFilterMapper2
  // ========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IFilterMapper2);'}
  {$EXTERNALSYM IFilterMapper2}
  IFilterMapper2 = interface(IUnknown)
  ['{b79bb0b0-33c1-11d1-abe1-00a0c905f375}']

    function CreateCategory(const clsidCategory: TGUID;
                            dwCategoryMerit: DWORD;
                            Description: LPCWSTR): HResult; stdcall;

    function UnregisterFilter(const pclsidCategory: TGUID;
                              szInstance: LPCWSTR;
                              const Filter: TGUID): HResult; stdcall;

    function RegisterFilter(const clsidFilter: TGUID;
                            Name: LPCWSTR;
                            var ppMoniker: IMoniker;
                            const pclsidCategory: TGUID;
                            szInstance: LPCWSTR;
                            prf2: REGFILTER2): HResult; stdcall;

    function EnumMatchingFilters(out ppEnum: IEnumMoniker;
                                 const dwFlags: DWORD;
                                 bExactMatch: BOOL;
                                 dwMerit: DWORD;
                                 bInputNeeded: BOOL;
                                 cInputTypes: DWORD;
                                 pInputTypes: PGUID;
                                 pMedIn: REGPINMEDIUM;
                                 pPinCategoryIn: PGUID;
                                 bRender: BOOL;
                                 bOutputNeeded: BOOL;
                                 cOutputTypes: DWORD;
                                 pOutputTypes: PGUID;
                                 pMedOut: RegPinMedium;
                                 pPinCategoryOut: PGUID): HResult; stdcall;

  end;
  IID_IFilterMapper2 = IFilterMapper2;
  {$EXTERNALSYM IID_IFilterMapper2}



  // Interface IFilterMapper3
  // ========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IFilterMapper3);'}
  {$EXTERNALSYM IFilterMapper3}
  IFilterMapper3 = interface(IFilterMapper2)
   ['{b79bb0b0-33c1-11d1-abe1-00a0c905f375}']

      function CreateCategory(const clsidCategory: REFCLSID;
                              dwCategoryMerit: DWORD;
                              Description: LPCOLESTR): HResult; stdcall;

      function UnregisterFilter(const pclsidCategory: CLSID;
                                szInstance: LPCOLESTR;
                                const Filter: REFCLSID): HResult; stdcall;

      function RegisterFilter(const clsidFilter: REFCLSID;
                              Name: LPCOLESTR;
                              ppMoniker: IMoniker;
                              const pclsidCategory: CLSID;
                              szInstance: LPCOLESTR;
                              prf2: REGFILTER2): HResult; stdcall;

      function EnumMatchingFilters(out ppEnum: IEnumMoniker;
                                   dwFlags: DWORD;
                                   bExactMatch: BOOL;
                                   dwMerit: DWORD;
                                   bInputNeeded: BOOL;
                                   cInputTypes: DWORD;
                                   const pInputTypes: TGUID;
                                   const pMedIn: REGPINMEDIUM;
                                   const pPinCategoryIn: CLSID;
                                   bRender: BOOL;
                                   bOutputNeeded: BOOL;
                                   cOutputTypes: DWORD;
                                   const pOutputTypes: TGUID;
                                   pMedOut: REGPINMEDIUM;
                                   const pPinCategoryOut: CLSID): HResult; stdcall;

  end;
  IID_IFilterMapper3 = IFilterMapper3;
  {$EXTERNALSYM IID_IFilterMapper3}


  PQualityMessageType = ^QualityMessageType;
  tagQualityMessageType = (
    Famine,               // = 0
    Flood                 // = (Famine  + 1)
  );
  {$EXTERNALSYM tagQualityMessageType}
  QualityMessageType = tagQualityMessageType;
  {$EXTERNALSYM QualityMessageType}


  PQuality = ^Quality;
  tagQuality = record
    _Type: QualityMessageType;
    Proportion: Longint;
    Late: REFERENCE_TIME;
    TimeStamp: REFERENCE_TIME;
  end;
  {$EXTERNALSYM tagQuality}
  Quality = tagQuality;
  {$EXTERNALSYM Quality}


  // Interface IQualityControl
  // =========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IQualityControl);'}
  {$EXTERNALSYM IQualityControl}
  IQualityControl = interface(IUnknown)
    ['{56a868a5-0ad4-11ce-b03a-0020af0ba770}']

      function Notify(pSelf: IBaseFilter;
                      q: Quality): HResult; stdcall;

      function SetSink(piqc: IQualityControl): HResult; stdcall;

  end;
  IID_IQualityControl = IQualityControl;
  {$EXTERNALSYM IID_IQualityControl}


  PColorkey = ^COLORKEY;
  tagCOLORKEY = record
    KeyType: DWORD;   // Can be CK_NOCOLORKEY, CK_INDEX, or CK_RGB. CK_INDEX and CK_RGB can be logically combined.
    PaletteIndex: DWORD;
    LowColorValue: COLORREF;
    HighColorValue: COLORREF;
  end;
  {$EXTERNALSYM tagCOLORKEY}
  COLORKEY = tagCOLORKEY;
  {$EXTERNALSYM COLORKEY}


//#ifndef _WINGDI_
  PRGNDATAHEADER = ^RGNDATAHEADER;
  _RGNDATAHEADER = record
    dwSize: DWORD;
    iType: DWORD;
    nCount: DWORD;
    nRgnSize: DWORD;
    rcBound: TRect;
  end;
  {$EXTERNALSYM _RGNDATAHEADER}
  RGNDATAHEADER = _RGNDATAHEADER;
  {$EXTERNALSYM RGNDATAHEADER}



  PRGNDATA = ^RGNDATA;
  _RGNDATA = record
    rdh: RGNDATAHEADER;
    Buffer: array [0..0] of AnsiChar;
  end;
  {$EXTERNALSYM _RGNDATA}
  RGNDATA = _RGNDATA;
  {$EXTERNALSYM RGNDATA}
//#endif  //_WINGDI_



  // Interface IOverlayNotify
  // ========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IOverlayNotify);'}
  {$EXTERNALSYM IOverlayNotify}
  IOverlayNotify = interface(IUnknown)
    ['{56a868a0-0ad4-11ce-b03a-0020af0ba770}']

      function OnPaletteChange(dwColors: DWORD;
                               pPalette: PALETTEENTRY): HResult; stdcall;

      function OnClipChange(pSourceRect: TRect;
                            pDestinationRect: TRect;
                            pRgnData: RGNDATA): HResult; stdcall;

      function OnColorKeyChange(pColorKey: COLORKEY): HResult; stdcall;

      function OnPositionChange(pSourceRect: TRect;
                                pDestinationRect: TRect): HResult; stdcall;

  end;
  IID_IOverlayNotify = IOverlayNotify;
  {$EXTERNALSYM IID_IOverlayNotify}



  // Interface IOverlayNotify2
  // =========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IOverlayNotify2);'}
  {$EXTERNALSYM IOverlayNotify2}
  IOverlayNotify2 = interface(IOverlayNotify)
    ['{680EFA10-D535-11D1-87C8-00A0C9223196}']

      function OnDisplayChange(const hMonitor: HMONITOR ): HResult; stdcall;
      // Parameters
      //  hMonitor [in]
      //    Handle to the monitor used for displaying the overlay.

  end;
  IID_IOverlayNotify2 = IOverlayNotify2;
  {$EXTERNALSYM IID_IOverlayNotify2}



  // Interface IOverlay
  // ==================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IOverlay);'}
  {$EXTERNALSYM IOverlay}
  IOverlay = interface(IUnknown)
    ['{56A868A1-0AD4-11CE-B03A-0020AF0BA770}']

      function GetPalette(out pdwColors: DWORD;
                          out ppPalette: PALETTEENTRY): HResult; stdcall;

      function SetPalette(dwColors: DWORD;
                          pPalette: PALETTEENTRY): HResult; stdcall;

      function GetDefaultColorKey(out pColorKey: COLORKEY): HResult; stdcall;

      function GetColorKey(out pColorKey: COLORKEY): HResult; stdcall;

      function SetColorKey(var pColorKey: COLORKEY): HResult; stdcall;

      function GetWindowHandle(out pHwnd: HWND): HResult; stdcall;

      function GetClipList(out pSourceRect: TRect;
                           out pDestinationRect: TRect;
                           out ppRgnData: RgnData): HResult; stdcall;

      function GetVideoPosition(out pSourceRect: TRect;
                                out pDestinationRect: TRect): HResult; stdcall;

      function Advise(pOverlayNotify: IOverlayNotify;
                      dwInterests: DWORD): HResult; stdcall;

      function Unadvise(): HResult; stdcall;

  end;
  IID_IOverlay = IOverlay;
  {$EXTERNALSYM IID_IOverlay}



  // Interface IMediaEventSink
  // =========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMediaEventSink);'}
  {$EXTERNALSYM IMediaEventSink}
  IMediaEventSink = interface(IUnknown)
    ['{56a868a2-0ad4-11ce-b03a-0020af0ba770}']

      function Notify(EventCode: LONG;
                      EventParam1: LONG_PTR;
                      EventParam2: LONG_PTR): HResult; stdcall;

  end;
  IID_IMediaEventSink = IMediaEventSink;
  {$EXTERNALSYM IID_IMediaEventSink}


  // Interface IFileSourceFilter
  // ===========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IFileSourceFilter);'}
  {$EXTERNALSYM IFileSourceFilter}
  IFileSourceFilter = interface(IUnknown)
    ['{56a868a6-0ad4-11ce-b03a-0020af0ba770}']

      function Load(pszFileName: LPCOLESTR;
                    pmt: AM_MEDIA_TYPE): HResult; stdcall;

      function GetCurFile(out ppszFileName: LPCOLESTR;
                          out pmt: AM_MEDIA_TYPE): HResult; stdcall;

  end;
  IID_IFileSourceFilter = IFileSourceFilter;
  {$EXTERNALSYM IID_IFileSourceFilter}



  // Interface IFileSinkFilter
  // =========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IFileSinkFilter);'}
  {$EXTERNALSYM IFileSinkFilter}
  IFileSinkFilter = interface(IUnknown)
    ['{a2104830-7c70-11cf-8bce-00aa00a3f1a6}']

      function SetFileName(pszFileName: LPCOLESTR;
                           pmt: AM_MEDIA_TYPE): HResult; stdcall;

      function GetCurFile(out ppszFileName: LPCOLESTR;
                          out pmt: AM_MEDIA_TYPE): HResult; stdcall;

  end;
  IID_IFileSinkFilter = IFileSinkFilter;
  {$EXTERNALSYM IID_IFileSinkFilter}



  // Interface IFileSinkFilter2
  // ==========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IFileSinkFilter2);'}
  {$EXTERNALSYM IFileSinkFilter2}
  IFileSinkFilter2 = interface(IFileSinkFilter)
    ['{00855B90-CE1B-11D0-BD4F-00A0C911CE86}']

      function SetMode(dwFlags: DWORD): HResult; stdcall;

      function GetMode(out pdwFlags: DWORD): HResult; stdcall;

  end;
  IID_IFileSinkFilter2 = IFileSinkFilter2;
  {$EXTERNALSYM IID_IFileSinkFilter2}


  AM_FILESINK_FLAGS = (
    AM_FILE_OVERWRITE = 1
  );
  {$EXTERNALSYM AM_FILESINK_FLAGS}


  // Interface IGraphBuilder
  // =======================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IGraphBuilder);'}
  {$EXTERNALSYM IGraphBuilder}
  IGraphBuilder = interface(IFilterGraph)
    ['{56a868a9-0ad4-11ce-b03a-0020af0ba770}']

      function Connect(ppinOut: IPin;
                       ppinIn: IPin): HResult; stdcall;

      function Render(ppinOut: IPin): HResult; stdcall;

      function RenderFile(lpcwstrFile: LPCWSTR;
                          lpcwstrPlayList: LPCWSTR): HResult; stdcall;

      function AddSourceFilter(lpcwstrFileName: LPCWSTR;
                               lpcwstrFilterName: LPCWSTR;
                               out ppFilter: IBaseFilter): HResult; stdcall;

      function SetLogFile(hFile: THandle): HResult; stdcall;

      function Abort(): HResult; stdcall;

      function ShouldOperationContinue(): HResult; stdcall;

  end;
  IID_IGraphBuilder = IGraphBuilder;
  {$EXTERNALSYM IID_IGraphBuilder}


  // Interface ICaptureGraphBuilder
  // ==============================
  // Note
  //  This interface has been deprecated.
  //  It will continue to be supported for backward compatibility with existing applications,
  //  but new applications should use the ICaptureGraphBuilder2 interface.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ICaptureGraphBuilder);'}
  {$EXTERNALSYM ICaptureGraphBuilder}
  ICaptureGraphBuilder = interface(IUnknown)
    ['{bf87b6e0-8c27-11d0-b3f0-00aa003761c5}']

      function SetFiltergraph(pfg: IGraphBuilder): HResult; stdcall;

      function GetFiltergraph(out ppfg: IGraphBuilder): HResult; stdcall;

      function SetOutputFileName(pType: TGUID;
                                 lpstrFile: PWCHAR;
                                 out ppf: IBaseFilter;
                                 out ppSink: IFileSinkFilter): HResult; stdcall;

      function FindInterface(pCategory: PGUID;
                             pf: IBaseFilter;
                             const riid: TGUID;
                             out ppint {IUnknown} ): HResult; stdcall;

      function RenderStream(pCategory: PGUID;
                            pSource: IUnknown;
                            pfCompressor: IBaseFilter;
                            pfRenderer: IBaseFilter): HResult; stdcall;

      function ControlStream(pCategory: PGUID;
                             pFilter: IBaseFilter;
                             pstart: REFERENCE_TIME;
                             pstop: PReferenceTime;
                             wStartCookie: WORD;
                             wStopCookie: WORD): HResult; stdcall;

      function AllocCapFile(lpstr: PWCHAR;
                            dwlSize: int64): HResult; stdcall;

      function CopyCaptureFile(lpwstrOld: PWCHAR;
                               lpwstrNew: PWCHAR;
                               fAllowEscAbort: Integer;
                               pCallback: IAMCopyCaptureFileProgress): HResult; stdcall;

  end;
  IID_ICaptureGraphBuilder = ICaptureGraphBuilder;
  {$EXTERNALSYM IID_ICaptureGraphBuilder}



  // Interface IAMCopyCaptureFileProgress
  // ====================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IAMCopyCaptureFileProgress);'}
  {$EXTERNALSYM IAMCopyCaptureFileProgress}
  IAMCopyCaptureFileProgress = interface(IUnknown)
    ['{670d1d20-a068-11d0-b3f0-00aa003761c5}']

      function Progress(iProgress: INT): HResult; stdcall;

  end;
  IID_IAMCopyCaptureFileProgress = IAMCopyCaptureFileProgress;
  {$EXTERNALSYM IID_IAMCopyCaptureFileProgress}



  // Interface ICaptureGraphBuilder2
  // ===============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ICaptureGraphBuilder2);'}
  {$EXTERNALSYM ICaptureGraphBuilder2}
  ICaptureGraphBuilder2 = interface(IUnknown)
    ['{93E5A4E0-2D50-11d2-ABFA-00A0C9C6E38D}']

      function SetFiltergraph(pfg: IGraphBuilder): HResult; stdcall;

      function GetFiltergraph(out ppfg: IGraphBuilder): HResult; stdcall;

      function SetOutputFileName(const pType: TGUID;
                                 lpstrFile: LPCOLESTR;
                                 out ppf: IBaseFilter;
                                 out ppSink: IFileSinkFilter): HResult; stdcall;

      function FindInterface(const pCategory: TGUID;
                             const pType: TGUID;
                             pf: IBaseFilter;
                             out riid: REFIID;
                             out ppint): HResult; stdcall;

      function RenderStream(const pCategory: TGUID;
                            const pType: TGUID;
                            pSource: IUnknown;
                            pfCompressor: IBaseFilter;
                            pfRenderer: IBaseFilter): HResult; stdcall;

      function ControlStream(const pCategory: TGUID;
                             const pType: TGUID;
                             pFilter: IBaseFilter;
                             pstart: REFERENCE_TIME;
                             pstop: REFERENCE_TIME;
                             wStartCookie: WORD;
                             wStopCookie: WORD): HResult; stdcall;

      function AllocCapFile(lpstr: LPCOLESTR;
                            dwlSize: DWORDLONG): HResult; stdcall;

      function CopyCaptureFile(lpwstrOld: LPOLESTR;
                               lpwstrNew: LPOLESTR;
                               fAllowEscAbort: Integer;
                               pCallback: IAMCopyCaptureFileProgress): HResult; stdcall;

      function FindPin(pSource: IUnknown;
                       pindir: PIN_DIRECTION;
                       const pCategory: TGUID;
                       const pType: TGUID;
                       fUnconnected: BOOL;
                       num: Integer;
                       out ppPin: IPin): HResult; stdcall;

    end;
  IID_ICaptureGraphBuilder2 = ICaptureGraphBuilder2;
  {$EXTERNALSYM IID_ICaptureGraphBuilder2}


  _AM_RENSDEREXFLAGS = (
    AM_RENDEREX_RENDERTOEXISTINGRENDERERS = $01
  );
  {$EXTERNALSYM _AM_RENSDEREXFLAGS}



  // Interface IFilterGraph2
  // =======================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IFilterGraph2);'}
  {$EXTERNALSYM IFilterGraph2}
  IFilterGraph2 = interface(IGraphBuilder)
    ['{36b73882-c2c8-11cf-8b46-00805f6cef60}']

      function AddSourceFilterForMoniker(pMoniker: IMoniker;
                                         pCtx: IBindCtx;
                                         lpcwstrFilterName: LPCWSTR;
                                         out ppFilter: IBaseFilter): HResult; stdcall;

      function ReconnectEx(ppin: IPin;
                           pmt: PAM_MEDIA_TYPE): HResult; stdcall;

      function RenderEx(pPinOut: IPin;
                        dwFlags: DWORD;
                        var pvContext: DWORD): HResult; stdcall;

  end;
  IID_IFilterGraph2 = IFilterGraph2;
  {$EXTERNALSYM IID_IFilterGraph2}



  // Interface IFilterGraph3
  // =======================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IFilterGraph3);'}
  {$EXTERNALSYM IFilterGraph3}
  IFilterGraph3 = interface(IFilterGraph2)
    ['{aaf38154-b80b-422f-91e6-b66467509a07}']

      function SetSyncSourceEx(pClockForMostOfFilterGraph: IReferenceClock;
                               pClockForFilter: IReferenceClock;
                               pFilter: IBaseFilter): HResult; stdcall;


  end;
  IID_IFilterGraph3 = IFilterGraph3;
  {$EXTERNALSYM IID_IFilterGraph3}



  // Interface IStreamBuilder
  // ========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IStreamBuilder);'}
  {$EXTERNALSYM IStreamBuilder}
  IStreamBuilder = interface(IUnknown)
    ['{56a868bf-0ad4-11ce-b03a-0020af0ba770}']
      function Render(ppinOut: IPin;
                      pGraph: IGraphBuilder): HResult; stdcall;

      function Backout(ppinOut: IPin;
                       pGraph: IGraphBuilder): HResult; stdcall;

  end;
  IID_IStreamBuilder = IStreamBuilder;
  {$EXTERNALSYM IID_IStreamBuilder}



  // Interface IAsyncReader
  // ======================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IAsyncReader);'}
  {$EXTERNALSYM IAsyncReader}
  IAsyncReader = interface(IUnknown)
    ['{56A868AA-0AD4-11CE-B03A-0020AF0BA770}']

      function RequestAllocator(pPreferred: IMemAllocator;
                                pProps: ALLOCATOR_PROPERTIES;
                                out ppActual: IMemAllocator): HResult; stdcall;

      function Request(pSample: IMediaSample;
                       dwUser: DWORD): HResult; stdcall;

      function WaitForNext(dwTimeout: DWORD;
                           out ppSample: IMediaSample;
                           out pdwUser: DWORD): HResult; stdcall;

      function SyncReadAligned(pSample: IMediaSample): HResult; stdcall;

      function SyncRead(llPosition: LONGLONG;
                        lLength: LONG;
                        pBuffer: Byte): HResult; stdcall;

      function Length(out pTotal: LONGLONG;
                      pAvailable: LONGLONG): HResult; stdcall;

      function BeginFlush(): HResult; stdcall;

      function EndFlush(): HResult; stdcall;

  end;
  IID_IAsyncReader = IAsyncReader;
  {$EXTERNALSYM IID_IAsyncReader}



  // Interface IGraphVersion
  // =======================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IGraphVersion);'}
  {$EXTERNALSYM IGraphVersion}
  IGraphVersion = interface(IUnknown)
    ['{56a868ab-0ad4-11ce-b03a-0020af0ba770}']

      function QueryVersion(out pVersion: LONG): HResult; stdcall;

  end;
  IID_IGraphVersion = IGraphVersion;
  {$EXTERNALSYM IID_IGraphVersion}


  // Interface IResourceConsumer
  // ===========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IResourceConsumer);'}
  {$EXTERNALSYM IResourceConsumer}
  IResourceConsumer = interface(IUnknown)
    ['{56a868ad-0ad4-11ce-b03a-0020af0ba770}']

      function AcquireResource(idResource: LONG): HResult; stdcall;

      function ReleaseResource(idResource: LONG): HResult; stdcall;

  end;
  IID_IResourceConsumer = IResourceConsumer;
  {$EXTERNALSYM IID_IResourceConsumer}



  // Interface IResourceManager
  // ==========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IResourceManager);'}
  {$EXTERNALSYM IResourceManager}
  IResourceManager = interface(IUnknown)
    ['{56a868ac-0ad4-11ce-b03a-0020af0ba770}']

      function Register(pName: LPCWSTR;
                        cResource: LONG;
                        out plToken: LONG): HResult; stdcall;

      function RegisterGroup(pName: LPCWSTR;
                             cResource: Longint;
                             palTokens: LONG;
                             out plToken: LONG): HResult; stdcall;

      function RequestResource(idResource: Longint;
                               pFocusObject: IUnknown;
                               pConsumer: IResourceConsumer): HResult; stdcall;

      function NotifyAcquire(idResource: Longint;
                             pConsumer: IResourceConsumer;
                             const hr: HResult): HResult; stdcall;

      function NotifyRelease(idResource: Longint;
                             pConsumer: IResourceConsumer;
                             bStillWant: BOOL): HResult; stdcall;

      function CancelRequest(idResource: Longint;
                             pConsumer: IResourceConsumer): HResult; stdcall;

      function SetFocus(pFocusObject: IUnknown): HResult; stdcall;

      function ReleaseFocus(pFocusObject: IUnknown): HResult; stdcall;

  end;
  IID_IResourceManager = IResourceManager;
  {$EXTERNALSYM IID_IResourceManager}



  // Interface IDistributorNotify
  // ============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDistributorNotify);'}
  {$EXTERNALSYM IDistributorNotify}
  IDistributorNotify = interface(IUnknown)
    ['{56a868af-0ad4-11ce-b03a-0020af0ba770}']

      function Stop(): HResult; stdcall;

      function Pause(): HResult; stdcall;

      function Run(tStart: REFERENCE_TIME): HResult; stdcall;

      function SetSyncSource(pClock: IReferenceClock): HResult; stdcall;

      function NotifyGraphChange(): HResult; stdcall;

  end;
  IID_IDistributorNotify = IDistributorNotify;
  {$EXTERNALSYM IID_IDistributorNotify}


  PAM_STREAM_INFO = ^AM_STREAM_INFO;
  AM_STREAM_INFO = record
    tStart: REFERENCE_TIME;
    tStop: REFERENCE_TIME;
    dwStartCookie: DWORD;
    dwStopCookie: DWORD;
    dwFlags: DWORD;
  end;
  {$EXTERNALSYM AM_STREAM_INFO}



  // Interface IAMStreamControl
  // ==========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IAMStreamControl);'}
  {$EXTERNALSYM IAMStreamControl}
  IAMStreamControl = interface(IUnknown)
    ['{36b73881-c2c8-11cf-8b46-00805f6cef60}']

      function StartAt(ptStart: REFERENCE_TIME;
                       dwCookie: DWORD): HResult; stdcall;

      function StopAt(ptStop: REFERENCE_TIME;
                      bSendExtra: BOOL;
                      dwCookie: DWORD): HResult; stdcall;

      function GetInfo(out pInfo: AM_STREAM_INFO): HResult; stdcall;

  end;
  IID_IAMStreamControl = IAMStreamControl;
  {$EXTERNALSYM IID_IAMStreamControl}



  // Interface ISeekingPassThru
  // ==========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ISeekingPassThru);'}
  {$EXTERNALSYM ISeekingPassThru}
  ISeekingPassThru = interface(IUnknown)
    ['{36b73883-c2c8-11cf-8b46-00805f6cef60}']

    function Init(bSupportRendering: BOOL;
                  pPin: IPin): HResult; stdcall;

  end;
  IID_ISeekingPassThru = ISeekingPassThru;
  {$EXTERNALSYM IID_ISeekingPassThru}


  PVIDEO_STREAM_CONFIG_CAPS = ^VIDEO_STREAM_CONFIG_CAPS;
  _VIDEO_STREAM_CONFIG_CAPS = record
    guid: TGUID;
    VideoStandard: ULONG;
    InputSize: SIZE;
    MinCroppingSize: SIZE;
    MaxCroppingSize: SIZE;
    CropGranularityX: Integer;
    CropGranularityY: Integer;
    CropAlignX: Integer;
    CropAlignY: Integer;
    MinOutputSize: SIZE;
    MaxOutputSize: SIZE;
    OutputGranularityX: Integer;
    OutputGranularityY: Integer;
    StretchTapsX: Integer;
    StretchTapsY: Integer;
    ShrinkTapsX: Integer;
    ShrinkTapsY: Integer;
    MinFrameInterval: LONGLONG;
    MaxFrameInterval: LONGLONG;
    MinBitsPerSecond: LONG;
    MaxBitsPerSecond: LONG;
  end;
  {$EXTERNALSYM _VIDEO_STREAM_CONFIG_CAPS}
  VIDEO_STREAM_CONFIG_CAPS = _VIDEO_STREAM_CONFIG_CAPS;
  {$EXTERNALSYM VIDEO_STREAM_CONFIG_CAPS}


  PAUDIO_STREAM_CONFIG_CAPS = ^AUDIO_STREAM_CONFIG_CAPS;
  _AUDIO_STREAM_CONFIG_CAPS = record
    guid: TGUID;
    MinimumChannels: ULONG;
    MaximumChannels: ULONG;
    ChannelsGranularity: ULONG;
    MinimumBitsPerSample: ULONG;
    MaximumBitsPerSample: ULONG;
    BitsPerSampleGranularity: ULONG;
    MinimumSampleFrequency: ULONG;
    MaximumSampleFrequency: ULONG;
    SampleFrequencyGranularity: ULONG;
  end;
  {$EXTERNALSYM _AUDIO_STREAM_CONFIG_CAPS}
  AUDIO_STREAM_CONFIG_CAPS = _AUDIO_STREAM_CONFIG_CAPS;
  {$EXTERNALSYM AUDIO_STREAM_CONFIG_CAPS}


  // Interface IAMStreamConfig
  // =========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IAMStreamConfig);'}
  {$EXTERNALSYM IAMStreamConfig}
  IAMStreamConfig = interface(IUnknown)
    ['{C6E13340-30AC-11d0-A18C-00A0C9118956}']

      function SetFormat(const pmt: AM_MEDIA_TYPE): HResult; stdcall;

      function GetFormat(out ppmt: PAM_MEDIA_TYPE): HResult; stdcall;

      function GetNumberOfCapabilities(out piCount: INT;
                                       out piSize: INT): HResult; stdcall;

      function GetStreamCaps(iIndex: INT;
                             out ppmt: PAM_MEDIA_TYPE;
                             out pSCC: Byte): HResult; stdcall;

  end;
  IID_IAMStreamConfig = IAMStreamConfig;
  {$EXTERNALSYM IID_IAMStreamConfig}


  PIterleavingMode = ^InterleavingMode;
  InterleavingMode = (
    INTERLEAVE_NONE           = 0,
    INTERLEAVE_CAPTURE       = (INTERLEAVE_NONE + 1) ,
    INTERLEAVE_FULL           = (INTERLEAVE_CAPTURE + 1) ,
    INTERLEAVE_NONE_BUFFERED = (INTERLEAVE_FULL + 1)
  );
  {$EXTERNALSYM InterleavingMode}



  // Interface IConfigInterleaving
  // =============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IConfigInterleaving);'}
  {$EXTERNALSYM IConfigInterleaving}
  IConfigInterleaving = interface(IUnknown)
    ['{BEE3D220-157B-11d0-BD23-00A0C911CE86}']

      function put_Mode(mode: InterleavingMode): HResult; stdcall;

      function get_Mode(out pMode: InterleavingMode): HResult; stdcall;

      function put_Interleaving(prtInterleave: REFERENCE_TIME;
                                prtPreroll: REFERENCE_TIME): HResult; stdcall;

      function get_Interleaving(out prtInterleave: REFERENCE_TIME;
                                prtPreroll: REFERENCE_TIME): HResult; stdcall;

  end;
  IID_IConfigInterleaving = IConfigInterleaving;
  {$EXTERNALSYM IID_IConfigInterleaving}


  // Interface IConfigAviMux
  // =======================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IConfigAviMux);'}
  {$EXTERNALSYM IConfigAviMux}
  IConfigAviMux = interface(IUnknown)
    ['{5ACD6AA0-F482-11ce-8B67-00AA00A3F1A6}']

      function SetMasterStream(iStream: LONG): HResult; stdcall;

      function GetMasterStream(out pStream: LONG): HResult; stdcall;

      function SetOutputCompatibilityIndex(fOldIndex: BOOL): HResult; stdcall;

      function GetOutputCompatibilityIndex(out pfOldIndex: BOOL): HResult; stdcall;

  end;
  IID_IConfigAviMux = IConfigAviMux;
  {$EXTERNALSYM IID_IConfigAviMux}


  PCompressionCaps = ^CompressionCaps;
  CompressionCaps = (
    CompressionCaps_CanQuality   = $1,
    CompressionCaps_CanCrunch    = $2,
    CompressionCaps_CanKeyFrame  = $4,
    CompressionCaps_CanBFrame    = $8,
    CompressionCaps_CanWindow    = $10
  );
  {$EXTERNALSYM CompressionCaps}


  // Interface IAMVideoCompression
  // =============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IAMVideoCompression);'}
  {$EXTERNALSYM IAMVideoCompression}
  IAMVideoCompression = interface(IUnknown)
    ['{C6E13343-30AC-11d0-A18C-00A0C9118956}']

      function put_KeyFrameRate(KeyFrameRate: LONG): HResult; stdcall;

      function get_KeyFrameRate(out pKeyFrameRate: LONG): HResult; stdcall;

      function put_PFramesPerKeyFrame(PFramesPerKeyFrame: LONG): HResult; stdcall;

      function get_PFramesPerKeyFrame(out pPFramesPerKeyFrame: LONG): HResult; stdcall;

      function put_Quality(Quality: double): HResult; stdcall;

      function get_Quality(out pQuality: double): HResult; stdcall;

      function put_WindowSize(WindowSize: LONG): HResult; stdcall;

      function get_WindowSize(out pWindowSize: LONG): HResult; stdcall;

      function GetInfo(pszVersion: LPWSTR;
                       var pcbVersion: INT;
                       pszDescription: LPWSTR;
                       var pcbDescription: INT;
                       out pDefaultKeyFrameRate: LONG;
                       pDefaultPFramesPerKey: LONG;
                       out pDefaultQuality: Double;
                       out pCapabilities: LONG): HResult; stdcall;

      function OverrideKeyFrame(FrameNumber: LONG): HResult; stdcall;

      function OverrideFrameSize(FrameNumber: LONG;
                                 Size: LONG): HResult; stdcall;

  end;
  IID_IAMVideoCompression = IAMVideoCompression;
  {$EXTERNALSYM IID_IAMVideoCompression}


  PVfwCaptureDialogs = ^VfwCaptureDialogs;
  VfwCaptureDialogs = (
    VfwCaptureDialog_Source  = $1,
    VfwCaptureDialog_Format  = $2,
    VfwCaptureDialog_Display = $4
  );
  {$EXTERNALSYM VfwCaptureDialogs}

  PVfwCompressDialogs = ^VfwCompressDialogs;
  VfwCompressDialogs = (
    VfwCompressDialog_Config      = $1,
    VfwCompressDialog_About       = $2,
    VfwCompressDialog_QueryConfig = $4,
    VfwCompressDialog_QueryAbout  = $8
  );
  {$EXTERNALSYM VfwCompressDialogs}


  // Interface IAMVfwCaptureDialogs
  // ==============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IAMVfwCaptureDialogs);'}
  {$EXTERNALSYM IAMVfwCaptureDialogs}
  IAMVfwCaptureDialogs = interface(IUnknown)
    ['{D8D715A0-6E5E-11D0-B3F0-00AA003761C5}']

      function HasDialog(iDialog: INT): HResult; stdcall;

      function ShowDialog(iDialog: INT;
                          hwnd: HWND): HResult; stdcall;

      function SendDriverMessage(iDialog: INT;
                                 uMsg: INT;
                                 dw1: LONG;
                                 dw2: LONG): HResult; stdcall;

  end;
  IID_IAMVfwCaptureDialogs = IAMVfwCaptureDialogs;
  {$EXTERNALSYM IID_IAMVfwCaptureDialogs}



  // Interface IAMVfwCompressDialogs
  // ===============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IAMVfwCompressDialogs);'}
  {$EXTERNALSYM IAMVfwCompressDialogs}
  IAMVfwCompressDialogs = interface(IUnknown)
    ['{D8D715A3-6E5E-11D0-B3F0-00AA003761C5}']

    function ShowDialog(iDialog: INT;
                        hwnd: HWND): HResult; stdcall;

    function GetState(out pState: LPVOID;
                      var pcbState: INT): HResult; stdcall;

    function SetState(pState: LPVOID;
                      cbState: INT): HResult; stdcall;

    function SendDriverMessage(uMsg: INT;
                               dw1: LONG;
                               dw2: LONG): HResult; stdcall;

  end;
  IID_IAMVfwCompressDialogs = IAMVfwCompressDialogs;
  {$EXTERNALSYM IID_IAMVfwCompressDialogs}



  // Interface IAMDroppedFrames
  // ==========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IAMDroppedFrames);'}
  {$EXTERNALSYM IAMDroppedFrames}
  IAMDroppedFrames = interface(IUnknown)
    ['{C6E13344-30AC-11d0-A18C-00A0C9118956}']

      function GetNumDropped(out plDropped: LONG): HResult; stdcall;

      function GetNumNotDropped(out plNotDropped: LONG): HResult; stdcall;

      function GetDroppedInfo(lSize: LONG;
                              out plArray: LONG;
                              out plNumCopied: LONG): HResult; stdcall;

      function GetAverageFrameSize(out plAverageSize: LONG): HResult; stdcall;

  end;
  IID_IAMDroppedFrames = IAMDroppedFrames;
  {$EXTERNALSYM IID_IAMDroppedFrames}



  // Interface IAMAudioInputMixer
  // ============================
  // The IAMAudioInputMixer interface controls audio capture properties,
  // such as panning and loudness; and enables or disables specific audio inputs,
  // such as the line in or the microphone.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IAMAudioInputMixer);'}
  {$EXTERNALSYM IAMAudioInputMixer}
  IAMAudioInputMixer = interface(IUnknown)
    ['{54C39221-8380-11d0-B3F0-00AA003761C5}']

      function put_Enable(fEnable: BOOL): HResult; stdcall;

      function get_Enable(out pfEnable: BOOL): HResult; stdcall;

      function put_Mono(fMono: BOOL): HResult; stdcall;

      function get_Mono(out pfMono: BOOL): HResult; stdcall;

      function put_MixLevel(Level: double): HResult; stdcall;

      function get_MixLevel(out pLevel: double): HResult; stdcall;

      function put_Pan(Pan: double): HResult; stdcall;

      function get_Pan(out pPan: double): HResult; stdcall;

      function put_Loudness(fLoudness: BOOL): HResult; stdcall;

      function get_Loudness(out pfLoudness: BOOL): HResult; stdcall;

      function put_Treble(Treble: double): HResult; stdcall;

      function get_Treble(out pTreble: double): HResult; stdcall;

      function get_TrebleRange(out pRange: double): HResult; stdcall;

      function put_Bass(Bass: double): HResult; stdcall;

      function get_Bass(out pBass: double): HResult; stdcall;

      function get_BassRange(out pRange: double): HResult; stdcall;

  end;
  IID_IAMAudioInputMixer = IAMAudioInputMixer;
  {$EXTERNALSYM IID_IAMAudioInputMixer}



  // Interface IAMBufferNegotiation
  // ==============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IAMBufferNegotiation);'}
  {$EXTERNALSYM IAMBufferNegotiation}
  IAMBufferNegotiation = interface(IUnknown)
    ['{56ED71A0-AF5F-11D0-B3F0-00AA003761C5}']

    function SuggestAllocatorProperties(pprop: ALLOCATOR_PROPERTIES): HResult; stdcall;

    function GetAllocatorProperties(out pprop: ALLOCATOR_PROPERTIES): HResult; stdcall;

  end;
  IID_IAMBufferNegotiation = IAMBufferNegotiation;
  {$EXTERNALSYM IID_IAMBufferNegotiation}


  PTunerInputType = ^TunerInputType;
  tagTunerInputType = (
    TunerInputCable = 0,
    TunerInputAntenna = (TunerInputCable + 1)
  );
  {$EXTERNALSYM tagTunerInputType}
  TunerInputType = tagTunerInputType;
  {$EXTERNALSYM TunerInputType}


  PVideoCopyProtectionType = ^VideoCopyProtectionType;
  VideoCopyProtectionType               = (
    VideoCopyProtectionMacrovisionBasic = 0,
    VideoCopyProtectionMacrovisionCBI   = (VideoCopyProtectionMacrovisionBasic + 1)
  );
  {$EXTERNALSYM VideoCopyProtectionType}


  PPhysicalConnectorType = ^PhysicalConnectorType;
  tagPhysicalConnectorType         = (
    PhysConn_Video_Tuner           = 1,
    PhysConn_Video_Composite       = (PhysConn_Video_Tuner + 1),
    PhysConn_Video_SVideo          = (PhysConn_Video_Composite + 1),
    PhysConn_Video_RGB             = (PhysConn_Video_SVideo + 1),
    PhysConn_Video_YRYBY           = (PhysConn_Video_RGB + 1),
    PhysConn_Video_SerialDigital   = (PhysConn_Video_YRYBY + 1),
    PhysConn_Video_ParallelDigital = (PhysConn_Video_SerialDigital + 1),
    PhysConn_Video_SCSI            = (PhysConn_Video_ParallelDigital + 1),
    PhysConn_Video_AUX             = (PhysConn_Video_SCSI + 1),
    PhysConn_Video_1394            = (PhysConn_Video_AUX + 1),
    PhysConn_Video_USB             = (PhysConn_Video_1394 + 1),
    PhysConn_Video_VideoDecoder    = (PhysConn_Video_USB + 1),
    PhysConn_Video_VideoEncoder    = (PhysConn_Video_VideoDecoder + 1),
    PhysConn_Video_SCART           = (PhysConn_Video_VideoEncoder + 1),
    PhysConn_Video_Black           = (PhysConn_Video_SCART + 1),
    PhysConn_Audio_Tuner           = $1000,
    PhysConn_Audio_Line            = (PhysConn_Audio_Tuner + 1),
    PhysConn_Audio_Mic             = (PhysConn_Audio_Line + 1),
    PhysConn_Audio_AESDigital      = (PhysConn_Audio_Mic + 1),
    PhysConn_Audio_SPDIFDigital    = (PhysConn_Audio_AESDigital + 1),
    PhysConn_Audio_SCSI            = (PhysConn_Audio_SPDIFDigital + 1),
    PhysConn_Audio_AUX             = (PhysConn_Audio_SCSI + 1),
    PhysConn_Audio_1394            = (PhysConn_Audio_AUX + 1),
    PhysConn_Audio_USB             = (PhysConn_Audio_1394 + 1),
    PhysConn_Audio_AudioDecoder    = (PhysConn_Audio_USB + 1)
  );
  {$EXTERNALSYM tagPhysicalConnectorType}
  PhysicalConnectorType = tagPhysicalConnectorType;
  {$EXTERNALSYM PhysicalConnectorType}



  // Interface IAMAnalogVideoDecoder
  // ===============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IAMAnalogVideoDecoder);'}
  {$EXTERNALSYM IAMAnalogVideoDecoder}
  IAMAnalogVideoDecoder = interface(IUnknown)
    ['{C6E13350-30AC-11d0-A18C-00A0C9118956}']

      function get_AvailableTVFormats(out lAnalogVideoStandard: LONG): HResult; stdcall;

      function put_TVFormat(lAnalogVideoStandard: LONG): HResult; stdcall;

      function get_TVFormat(out plAnalogVideoStandard: LONG): HResult; stdcall;

      function get_HorizontalLocked(out plLocked: LONG): HResult; stdcall;

      function put_VCRHorizontalLocking(lVCRHorizontalLocking: LONG): HResult; stdcall;

      function get_VCRHorizontalLocking(out plVCRHorizontalLocking: LONG): HResult; stdcall;

      function get_NumberOfLines(out plNumberOfLines: LONG): HResult; stdcall;

      function put_OutputEnable(lOutputEnable: LONG): HResult; stdcall;

      function get_OutputEnable(out plOutputEnable: LONG): HResult; stdcall;

  end;
  IID_IAMAnalogVideoDecoder = IAMAnalogVideoDecoder;
  {$EXTERNALSYM IID_IAMAnalogVideoDecoder}


  PVideoProcAmpProperty = ^VideoProcAmpProperty;
  tagVideoProcAmpProperty              = (
    VideoProcAmp_Brightness            = 0,
    VideoProcAmp_Contrast              = (VideoProcAmp_Brightness + 1),
    VideoProcAmp_Hue                   = (VideoProcAmp_Contrast + 1),
    VideoProcAmp_Saturation            = (VideoProcAmp_Hue + 1),
    VideoProcAmp_Sharpness             = (VideoProcAmp_Saturation + 1),
    VideoProcAmp_Gamma                 = (VideoProcAmp_Sharpness + 1),
    VideoProcAmp_ColorEnable           = (VideoProcAmp_Gamma + 1),
    VideoProcAmp_WhiteBalance          = (VideoProcAmp_ColorEnable + 1),
    VideoProcAmp_BacklightCompensation = (VideoProcAmp_WhiteBalance + 1),
    VideoProcAmp_Gain                  = (VideoProcAmp_BacklightCompensation + 1));
  {$EXTERNALSYM tagVideoProcAmpProperty}
  VideoProcAmpProperty = tagVideoProcAmpProperty;
  {$EXTERNALSYM VideoProcAmpProperty}


  PVideoProcAmpFlags = ^VideoProcAmpFlags;
  tagVideoProcAmpFlags        = (
    VideoProcAmp_Flags_Auto   = $1,
    VideoProcAmp_Flags_Manual = $2
  );
  {$EXTERNALSYM tagVideoProcAmpFlags}
  VideoProcAmpFlags = tagVideoProcAmpFlags;
  {$EXTERNALSYM VideoProcAmpFlags}


  // Interface IAMVideoProcAmp
  // =========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IAMVideoProcAmp);'}
  {$EXTERNALSYM IAMVideoProcAmp}
  IAMVideoProcAmp = interface(IUnknown)
    ['{C6E13360-30AC-11d0-A18C-00A0C9118956}']

    function GetRange(Property_: LONG; { VideoProcAmpProperty }
                      out pMin: LONG;
                      pMax: LONG;
                      pSteppingDelta: LONG;
                      pDefault: LONG;
                      out pCapsFlags: LONG { VideoProcAmpFlags }): HResult; stdcall;

    function Set_(Property_: VideoProcAmpProperty;
                  lValue: LONG;
                  Flags: LONG { VideoProcAmpFlags }): HResult; stdcall;

    function Get(Property_: LONG { VideoProcAmpProperty };
                 out lValue: LONG;
                 out Flags: LONG { VideoProcAmpFlags }): HResult; stdcall;

  end;
  IID_IAMVideoProcAmp = IAMVideoProcAmp;
  {$EXTERNALSYM IID_IAMVideoProcAmp}


  PCameraControlProperty = ^CameraControlProperty;
  tagCameraControlProperty = (
    CameraControl_Pan,
    CameraControl_Tilt,
    CameraControl_Roll,
    CameraControl_Zoom,
    CameraControl_Exposure,
    CameraControl_Iris,
    CameraControl_Focus
  );
  {$EXTERNALSYM tagCameraControlProperty}
  CameraControlProperty = tagCameraControlProperty;
  {$EXTERNALSYM CameraControlProperty}


  PCameraControlFlags = ^CameraControlFlags;
  tagCameraControlFlags = (
    CameraControl_Flags_Manual = 1,
    CameraControl_Flags_Auto   = 2
  );
  {$EXTERNALSYM tagCameraControlFlags}
  CameraControlFlags = tagCameraControlFlags;
  {$EXTERNALSYM CameraControlFlags}


  // Interface IAMCameraControl
  // ==========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IAMCameraControl);'}
  {$EXTERNALSYM IAMCameraControl}
  IAMCameraControl = interface(IUnknown)
    ['{C6E13370-30AC-11d0-A18C-00A0C9118956}']

    function GetRange(Property_: LONG { CameraControlProperty };
                      out pMin: LONG;
                      pMax: LONG;
                      pSteppingDelta: LONG;
                      pDefault: LONG;
                      pCapsFlags: LONG): HResult; stdcall;

    function Set_(Property_: LONG { CameraControlProperty };
                  lValue: LONG;
                  Flags: LONG { CameraControlFlags} ): HResult; stdcall;

    function Get( Property_: LONG { CameraControlProperty };
                  out lValue: LONG;
                  out Flags: LONG { CameraControlFlags }): HResult; stdcall;

  end;
  IID_IAMCameraControl = IAMCameraControl;
  {$EXTERNALSYM IID_IAMCameraControl}


  PVideoControlFlags = ^VideoControlFlags;
  tagVideoControlFlags                     = (
    VideoControlFlag_FlipHorizontal        = $1,
    VideoControlFlag_FlipVertical          = $2,
    VideoControlFlag_ExternalTriggerEnable = $4,
    VideoControlFlag_Trigger               = $8
  );
  {$EXTERNALSYM tagVideoControlFlags}
  VideoControlFlags = tagVideoControlFlags;
  {$EXTERNALSYM VideoControlFlags}


  // Interface IAMVideoControl
  // =========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IAMVideoControl);'}
  {$EXTERNALSYM IAMVideoControl}
  IAMVideoControl = interface(IUnknown)
    ['{6a2e0670-28e4-11d0-a18c-00a0c9118956}']
      function GetCaps(pPin: IPin;
                       out pCapsFlags: LONG): HResult; stdcall;

      function SetMode(pPin: IPin;
                       Mode: LONG): HResult; stdcall;

      function GetMode(pPin: IPin;
                       out Mode: LONG): HResult; stdcall;

      function GetCurrentActualFrameRate(pPin: IPin;
                                         out ActualFrameRate: LONGLONG): HResult; stdcall;

      function GetMaxAvailableFrameRate(pPin: IPin;
                                        const iIndex: LONG;
                                        const Dimensions: TSize;
                                        out MaxAvailableFrameRate: LONGLONG): HResult; stdcall;

      function GetFrameRateList(pPin: IPin;
                                const iIndex: LONG;
                                const Dimensions: SIZE;
                                out ListSize: LONG;
                                out FrameRates: LONGLONG): HResult; stdcall;

  end;
  IID_IAMVideoControl = IAMVideoControl;
  {$EXTERNALSYM IID_IAMVideoControl}


  // Interface IAMCrossbar
  // =====================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IAMCrossbar);'}
  {$EXTERNALSYM IAMCrossbar}
  IAMCrossbar = interface(IUnknown)
    ['{C6E13380-30AC-11d0-A18C-00A0C9118956}']

      function get_PinCounts(out OutputPinCount: LONG;
                             out InputPinCount: LONG): HResult; stdcall;

      function CanRoute(OutputPinIndex: LONG;
                        InputPinIndex: LONG): HResult; stdcall;

      function Route(OutputPinIndex: LONG;
                     InputPinIndex: LONG): HResult; stdcall;

      function get_IsRoutedTo(OutputPinIndex: LONG;
                              out InputPinIndex: LONG): HResult; stdcall;

      function get_CrossbarPinInfo(IsInputPin: BOOL;
                                   PinIndex: LONG;
                                   out PinIndexRelated: LONG;
                                   out PhysicalType: LONG { PhysicalConnectorType }): HResult; stdcall;

  end;
  IID_IAMCrossbar = IAMCrossbar;
  {$EXTERNALSYM IID_IAMCrossbar}


  PAMTunerSubChannel = ^AMTunerSubChannel;
  tagAMTunerSubChannel = (
     AMTUNER_SUBCHAN_NO_TUNE  = -2,
    AMTUNER_SUBCHAN_DEFAULT   = -1
  );
  {$EXTERNALSYM tagAMTunerSubChannel}
  AMTunerSubChannel = tagAMTunerSubChannel;
  {$EXTERNALSYM AMTunerSubChannel}


  PAMTunerSignalStrength = ^AMTunerSignalStrength;
  tagAMTunerSignalStrength = (
    AMTUNER_HASNOSIGNALSTRENGTH  = -1,
    AMTUNER_NOSIGNAL             = 0,
    AMTUNER_SIGNALPRESENT        = 1
  );
  {$EXTERNALSYM tagAMTunerSignalStrength}
  AMTunerSignalStrength = tagAMTunerSignalStrength;
  {$EXTERNALSYM AMTunerSignalStrength}


  PAMTunerModeType = ^AMTunerModeType;
  tagAMTunerModeType = (
    AMTUNER_MODE_DEFAULT   = 0,
    AMTUNER_MODE_TV        = 1,
    AMTUNER_MODE_FM_RADIO  = 2,
    AMTUNER_MODE_AM_RADIO  = 4,
    AMTUNER_MODE_DSS       = 8
  );
  {$EXTERNALSYM tagAMTunerModeType}
  AMTunerModeType = tagAMTunerModeType;
  {$EXTERNALSYM AMTunerModeType}


  PAMTunerEventType = ^AMTunerEventType;
  tagAMTunerEventType = (
    AMTUNER_EVENT_CHANGED = 1
  );
  {$EXTERNALSYM tagAMTunerEventType}
  AMTunerEventType = tagAMTunerEventType;
  {$EXTERNALSYM AMTunerEventType}


  // Interface IAMTuner
  // ==================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IAMTuner);'}
  {$EXTERNALSYM IAMTuner}
  IAMTuner = interface(IUnknown)
    ['{211A8761-03AC-11d1-8D13-00AA00BD8339}']

      function put_Channel(lChannel: LONG;
                           lVideoSubChannel: LONG;
                           lAudioSubChannel: LONG): HResult; stdcall;

      function get_Channel(out lChannel: LONG;
                           lVideoSubChannel: LONG;
                           lAudioSubChannel: LONG): HResult; stdcall;

      function ChannelMinMax(out lChannelMin: LONG;
                             lChannelMax: LONG): HResult; stdcall;

      function put_CountryCode(lCountryCode: LONG): HResult; stdcall;

      function get_CountryCode(out lCountryCode: LONG): HResult; stdcall;

      function put_TuningSpace(lTuningSpace: LONG): HResult; stdcall;

      function get_TuningSpace(out lTuningSpace: LONG): HResult; stdcall;

      function Logon(const hCurrentUser: THandle): HResult; stdcall;

      function Logout(): HResult; stdcall;

      function SignalPresent(out plSignalStrength: LONG): HResult; stdcall;

      function put_Mode(lMode: LONG { AMTunerModeType }): HResult; stdcall;

      function get_Mode(out plMode: LONG { AMTunerModeType }): HResult; stdcall;

      function GetAvailableModes(out plModes: LONG): HResult; stdcall;

      function RegisterNotificationCallBack(pNotify: LONG { IAMTunerNotification };
                                            lEvents: LONG): HResult; stdcall;

      function UnRegisterNotificationCallBack(pNotify: LONG { IAMTunerNotification }): HResult; stdcall;

  end;
  IID_IAMTuner = IAMTuner;
  {$EXTERNALSYM IID_IAMTuner}


  // Interface IAMTunerNotification
  // ==============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IAMTunerNotification);'}
  {$EXTERNALSYM IAMTunerNotification}
  IAMTunerNotification = interface(IUnknown)
    ['{211A8760-03AC-11d1-8D13-00AA00BD8339}']

      function OnEvent(Event: LONG{ AMTunerEventType }): HResult; stdcall;

  end;
  IID_IAMTunerNotification = IAMTunerNotification;
  {$EXTERNALSYM IID_IAMTunerNotification}


  // Interface IAMTVTuner
  // ====================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IAMTVTuner);'}
  {$EXTERNALSYM IAMTVTuner}
  IAMTVTuner = interface(IAMTuner)
    ['{211A8766-03AC-11d1-8D13-00AA00BD8339}']

      function get_AvailableTVFormats(out lAnalogVideoStandard: LONG): HResult; stdcall;

      function get_TVFormat(out plAnalogVideoStandard: LONG): HResult; stdcall;

      function AutoTune(lChannel: LONG;
                        out plFoundSignal: LONG): HResult; stdcall;

      function StoreAutoTune(): HResult; stdcall;

      function get_NumInputConnections(out plNumInputConnections: LONG): HResult; stdcall;

      function put_InputType(lIndex: LONG;
                              nputType: LONG { TunerInputType }): HResult; stdcall;

      function get_InputType(lIndex: LONG;
                             out InputType: LONG { TunerInputType }): HResult; stdcall;

      function put_ConnectInput(lIndex: LONG): HResult; stdcall;

      function get_ConnectInput(out plIndex: LONG): HResult; stdcall;

      function get_VideoFrequency(out lFreq: LONG): HResult; stdcall;

      function get_AudioFrequency(out lFreq: LONG): HResult; stdcall;

  end;
  IID_IAMTVTuner = IAMTVTuner;
  {$EXTERNALSYM IID_IAMTVTuner}


  // interface IBPCSatelliteTuner
  //=============================
  //
  // Note
  //  This interface is not implemented and has been deprecated.
  //{$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IBPCSatelliteTuner);'}
  //{$EXTERNALSYM IBPCSatelliteTuner}
  //IBPCSatelliteTuner = interface(IAMTuner)
  //  ['{211A8765-03AC-11d1-8D13-00AA00BD8339}']
  //    function get_DefaultSubChannelTypes(out plDefaultVideoType: LONG;
  //                                        const plDefaultAudioType: LONG): HResult; stdcall;
  //
  //    function put_DefaultSubChannelTypes(const lDefaultVideoType: LONG;
  //                                        const lDefaultAudioType: LONG): HResult; stdcall;
  //
  //    function IsTapingPermitted(): HResult; stdcall;
  //
  //end;
  //IID_IBPCSatelliteTuner = IBPCSatelliteTuner;
  //{$EXTERNALSYM IID_IBPCSatelliteTuner}


  PTVAudioMode = ^TVAudioMode;
  tagTVAudioMode            = (
    AMTVAUDIO_MODE_MONO     = $1,
    AMTVAUDIO_MODE_STEREO   = $2,
    AMTVAUDIO_MODE_LANG_A   = $10,
    AMTVAUDIO_MODE_LANG_B   = $20,
    AMTVAUDIO_MODE_LANG_C   = $40,
    AMTVAUDIO_PRESET_STEREO = $200,
    AMTVAUDIO_PRESET_LANG_A = $1000,
    AMTVAUDIO_PRESET_LANG_B = $2000,
    AMTVAUDIO_PRESET_LANG_C = $4000
  );
  {$EXTERNALSYM tagTVAudioMode}
  TVAudioMode = tagTVAudioMode;
  {$EXTERNALSYM TVAudioMode}


  PAMTVAudioEventType = ^AMTVAudioEventType;
  tagAMTVAudioEventType     = (
    AMTVAUDIO_EVENT_CHANGED = $1
  );
  {$EXTERNALSYM tagAMTVAudioEventType}
  AMTVAudioEventType = tagAMTVAudioEventType;
  {$EXTERNALSYM AMTVAudioEventType}


  // Interface IAMTVAudio
  // ====================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IAMTVAudio);'}
  {$EXTERNALSYM IAMTVAudio}
  IAMTVAudio = interface(IUnknown)
    ['{83EC1C30-23D1-11d1-99E6-00A0C9560266}']

      function GetHardwareSupportedTVAudioModes(out plModes: LONG): HResult; stdcall;

      function GetAvailableTVAudioModes(out plModes: LONG): HResult; stdcall;

      function get_TVAudioMode(out plMode: LONG): HResult; stdcall;

      function put_TVAudioMode(lMode: LONG): HResult; stdcall;

      function RegisterNotificationCallBack(pNotify: IAMTunerNotification;
                                            lEvents: LONG): HResult; stdcall;

      function UnRegisterNotificationCallBack(pNotify: IAMTunerNotification): HResult; stdcall;

  end;
  IID_IAMTVAudio = IAMTVAudio;
  {$EXTERNALSYM IID_IAMTVAudio}


  // Interface IAMTVAudioNotification
  // ================================
  // Note
  //  This callback interface has been deprecated,
  //  because the TV Audio filter does not implement the callback mechanism.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IAMTVAudioNotification);'}
  {$EXTERNALSYM IAMTVAudioNotification}
  IAMTVAudioNotification = interface(IUnknown)
    ['{83EC1C33-23D1-11D1-99E6-00A0C9560266}']

      function OnEvent(Event: AMTVAudioEventType): HResult; stdcall;

  end;
  IID_IAMTVAudioNotification = IAMTVAudioNotification;
  {$EXTERNALSYM IID_IAMTVAudioNotification}



  // Interface IAMAnalogVideoEncoder
  // ===============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IAMAnalogVideoEncoder);'}
  {$EXTERNALSYM IAMAnalogVideoEncoder}
  IAMAnalogVideoEncoder = interface(IUnknown)
    ['{C6E133B0-30AC-11d0-A18C-00A0C9118956}']

      function get_AvailableTVFormats(out lAnalogVideoStandard: LONG): HResult; stdcall;

      function put_TVFormat(lAnalogVideoStandard: LONG): HResult; stdcall;

      function get_TVFormat(out plAnalogVideoStandard: LONG): HResult; stdcall;

      function put_CopyProtection(lVideoCopyProtection: LONG): HResult; stdcall;

      function get_CopyProtection(out lVideoCopyProtection: LONG): HResult; stdcall;

      function put_CCEnable(lCCEnable: LONG): HResult; stdcall;

      function get_CCEnable(out lCCEnable: LONG): HResult; stdcall;

  end ;
  IID_IAMAnalogVideoEncoder = IAMAnalogVideoEncoder;
  {$EXTERNALSYM IID_IAMAnalogVideoEncoder}


  PAMPropertyPin = ^AMPROPERTY_PIN;
  AMPROPERTY_PIN = (
    AMPROPERTY_PIN_CATEGORY,
    AMPROPERTY_PIN_MEDIUM
  );
  {$EXTERNALSYM AMPROPERTY_PIN}


  // Interface IKsPropertySet
  // ========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IKsPropertySet);'}
  {$EXTERNALSYM IKsPropertySet}
  IKsPropertySet = interface(IUnknown)
    ['{31EFAC30-515C-11d0-A9AA-00AA0061BE93}']

      function Set_(const guidPropSet: REFGUID;
                    dwPropID: AMPROPERTY_PIN;
                    pInstanceData: LPVOID;
                    cbInstanceData: DWORD;
                    pPropData: LPVOID;
                    cbPropData: DWORD): HResult; stdcall;

      function Get(const guidPropSet: REFGUID;
                   dwPropID: AMPROPERTY_PIN;
                   pInstanceData: LPVOID;
                   cbInstanceData: DWORD;
                   out pPropData: DWORD;
                   cbPropData: DWORD;
                   out pcbReturned: DWORD): HResult; stdcall;

      function QuerySupported(const guidPropSet: REFGUID;
                              dwPropID: AMPROPERTY_PIN;
                              out pTypeSupport: DWORD): HResult; stdcall;

  end;
  IID_IKsPropertySet = IKsPropertySet;
  {$EXTERNALSYM IID_IKsPropertySet}


  // Interface IMediaPropertyBag
  // ===========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMediaPropertyBag);'}
  {$EXTERNALSYM IMediaPropertyBag}
  IMediaPropertyBag = interface(IPropertyBag)
    ['{6025A880-C0D5-11d0-BD4E-00A0C911CE86}']

      function EnumProperty(iProperty: ULONG;
                            var pvarPropertyName: VARIANT;
                            var pvarPropertyValue: VARIANT): HResult; stdcall;

  end;
  IID_IMediaPropertyBag = IMediaPropertyBag;
  {$EXTERNALSYM IID_IMediaPropertyBag}


  // Interface IPersistMediaPropertyBag
  // ==================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IPersistMediaPropertyBag);'}
  {$EXTERNALSYM IPersistMediaPropertyBag}
  IPersistMediaPropertyBag = interface(IPersist)
    ['{5738E040-B67F-11d0-BD4D-00A0C911CE86}']

      function InitNew(): HResult; stdcall;

      function Load(pPropBag: IMediaPropertyBag;
                    pErrorLog: IErrorLog): HResult; stdcall;

      function Save(pPropBag: IMediaPropertyBag;
                    fClearDirty: BOOL;
                    fSaveAllProperties: BOOL): HResult; stdcall;

  end;
  IID_IPersistMediaPropertyBag = IPersistMediaPropertyBag;
  {$EXTERNALSYM IID_IPersistMediaPropertyBag}



  // Interface IAMPhysicalPinInfo
  // ============================
  // Note
  //  This interface has been deprecated.
  //  It will continue to be supported for backward compatibility with existing applications,
  //  but new applications and filters should not use it.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IAMPhysicalPinInfo);'}
  {$EXTERNALSYM IAMPhysicalPinInfo}
  IAMPhysicalPinInfo = interface(IUnknown)
    ['{F938C991-3029-11cf-8C44-00AA006B6814}']

      function GetPhysicalType(out pType: LONG;
                               out ppszType: LPOLESTR): HResult; stdcall;

  end;
  IID_IAMPhysicalPinInfo = IAMPhysicalPinInfo;
  {$EXTERNALSYM IID_IAMPhysicalPinInfo}



  // Interface IAMExtDevice
  // ======================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IAMExtDevice);'}
  {$EXTERNALSYM IAMExtDevice}
  IAMExtDevice = interface(IUnknown)
    ['{B5730A90-1A2C-11cf-8C23-00AA006B6814}']

      function GetCapability(Capability: LONG;
                             out pValue: LONG;
                             out pdblValue: double): HResult; stdcall;

      function get_ExternalDeviceID(out ppszData: LPOLESTR): HResult; stdcall;

      function get_ExternalDeviceVersion(out ppszData: LPOLESTR): HResult; stdcall;

      function put_DevicePower(PowerMode: LONG): HResult; stdcall;

      function get_DevicePower(out pPowerMode: LONG): HResult; stdcall;

      function Calibrate(hEvent: HEVENT;
                         Mode: LONG;
                         out pStatus: LONG): HResult; stdcall;

      function put_DevicePort(DevicePort: LONG): HResult; stdcall;

      function get_DevicePort(out pDevicePort: LONG): HResult; stdcall;

  end;
  IID_IAMExtDevice = IAMExtDevice;
  {$EXTERNALSYM IID_IAMExtDevice}


  // Interface IAMExtTransport
  // =========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IAMExtTransport);'}
  {$EXTERNALSYM IAMExtTransport}
  IAMExtTransport = interface(IUnknown)
    ['{A03CD5F0-3045-11CF-8C44-00AA006B6814}']

      function GetCapability(Capability: LONG;
                             out pValue: LONG;
                             out pdblValue: double): HResult; stdcall;

      function put_MediaState(State: LONG): HResult; stdcall;

      function get_MediaState(out pState: LONG): HResult; stdcall;

      function put_LocalControl(State: LONG): HResult; stdcall;

      function get_LocalControl(out pState: LONG): HResult; stdcall;

      function GetStatus(StatusItem: LONG;
                         out pValue: LONG): HResult; stdcall;

      function GetTransportBasicParameters(Param: LONG;
                                           var pValue: LONG;
                                           var ppszData: LPOLESTR): HResult; stdcall;

      function SetTransportBasicParameters(Param: LONG;
                                           Value: LONG;
                                           pszData: LPCOLESTR): HResult; stdcall;

      function GetTransportVideoParameters( const Param: LONG;
                                            out pValue: LONG): HResult; stdcall;

      function SetTransportVideoParameters(Param: LONG;
                                           Value: LONG): HResult; stdcall;

      function GetTransportAudioParameters(Param: LONG;
                                           out pValue: LONG): HResult; stdcall;

      function SetTransportAudioParameters(Param: LONG;
                                           Value: LONG): HResult; stdcall;

      function put_Mode(Mode: LONG): HResult; stdcall;

      function get_Mode(out pMode: LONG): HResult; stdcall;

      function put_Rate(dblRate: double): HResult; stdcall;

      function get_Rate(out pdblRate: double): HResult; stdcall;

      function GetChase(out pEnabled: LONG;
                        out pOffset: LONG;
                        var phEvent: HEVENT): HResult; stdcall;

      function SetChase(Enable: LONG;
                        Offset: LONG;
                        hEvent: HEVENT): HResult; stdcall;

      function GetBump(out pSpeed: LONG;
                       pDuration: LONG): HResult; stdcall;

      function SetBump(Speed: LONG;
                       Duration: LONG): HResult; stdcall;

      function get_AntiClogControl(out pEnabled: LONG): HResult; stdcall;

      function put_AntiClogControl(Enable: LONG): HResult; stdcall;

      function GetEditPropertySet(EditID: LONG;
                                  out pState: LONG): HResult; stdcall;

      function SetEditPropertySet(var pEditID: LONG;
                                  State: LONG): HResult; stdcall;

      function GetEditProperty(EditID: LONG;
                               Param: LONG;
                               out pValue: LONG): HResult; stdcall;

      function SetEditProperty(EditID: LONG;
                               Param: LONG;
                               Value: LONG): HResult; stdcall;

      function get_EditStart(out pValue: LONG): HResult; stdcall;

      function put_EditStart(Value: LONG): HResult; stdcall;

  end;
  IID_IAMExtTransport = IAMExtTransport;
  {$EXTERNALSYM IID_IAMExtTransport}


{$ifndef TIMECODE_DEFINED}
{$define TIMECODE_DEFINED}
  PTIMECODE = ^TIMECODE;
  tagTIMECODE = record
    wFrameRate: WORD;
    wFrameFract: WORD;
    dwFrames: DWORD;
  end;
  {$EXTERNALSYM tagTIMECODE}
  TIMECODE = tagTIMECODE;
  {$EXTERNALSYM TIMECODE}

  PTIMECODE_SAMPLE = ^TIMECODE_SAMPLE;
  tagTIMECODE_SAMPLE = record
    qwTick: LONGLONG;
    timecode: TIMECODE;
    dwUser: DWORD;
    dwFlags: DWORD;
  end;
  {$EXTERNALSYM tagTIMECODE_SAMPLE}
  TIMECODE_SAMPLE = tagTIMECODE_SAMPLE;
  {$EXTERNALSYM TIMECODE_SAMPLE}
{$endif} //* TIMECODE_DEFINED */


  // Interface IAMTimecodeReader
  // ===========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IAMTimecodeReader);'}
  {$EXTERNALSYM IAMTimecodeReader}
  IAMTimecodeReader = interface(IUnknown)
    ['{9B496CE1-811B-11CF-8C77-00AA006B6814}']

      function GetTCRMode(Param: LONG;
                          out pValue: LONG): HResult; stdcall;

      function SetTCRMode(Param: LONG;
                          Value: LONG): HResult; stdcall;

      function put_VITCLine(Line: LONG): HResult; stdcall;

      function get_VITCLine(out pLine: LONG): HResult; stdcall;

      function GetTimecode(out pTimecodeSample: TIMECODE_SAMPLE): HResult; stdcall;

  end;
  IID_IAMTimecodeReader = IAMTimecodeReader;
  {$EXTERNALSYM IID_IAMTimecodeReader}


  // Interface IAMTimecodeGenerator
  // ==============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IAMTimecodeGenerator);'}
  {$EXTERNALSYM IAMTimecodeGenerator}
  IAMTimecodeGenerator = interface(IUnknown)
    ['{9B496CE0-811B-11CF-8C77-00AA006B6814}']

      function GetTCGMode(Param: LONG;
                          out pValue: LONG): HResult; stdcall;

      function SetTCGMode(Param: LONG;
                          Value: LONG): HResult; stdcall;

      function put_VITCLine(Line: LONG): HResult; stdcall;

      function get_VITCLine(out Line: LONG): HResult; stdcall;

      function SetTimecode(var pTimecodeSample: TIMECODE_SAMPLE): HResult; stdcall;

      function GetTimecode(out pTimecodeSample: TIMECODE_SAMPLE): HResult; stdcall;

  end;
  IID_IAMTimecodeGenerator = IAMTimecodeGenerator;
  {$EXTERNALSYM IID_IAMTimecodeGenerator}


  // Interface IAMTimecodeDisplay
  // ============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IAMTimecodeDisplay);'}
  {$EXTERNALSYM IAMTimecodeDisplay}
  IAMTimecodeDisplay = interface(IUnknown)
    ['{9B496CE2-811B-11cf-8C77-00AA006B6814}']

      function GetTCDisplayEnable(out pState: LONG): HResult; stdcall;

      function SetTCDisplayEnable(State: LONG): HResult; stdcall;

      function GetTCDisplay(Param: LONG;
                            out pValue: LONG): HResult; stdcall;

      function SetTCDisplay(Param: LONG;
                            Value: LONG): HResult; stdcall;

  end;
  IID_IAMTimecodeDisplay = IAMTimecodeDisplay;
  {$EXTERNALSYM IID_IAMTimecodeDisplay}


  // Interface IAMDevMemoryAllocator
  // ===============================
  // Note 1
  //  This interface is no longer supported by the AVI Splitter.
  //
  // Note 2
  //  This interface was defined to support older hardware decoders that
  //  required AVI files to be read into directly hardware memory.
  //  The interface enables the AVI parser to allocate memory from the downstream filter but
  //  still provide its own allocator.
  //
  // Implement this interface when your pin must support the creation of on-board memory allocators.
  // Source filters that are aware of on-board memory and need to create their own allocators should
  // query for this interface, request an amount of memory and then create an
  // allocator (aggregating the device memory control object).
  // Source filters that don't need to create their own allocator could just use the allocator of
  // the downstream pin (which also aggregates the device memory control object).
  // The hardware-based filter can confirm the usage of its on-board memory by calling methods on
  // the aggregated allocator.
  // Use this interface when applications need to control the memory of codecs with on-board memory.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IAMDevMemoryAllocator);'}
  {$EXTERNALSYM IAMDevMemoryAllocator}
  IAMDevMemoryAllocator = interface(IUnknown)
    ['{c6545bf0-e76b-11d0-bd52-00a0c911ce86}']

      function GetInfo(out pdwcbTotalFree: DWORD;
                       out pdwcbLargestFree: DWORD;
                       out pdwcbTotalMemory: DWORD;
                       out pdwcbMinimumChunk: DWORD): HResult; stdcall;

      function CheckMemory(pBuffer: BYTE): HResult; stdcall;

      function Alloc(out ppBuffer: PBYTE;
                     var pdwcbBuffer: DWORD): HResult; stdcall;

      function Free(pBuffer: BYTE): HResult; stdcall;

      function GetDevMemoryObject(out ppUnkInnner: IUnknown;
                                  pUnkOuter: IUnknown): HResult; stdcall;

  end;
  IID_IAMDevMemoryAllocator = IAMDevMemoryAllocator;
  {$EXTERNALSYM IID_IAMDevMemoryAllocator}


  // Interface IAMDevMemoryControl
  // =============================
  // Note 1
  //   This interface is no longer supported by the AVI Splitter.
  //
  // Note 2
  //   It was defined to support certain older hardware decoders that required AVI files to
  //   be read directly into hardware memory.
  //   The interface enables the AVI parser to allocate memory from the downstream filter but
  //   still provide its own allocator.
  //   There should be no need for any newer devices to support this interface.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IAMDevMemoryControl);'}
  {$EXTERNALSYM IAMDevMemoryControl}
  IAMDevMemoryControl = interface(IUnknown)
    ['{C6545BF1-E76B-11D0-BD52-00A0C911CE86}']

      function QueryWriteSync(): HResult; stdcall;

      function WriteSync(): HResult; stdcall;

      function GetDevId(out pdwDevId: DWORD): HResult; stdcall;

  end;
  IID_IAMDevMemoryControl = IAMDevMemoryControl;
  {$EXTERNALSYM IID_IAMDevMemoryControl}


  PAMSTREAMSELECTINFOFLAGS = ^_AMSTREAMSELECTINFOFLAGS;
  _AMSTREAMSELECTINFOFLAGS = (
    AMSTREAMSELECTINFO_ENABLED    = $1,
    AMSTREAMSELECTINFO_EXCLUSIVE  = $2
  );
  {$EXTERNALSYM _AMSTREAMSELECTINFOFLAGS}
  AMSTREAMSELECTINFOFLAGS = _AMSTREAMSELECTINFOFLAGS;
  {$EXTERNALSYM AMSTREAMSELECTINFOFLAGS}


  PAMSTREAMSELECTENABLEFLAGS = ^_AMSTREAMSELECTENABLEFLAGS;
  _AMSTREAMSELECTENABLEFLAGS = (
    AMSTREAMSELECTENABLE_ENABLE      = $1,
    AMSTREAMSELECTENABLE_ENABLEALL   = $2
  );
  {$EXTERNALSYM _AMSTREAMSELECTENABLEFLAGS}
  AMSTREAMSELECTENABLEFLAGS = _AMSTREAMSELECTENABLEFLAGS;
  {$EXTERNALSYM AMSTREAMSELECTENABLEFLAGS}


  // Interface IAMStreamSelect
  // =========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IAMStreamSelect);'}
  {$EXTERNALSYM IAMStreamSelect}
  IAMStreamSelect = interface(IUnknown)
    ['{c1960960-17f5-11d1-abe1-00a0c905f375}']

      function Count(out pcStreams: DWORD): HResult; stdcall;

      function Info(lIndex: Longint;
                    out ppmt: PAM_MEDIA_TYPE;
                    out pdwFlags: DWORD;
                    out plcid: LCID;
                    out pdwGroup: DWORD;
                    out ppszName: LPWSTR;
                    out ppObject: IUnknown;
                    out ppUnk: IUnknown): HResult; stdcall;

      function Enable(lIndex: Longint;
                      dwFlags: DWORD): HResult; stdcall;

  end;
  IID_IAMStreamSelect = IAMStreamSelect;
  {$EXTERNALSYM IID_IAMStreamSelect}


  // Interface IAMResourceControl
  // ============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IAMResourceControl);'}
  {$EXTERNALSYM IAMResourceControl}
  IAMResourceControl = interface(IUnknown)
    ['{8389d2d0-77d7-11d1-abe6-00a0c905f375}']

      function Reserve(dwFlags: DWORD;
                       var pvReserved: PVOID): HResult; stdcall;

  end;
  IID_IAMResourceControl = IAMResourceControl;
  {$EXTERNALSYM IID_IAMResourceControl}



  // Interface IAMClockAdjust
  // ========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IAMClockAdjust);'}
  {$EXTERNALSYM IAMClockAdjust}
  IAMClockAdjust = interface(IUnknown)
    ['{4d5466b0-a49c-11d1-abe8-00a0c905f375}']

      function SetClockDelta(const rtDelta: REFERENCE_TIME): HResult; stdcall;

  end;
  IID_IAMClockAdjust = IAMClockAdjust;
  {$EXTERNALSYM IID_IAMClockAdjust}



  PAM_FILTER_MISC_FLAGS = ^_AM_FILTER_MISC_FLAGS;
  _AM_FILTER_MISC_FLAGS = (
    AM_FILTER_MISC_FLAGS_IS_RENDERER  = $1,
    AM_FILTER_MISC_FLAGS_IS_SOURCE    = $2
  );
  {$EXTERNALSYM _AM_FILTER_MISC_FLAGS}
  AM_FILTER_MISC_FLAGS = _AM_FILTER_MISC_FLAGS;
  {$EXTERNALSYM AM_FILTER_MISC_FLAGS}


  // Interface IAMFilterMiscFlags
  // ============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IAMFilterMiscFlags);'}
  {$EXTERNALSYM IAMFilterMiscFlags}
  IAMFilterMiscFlags = interface(IUnknown)
    ['{2dd74950-a890-11d1-abe8-00a0c905f375}']

      function GetMiscFlags(): ULONG { AM_FILTER_MISC_FLAGS }; stdcall;

  end;
  IID_IAMFilterMiscFlags = IAMFilterMiscFlags;
  {$EXTERNALSYM IID_IAMFilterMiscFlags}


  // Interface IDrawVideoImage
  // =========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDrawVideoImage);'}
  {$EXTERNALSYM IDrawVideoImage}
  IDrawVideoImage = interface(IUnknown)
    ['{48efb120-ab49-11d2-aed2-00a0c995e8d5}']

      function DrawVideoImageBegin(): HResult; stdcall;

      function DrawVideoImageEnd(): HResult; stdcall;

      function DrawVideoImageDraw(hdc: HDC;
                                  lprcSrc: LPRECT;
                                  lprcDst: LPRECT): HResult; stdcall;

  end;
  IID_IDrawVideoImage = IDrawVideoImage;
  {$EXTERNALSYM IID_IDrawVideoImage}


  // Interface IDecimateVideoImage
  // =============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDecimateVideoImage);'}
  {$EXTERNALSYM IDecimateVideoImage}
  IDecimateVideoImage = interface(IUnknown)
    ['{2e5ea3e0-e924-11d2-b6da-00a0c995e8df}']

      function SetDecimationImageSize(lWidth: LONG;
                                      lHeight: LONG): HResult; stdcall;

      function ResetDecimationImageSize(): HResult; stdcall;

  end;


  PDECIMATION_USAGE = ^_DECIMATION_USAGE;
  _DECIMATION_USAGE               = (
    DECIMATION_LEGACY             = 0,
    DECIMATION_USE_DECODER_ONLY   = (DECIMATION_LEGACY + 1),
    DECIMATION_USE_VIDEOPORT_ONLY = (DECIMATION_USE_DECODER_ONLY + 1),
    DECIMATION_USE_OVERLAY_ONLY   = (DECIMATION_USE_VIDEOPORT_ONLY + 1),
    DECIMATION_DEFAULT            = (DECIMATION_USE_OVERLAY_ONLY + 1)
  );
  {$EXTERNALSYM _DECIMATION_USAGE}
  DECIMATION_USAGE = _DECIMATION_USAGE;
  {$EXTERNALSYM DECIMATION_USAGE}


  // Interface IAMVideoDecimationProperties
  // ======================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IAMVideoDecimationProperties);'}
  {$EXTERNALSYM IAMVideoDecimationProperties}
  IAMVideoDecimationProperties = interface(IUnknown)
    ['{60d32930-13da-11d3-9ec6-c4fcaef5c7be}']

      function QueryDecimationUsage(out lpUsage: DECIMATION_USAGE):HResult; stdcall;

      function SetDecimationUsage(Usage: DECIMATION_USAGE): HResult; stdcall;

  end;
  IID_IAMVideoDecimationProperties = IAMVideoDecimationProperties;
  {$EXTERNALSYM IID_IAMVideoDecimationProperties}


  // Interface IVideoFrameStep
  // =========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IVideoFrameStep);'}
  {$EXTERNALSYM IVideoFrameStep}
  IVideoFrameStep = interface(IUnknown)
    ['{e46a9787-2b71-444d-a4b5-1fab7b708d6a}']

      function Step(dwFrames: DWORD;
                    pStepObject: IUnknown): HResult; stdcall;

      function CanStep(bMultiple: LONG;
                       pStepObject: IUnknown): HResult; stdcall;

      function CancelStep(): HResult; stdcall;

  end;
  IID_IVideoFrameStep = IVideoFrameStep;
  {$EXTERNALSYM IID_IVideoFrameStep}


  PAM_PUSHSOURCE_FLAGS = ^_AM_PUSHSOURCE_FLAGS;
  _AM_PUSHSOURCE_FLAGS = (
    AM_PUSHSOURCECAPS_INTERNAL_RM        = $1,
    AM_PUSHSOURCECAPS_NOT_LIVE           = $2,
    AM_PUSHSOURCECAPS_PRIVATE_CLOCK      = $4,
    AM_PUSHSOURCEREQS_USE_STREAM_CLOCK   = $10000,
    AM_PUSHSOURCEREQS_USE_CLOCK_CHAIN    = $20000
  );
  {$EXTERNALSYM _AM_PUSHSOURCE_FLAGS}
  AM_PUSHSOURCE_FLAGS = _AM_PUSHSOURCE_FLAGS;
  {$EXTERNALSYM AM_PUSHSOURCE_FLAGS}


  // Interface IAMLatency
  // ====================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IAMLatency);'}
  {$EXTERNALSYM IAMLatency}
  IAMLatency = interface(IUnknown)
    ['{62EA93BA-EC62-11d2-B770-00C04FB6BD3D}']

      function GetLatency(out prtLatency: REFERENCE_TIME): HResult; stdcall;

  end;
  IID_IAMLatency = IAMLatency;
  {$EXTERNALSYM IID_IAMLatency}


  // Interface IAMPushSource
  // =======================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IAMPushSource);'}
  {$EXTERNALSYM IAMPushSource}
  IAMPushSource = interface(IAMLatency)
    ['{F185FE76-E64E-11d2-B76E-00C04FB6BD3D}']

      function GetPushSourceFlags(out pFlags: ULONG { _AM_PUSHSOURCE_FLAGS }): HResult; stdcall;

      function SetPushSourceFlags(Flags: ULONG { _AM_PUSHSOURCE_FLAGS }): HResult; stdcall;

      function SetStreamOffset(rtOffset: REFERENCE_TIME): HResult; stdcall;

      function GetStreamOffset(out prtOffset: REFERENCE_TIME): HResult; stdcall;

      function GetMaxStreamOffset(out prtMaxOffset: REFERENCE_TIME): HResult; stdcall;

      function SetMaxStreamOffset(rtMaxOffset: REFERENCE_TIME): HResult; stdcall;

  end;
  IID_IAMPushSource = IAMPushSource;
  {$EXTERNALSYM IID_IAMPushSource}


  // Interface IAMDeviceRemoval
  // ==========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IAMDeviceRemoval);'}
  {$EXTERNALSYM IAMDeviceRemoval}
  IAMDeviceRemoval = interface(IUnknown)
    ['{f90a6130-b658-11d2-ae49-0000f8754b99}']

    function DeviceInfo(out pclsidInterfaceClass: CLSID;
                        out pwszSymbolicLink: LPWSTR): HResult; stdcall;

    function Reassociate(): HResult; stdcall;

    function Disassociate(): HResult; stdcall;

  end;
  IID_IAMDeviceRemoval = IAMDeviceRemoval;
  {$EXTERNALSYM IID_IAMDeviceRemoval}


  PDVINFO = ^DVINFO;
  DVINFO = record
    dwDVAAuxSrc: DWORD;  // Specifies the audio auxiliary (AAUX) source pack for the first audio block.
    dwDVAAuxCtl: DWORD;  // Specifies the AAUX source control Pack for the first audio block.
    dwDVAAuxSrc1: DWORD; // Specifies the AAUX source pack for the second audio block.
    dwDVAAuxCtl1: DWORD; // Specifies the AAUX source control pack for the second audio block.
    dwDVVAuxSrc: DWORD;  // Specifies the video auxiliary (VAUX) source pack.
    dwDVVAuxCtl: DWORD;  // Specifies the VAUX source control pack.
    dwDVReserved: array[0..1] of DWORD;  // Reserved. Set this array to zero.
  end;
  {$EXTERNALSYM DVINFO}
  // Note
  //  The AAUX and VAUX packs are defined in IEC 61834-4.

  PDVENCODERRESOLUTION = ^_DVENCODERRESOLUTION;
  _DVENCODERRESOLUTION = (
    DVENCODERRESOLUTION_720x480  = 2012,
    DVENCODERRESOLUTION_360x240  = 2013,
    DVENCODERRESOLUTION_180x120  = 2014,
    DVENCODERRESOLUTION_88x60    = 2015
  );
  {$EXTERNALSYM _DVENCODERRESOLUTION}
  DVENCODERRESOLUTION = _DVENCODERRESOLUTION;
  {$EXTERNALSYM DVENCODERRESOLUTION}

  PDVENCODERVIDEOFORMAT = ^_DVENCODERVIDEOFORMAT;
  _DVENCODERVIDEOFORMAT = (
    DVENCODERVIDEOFORMAT_NTSC  = 2000,
    DVENCODERVIDEOFORMAT_PAL   = 2001
  );
  {$EXTERNALSYM _DVENCODERVIDEOFORMAT}
  DVENCODERVIDEOFORMAT = _DVENCODERVIDEOFORMAT;
  {$EXTERNALSYM DVENCODERVIDEOFORMAT}

  PDVENCODERFORMAT = ^_DVENCODERFORMAT;
  _DVENCODERFORMAT = (
    DVENCODERFORMAT_DVSD  = 2007,
    DVENCODERFORMAT_DVHD  = 2008,
    DVENCODERFORMAT_DVSL  = 2009
  );
  {$EXTERNALSYM _DVENCODERFORMAT}
  DVENCODERFORMAT = _DVENCODERFORMAT;
  {$EXTERNALSYM DVENCODERFORMAT}


  // Interface IDVEnc
  // ================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDVEnc);'}
  {$EXTERNALSYM IDVEnc}
  IDVEnc = interface(IUnknown)
    ['{d18e17a0-aacb-11d0-afb0-00aa00b67a42}']

      function get_IFormatResolution(out VideoFormat: INT;
                                     out DVFormat: INT;
                                     out Resolution: INT;
                                     out fDVInfo: Byte;
                                     out sDVInfo: DVINFO): HResult; stdcall;

      function put_IFormatResolution(VideoFormat: INT;
                                      DVFormat: INT;
                                      Resolution: INT;
                                      fDVInfo: Byte;
                                      sDVInfo: DVINFO): HResult; stdcall;

  end;
  IID_IDVEnc = IDVEnc;
  {$EXTERNALSYM IID_IDVEnc}


  PDVDECODERRESOLUTION = ^_DVDECODERRESOLUTION;
  _DVDECODERRESOLUTION = (
    DVDECODERRESOLUTION_720x480  = 1000,
    DVDECODERRESOLUTION_360x240  = 1001,
    DVDECODERRESOLUTION_180x120  = 1002,
    DVDECODERRESOLUTION_88x60    = 1003
  );
  {$EXTERNALSYM _DVDECODERRESOLUTION}
  DVDECODERRESOLUTION = _DVDECODERRESOLUTION;
  {$EXTERNALSYM DVDECODERRESOLUTION}

  PDVRESOLUTION = ^_DVRESOLUTION;
  _DVRESOLUTION = (
    DVRESOLUTION_FULL      = 1000,
    DVRESOLUTION_HALF      = 1001,
    DVRESOLUTION_QUARTER   = 1002,
    DVRESOLUTION_DC        = 1003
  );
  {$EXTERNALSYM _DVRESOLUTION}
  DVRESOLUTION = _DVRESOLUTION;
  {$EXTERNALSYM DVRESOLUTION}


  // Interface IIPDVDec
  // ==================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IIPDVDec);'}
  {$EXTERNALSYM IIPDVDec}
  IIPDVDec = interface(IUnknown)
    ['{b8e8bd60-0bfe-11d0-af91-00aa00b67a42}']

      function get_IPDisplay(out displayPix: INT): HResult; stdcall;

      function put_IPDisplay(displayPix: INT): HResult; stdcall;

  end;
  IID_IIPDVDec = IIPDVDec;
  {$EXTERNALSYM IID_IIPDVDec}


  // Interface IDVRGB219
  // ===================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDVRGB219);'}
  {$EXTERNALSYM IDVRGB219}
  IDVRGB219 = interface(IUnknown)
    ['{58473A19-2BC8-4663-8012-25F81BABDDD1}']

      function SetRGB219(bState: BOOL): HResult; stdcall;

  end;
  IID_IDVRGB219 = IDVRGB219;
  {$EXTERNALSYM IID_IDVRGB219}


  // Interface IDVSplitter
  // =====================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDVSplitter);'}
  {$EXTERNALSYM IDVSplitter}
  IDVSplitter = interface(IUnknown)
    ['{92a3a302-da7c-4a1f-ba7e-1802bb5d2d02}']

      function DiscardAlternateVideoFrames(nDiscard: INT): HResult; stdcall;

  end;
  IID_IDVSplitter = IDVSplitter;
  {$EXTERNALSYM IID_IDVSplitter}


  PAM_AUDIO_RENDERER_STAT_PARAM = ^_AM_AUDIO_RENDERER_STAT_PARAM;
  _AM_AUDIO_RENDERER_STAT_PARAM = (
    AM_AUDREND_STAT_PARAM_BREAK_COUNT  = 1,
    AM_AUDREND_STAT_PARAM_SLAVE_MODE  = ( AM_AUDREND_STAT_PARAM_BREAK_COUNT + 1),
    AM_AUDREND_STAT_PARAM_SILENCE_DUR  = ( AM_AUDREND_STAT_PARAM_SLAVE_MODE + 1),
    AM_AUDREND_STAT_PARAM_LAST_BUFFER_DUR  = ( AM_AUDREND_STAT_PARAM_SILENCE_DUR + 1),
    AM_AUDREND_STAT_PARAM_DISCONTINUITIES  = ( AM_AUDREND_STAT_PARAM_LAST_BUFFER_DUR + 1),
    AM_AUDREND_STAT_PARAM_SLAVE_RATE  = ( AM_AUDREND_STAT_PARAM_DISCONTINUITIES + 1),
    AM_AUDREND_STAT_PARAM_SLAVE_DROPWRITE_DUR  = ( AM_AUDREND_STAT_PARAM_SLAVE_RATE + 1 ),
    AM_AUDREND_STAT_PARAM_SLAVE_HIGHLOWERROR  = ( AM_AUDREND_STAT_PARAM_SLAVE_DROPWRITE_DUR + 1),
    AM_AUDREND_STAT_PARAM_SLAVE_LASTHIGHLOWERROR  = ( AM_AUDREND_STAT_PARAM_SLAVE_HIGHLOWERROR + 1),
    AM_AUDREND_STAT_PARAM_SLAVE_ACCUMERROR  = ( AM_AUDREND_STAT_PARAM_SLAVE_LASTHIGHLOWERROR + 1),
    AM_AUDREND_STAT_PARAM_BUFFERFULLNESS  = ( AM_AUDREND_STAT_PARAM_SLAVE_ACCUMERROR + 1),
    AM_AUDREND_STAT_PARAM_JITTER  = ( AM_AUDREND_STAT_PARAM_BUFFERFULLNESS + 1)
  );
  {$EXTERNALSYM _AM_AUDIO_RENDERER_STAT_PARAM}
  AM_AUDIO_RENDERER_STAT_PARAM = _AM_AUDIO_RENDERER_STAT_PARAM;
  {$EXTERNALSYM AM_AUDIO_RENDERER_STAT_PARAM}


  // Interface IAMAudioRendererStats
  // ===============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IAMAudioRendererStats);'}
  {$EXTERNALSYM IAMAudioRendererStats}
  IAMAudioRendererStats = interface(IUnknown)
    ['{22320CB2-D41A-11d2-BF7C-D7CB9DF0BF93}']

      function GetStatParam(dwParam: DWORD;
                            out pdwParam1: DWORD;
                            out pdwParam2: DWORD): HResult; stdcall;

  end;
  IID_IAMAudioRendererStats = IAMAudioRendererStats;
  {$EXTERNALSYM IID_IAMAudioRendererStats}


  PAM_INTF_SEARCH_FLAGS = ^_AM_INTF_SEARCH_FLAGS;
  _AM_INTF_SEARCH_FLAGS = (
    AM_INTF_SEARCH_INPUT_PIN   = $1,
    AM_INTF_SEARCH_OUTPUT_PIN  = $2,
    AM_INTF_SEARCH_FILTER      = $4
  );
  {$EXTERNALSYM _AM_INTF_SEARCH_FLAGS}
  AM_INTF_SEARCH_FLAGS = _AM_INTF_SEARCH_FLAGS;
  {$EXTERNALSYM AM_INTF_SEARCH_FLAGS}


  // Interface IAMGraphStreams
  // =========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IAMGraphStreams);'}
  {$EXTERNALSYM IAMGraphStreams}
  IAMGraphStreams = interface(IUnknown)
    ['{632105FA-072E-11d3-8AF9-00C04FB6BD3D}']

      function FindUpstreamInterface(pPin: IPin;
                                     const riid: TGUID;
                                     out ppvInterface;
                                     dwFlags: DWORD { AM_INTF_SEARCH_FLAGS }): HResult; stdcall;

      function SyncUsingStreamOffset(bUseStreamOffset: BOOL): HResult; stdcall;

      function SetMaxGraphLatency(rtMaxGraphLatency: REFERENCE_TIME): HResult; stdcall;

  end;
  IID_IAMGraphStreams = IAMGraphStreams;
  {$EXTERNALSYM IID_IAMGraphStreams}


  PAMOVERLAYFX = ^AMOVERLAYFX;
  AMOVERLAYFX = (
    AMOVERFX_NOFX              = 0,
    AMOVERFX_MIRRORLEFTRIGHT   = $2,
    AMOVERFX_MIRRORUPDOWN      = $4,
    AMOVERFX_DEINTERLACE       = $8
  );
  {$EXTERNALSYM AMOVERLAYFX}


  // Interface IAMOverlayFX
  // ======================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IAMOverlayFX);'}
  {$EXTERNALSYM IAMOverlayFX}
  IAMOverlayFX = interface(IUnknown)
    ['{62fae250-7e65-4460-bfc9-6398b322073c}']

      function QueryOverlayFXCaps(out lpdwOverlayFXCaps: DWORD): HResult; stdcall;

      function SetOverlayFX(dwOverlayFX: DWORD): HResult; stdcall;

      function GetOverlayFX(out lpdwOverlayFX: DWORD): HResult; stdcall;

  end;
  IID_IAMOverlayFX = IAMOverlayFX;
  {$EXTERNALSYM IID_IAMOverlayFX}


  // Interface IAMOpenProgress
  // =========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IAMOpenProgress);'}
  {$EXTERNALSYM IAMOpenProgress}
  IAMOpenProgress = interface(IUnknown)
    ['{8E1C39A1-DE53-11cf-AA63-0080C744528D}']

      function QueryProgress(out pllTotal: LONGLONG;
                             out pllCurrent: LONGLONG): HResult; stdcall;

      function AbortOperation(): HResult; stdcall;

  end;
  IID_IAMOpenProgress = IAMOpenProgress;
  {$EXTERNALSYM IID_IAMOpenProgress}


  // Interface IMpeg2Demultiplexer
  // =============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMpeg2Demultiplexer);'}
  {$EXTERNALSYM IMpeg2Demultiplexer}
  IMpeg2Demultiplexer = interface(IUnknown)
    ['{436eee9c-264f-4242-90e1-4e330c107512}']

      function CreateOutputPin(pMediaType: AM_MEDIA_TYPE;
                               pszPinName: LPWSTR;
                               out ppIPin: IPin): HResult; stdcall;

      function SetOutputPinMediaType(pszPinName: LPWSTR;
                                     pMediaType: AM_MEDIA_TYPE): HResult; stdcall;

      function DeleteOutputPin(pszPinName: LPWSTR): HResult; stdcall;

  end;
  IID_IMpeg2Demultiplexer = IMpeg2Demultiplexer;
  {$EXTERNALSYM IID_IMpeg2Demultiplexer}


  PSTREAM_ID_MAP = ^STREAM_ID_MAP;
  STREAM_ID_MAP = record
    stream_id             : ULONG;
    dwMediaSampleContent  : DWORD;
    ulSubstreamFilterValue: ULONG;
    iDataOffset           : INT;
  end;
  {$EXTERNALSYM STREAM_ID_MAP}


  // Interface IEnumStreamIdMap
  // ==========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IEnumStreamIdMap);'}
  {$EXTERNALSYM IEnumStreamIdMap}
  IEnumStreamIdMap = interface(IUnknown)
    ['{945C1566-6202-46fc-96C7-D87F289C6534}']

      function Next(cRequest: ULONG;
                    var pStreamIdMap: STREAM_ID_MAP;
                    out pcReceived: ULONG): HResult; stdcall;

      function Skip(cRecords: ULONG): HResult; stdcall;

      function Reset(): HResult; stdcall;

      function Clone(out ppIEnumStreamIdMap: IEnumStreamIdMap): HResult; stdcall;

  end;
  IID_IEnumStreamIdMap = IEnumStreamIdMap;
  {$EXTERNALSYM IID_IEnumStreamIdMap}


  // Interface IMPEG2StreamIdMap
  // ===========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMPEG2StreamIdMap);'}
  {$EXTERNALSYM IMPEG2StreamIdMap}
  IMPEG2StreamIdMap = interface(IUnknown)
    ['{D0E04C47-25B8-4369-925A-362A01D95444}']

      function MapStreamId(ulStreamId: ULONG;
                           MediaSampleContent: DWORD;
                           ulSubstreamFilterValue: ULONG;
                           iDataOffset: INT): HResult; stdcall;

      function UnmapStreamId(culStreamId: ULONG;
                             var pulStreamId: ULONG): HResult; stdcall;  // reads culStreamId

      function EnumStreamIdMap(out ppIEnumStreamIdMap: IEnumStreamIdMap): HResult; stdcall;

  end;
  IID_IMPEG2StreamIdMap = IMPEG2StreamIdMap;
  {$EXTERNALSYM IID_IMPEG2StreamIdMap}


  // Interface IRegisterServiceProvider
  // ==================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IRegisterServiceProvider);'}
  {$EXTERNALSYM IRegisterServiceProvider}
  IRegisterServiceProvider = interface(IUnknown)
    ['{7B3A2F01-0751-48DD-B556-004785171C54}']

      function RegisterService(const guidService: TGUID;
                               pUnkObject: IUnknown): HResult; stdcall;

  end;
  IID_IRegisterServiceProvider = IRegisterServiceProvider;
  {$EXTERNALSYM IID_IRegisterServiceProvider}


  // Interface IAMClockSlave
  // =======================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IAMClockSlave);'}
  {$EXTERNALSYM IAMClockSlave}
  IAMClockSlave = interface(IUnknown)
    ['{9FD52741-176D-4b36-8F51-CA8F933223BE}']

      function SetErrorTolerance(dwTolerance: DWORD): HResult; stdcall;

      function GetErrorTolerance(out dwTolerance: DWORD): HResult; stdcall;

  end;
  IID_IAMClockSlave = IAMClockSlave;
  {$EXTERNALSYM IID_IAMClockSlave}


  // Interface IAMGraphBuilderCallback
  // =================================
  // The IAMGraphBuilderCallback interface provides a callback mechanism during graph building.
  // To use this interface, implement the interface in your application or client object.
  // Query the Filter Graph Manager for the IObjectWithSite interface and call the IObjectWithSite.SetSite
  // method with a pointer to your implementation of the interface.
  // The Filter Graph Manager calls the methods on this interface while it builds the graph,
  // which gives the client the opportunity to modify the graph-building process.
  //
  // The primary use for this interface is to configure the Video Mixing Renderer filter before it is connected.
  //
  // You can also use it reject a specific filter during graph building, such as a decoder filter.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IAMGraphBuilderCallback);'}
  {$EXTERNALSYM IAMGraphBuilderCallback}
  IAMGraphBuilderCallback = interface(IUnknown)
    ['{4995f511-9ddb-4f12-bd3b-f04611807b79}']

    function SelectedFilter(pMon: IMoniker): HResult; stdcall;
    // Called after the Filter Graph Manager creates a filter, but before it tries to connect the filter.

    function CreatedFilter(pFil: IBaseFilter): HResult; stdcall;
    // Called when the Filter Graph Manager finds a candidate filter, but before it creates the filter.
  end;
  IID_IAMGraphBuilderCallback = IAMGraphBuilderCallback;
  {$EXTERNALSYM IID_IAMGraphBuilderCallback}


  // Interface IAMFilterGraphCallback
  // ================================
  // The Filter Graph Manager holds a graph-wide critical section while it calls this method. T
  // herefore, the callback method should avoid calling any methods on the
  // Filter Graph Manager, or any methods on filters that might change the
  // graph state (such as disconnecting pins).
  // Doing so might cause a deadlock or other unexpected behaviors.
  // However, it is safe to query the pin for an interface or check the pin's preferred media type.
  // The main use for this method would be to register a new filter, such as a decoder.
  //
  // This method uses the thiscall calling convention, rather than stdcall.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IAMFilterGraphCallback);'}
  {$EXTERNALSYM IAMFilterGraphCallback}
  IAMFilterGraphCallback = interface(IUnknown)
   ['{56a868fd-0ad4-11ce-b0a3-0020af0ba770}']

      function UnableToRender(Pin: IPin): HResult;
      // S_OK means rendering complete, S_FALSE means retry now.
  end;
  IID_IAMFilterGraphCallback = IAMFilterGraphCallback;
  {$EXTERNALSYM IID_IAMFilterGraphCallback}


  // Interface IGetCapabilitiesKey
  // =============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IGetCapabilitiesKey);'}
  {$EXTERNALSYM IGetCapabilitiesKey}
  IGetCapabilitiesKey = interface(IUnknown)
    ['{a8809222-07bb-48ea-951c-33158100625b}']

      function GetCapabilitiesKey(var pHKey: HKEY): HResult; stdcall;

  end;
  IID_IGetCapabilitiesKey = IGetCapabilitiesKey;
  {$EXTERNALSYM IID_IGetCapabilitiesKey}


  // Interface IEncoderAPI
  // =====================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IEncoderAPI);'}
  {$EXTERNALSYM IEncoderAPI}
  IEncoderAPI = interface(IUnknown)
    ['{70423839-6ACC-4b23-B079-21DBF08156A5}']

      function IsSupported(const Api: TGUID): HResult; stdcall;

      function IsAvailable(const Api: TGUID): HResult; stdcall;

      function GetParameterRange(const Api: TGUID;
                                 out ValueMin: VARIANT;
                                 out ValueMax: VARIANT;
                                 out SteppingDelta: VARIANT): HResult; stdcall;

      function GetParameterValues(const Api: TGUID;
                                  out Values: VARIANT;
                                  out ValuesCount: ULONG): HResult; stdcall;

      function GetDefaultValue(const Api: TGUID;
                               out Value: VARIANT): HResult; stdcall;

      function GetValue(const Api: TGUID;
                        out Value: VARIANT): HResult; stdcall;

      function SetValue(const Api: TGUID;
                        Value: VARIANT): HResult; stdcall;

  end;
  IID_IEncoderAPI = IEncoderAPI;
  {$EXTERNALSYM IID_IEncoderAPI}


  // Interface IVideoEncoder
  // =======================
  // IVideoEncoder may be altered or unavailable in subsequent versions.
  // The IVideoEncoder interface is optionally exposed by video encoder filters.
  //
  // Members
  //  The IVideoEncoder interface inherits from IEncoderAPI but does not have additional members.
  //
  // Remarks
  //  The original purpose of this interface was to enable application to determine whether a
  //  filter was a video decoder, by calling QueryInterface for the IVideoEncoder interface.
  //  The application could then use the IEncoderAPI interface (which IVideoEncoder inherits) to set
  //  properties on the encoder. However, IEncoderAPI is deprecated.
  //  Encoder filters should expose ICodecAPI instead, and applications should use ICodecAPI to
  //  configure encoders.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IVideoEncoder);'}
  {$EXTERNALSYM IVideoEncoder}
  IVideoEncoder = interface(IEncoderAPI)
    ['{02997C3B-8E1B-460e-9270-545E0DE9563E}']

  end;
  IID_IVideoEncoder = IVideoEncoder;
  {$EXTERNALSYM IID_IVideoEncoder}


  PVIDEOENCODER_BITRATE_MODE = ^VIDEOENCODER_BITRATE_MODE;
  VIDEOENCODER_BITRATE_MODE = (
    ConstantBitRate        = 0,
    VariableBitRateAverage = (ConstantBitRate + 1),
    VariableBitRatePeak    = (VariableBitRateAverage + 1)
  );
  {$EXTERNALSYM VIDEOENCODER_BITRATE_MODE}


  // Interface IAMDecoderCaps
  // ========================
  // The IAMDecoderCaps interface returns capabilities information from an MPEG decoder filter.
  // The capabilities reported through this interface include whether the decoder supports the
  // Video Mixing Renderer filter and whether it supports DirectX Video Acceleration.
  //
  // Some DirectShow components, such as the DVD Graph Builder, use this interface to
  // determine the correct filter graph to build.
  // Applications might use this interface to query the decoder's capabilities.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IAMDecoderCaps);'}
  {$EXTERNALSYM IAMDecoderCaps}
  IAMDecoderCaps = interface(IUnknown)
    ['{c0dff467-d499-4986-972b-e1d9090fa941}']

      function GetDecoderCaps(dwCapIndex: DWORD { see constants IAMDecoderCaps };
                              out lpdwCap: DWORD): HResult; stdcall;
      // Queries the decoder for its capabilities.
      // dwCapIndex [in]
      //   Specifies the capability being queried for.
      //
      // lpdwCap [out]
      //   Receives one of the following values:
      //     DECODER_CAP_NOTSUPPORTED;  The decoder does not support this capability.
      //     DECODER_CAP_SUPPORTED;  The decoder supports this capability.
  end;
  IID_IAMDecoderCaps = IAMDecoderCaps;
  {$EXTERNALSYM IID_IAMDecoderCaps}


  PAMCOPPSignature = ^AMCOPPSignature;
  _AMCOPPSignature = record
    Signature: array[0..255] of Byte;
  end;
  {$EXTERNALSYM _AMCOPPSignature}
  AMCOPPSignature = _AMCOPPSignature;
  {$EXTERNALSYM AMCOPPSignature}

  PAMCOPPCommand = ^AMCOPPCommand;
  _AMCOPPCommand = record
    macKDI: TGUID;
    guidCommandID: TGUID;
    dwSequence: DWORD;
    cbSizeData: DWORD;
    CommandData: array[0..4055] of Byte;
  end;
  {$EXTERNALSYM _AMCOPPCommand}
  AMCOPPCommand = _AMCOPPCommand;
  {$EXTERNALSYM AMCOPPCommand}

  PAMCOPPStatusInput = ^AMCOPPStatusInput;
  _AMCOPPStatusInput = record
    rApp: TGUID;
    guidStatusRequestID: TGUID;
    dwSequence: DWORD;
    cbSizeData: DWORD;
    StatusData: array[0..4055] of Byte;
  end;
  {$EXTERNALSYM _AMCOPPStatusInput}
  AMCOPPStatusInput = _AMCOPPStatusInput;
  {$EXTERNALSYM AMCOPPStatusInput}

  PAMCOPPStatusOutput = ^AMCOPPStatusOutput;
  _AMCOPPStatusOutput = record
    macKDI: TGUID;
    cbSizeData: DWORD;
    COPPStatus: array[0..4075] of Byte;
  end;
  {$EXTERNALSYM _AMCOPPStatusOutput}
  AMCOPPStatusOutput = _AMCOPPStatusOutput;
  {$EXTERNALSYM AMCOPPStatusOutput}


  // Interface IAMCertifiedOutputProtection
  // ======================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IAMCertifiedOutputProtection);'}
  {$EXTERNALSYM IAMCertifiedOutputProtection}
  IAMCertifiedOutputProtection = interface(IUnknown)
    ['{6feded3e-0ff1-4901-a2f1-43f7012c8515}']

      function KeyExchange(out pRandom: TGUID;
                           out VarLenCertGH: PByte;
                           out pdwLengthCertGH: DWORD): HResult; stdcall;

      function SessionSequenceStart(pSig: AMCOPPSignature): HResult; stdcall;

      function ProtectionCommand(cmd: AMCOPPCommand): HResult; stdcall;

      function ProtectionStatus(pStatusInput: AMCOPPStatusInput;
                                out pStatusOutput: AMCOPPStatusOutput): HResult; stdcall;

  end;
  IID_IAMCertifiedOutputProtection = IAMCertifiedOutputProtection;
  {$EXTERNALSYM IID_IAMCertifiedOutputProtection}


  // Interface IAMAsyncReaderTimestampScaling
  // ========================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IAMAsyncReaderTimestampScaling);'}
  {$EXTERNALSYM IAMAsyncReaderTimestampScaling}
  IAMAsyncReaderTimestampScaling = interface(IUnknown)
    ['{cf7b26fc-9a00-485b-8147-3e789d5e8f67}']

      function GetTimestampMode(out pfRaw: BOOL): HResult; stdcall;

      function SetTimestampMode(fRaw: BOOL): HResult; stdcall;

  end;
  IID_IAMAsyncReaderTimestampScaling = IAMAsyncReaderTimestampScaling;
  {$EXTERNALSYM IID_IAMAsyncReaderTimestampScaling}


  // Interface IAMPluginControl
  // ==========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IAMPluginControl);'}
  {$EXTERNALSYM IAMPluginControl}
  IAMPluginControl = interface(IUnknown)
    ['{0e26a181-f40c-4635-8786-976284b52981}']

      function GetPreferredClsid(const subType: REFGUID;
                                 out clsid: CLSID): HResult; stdcall;

      function GetPreferredClsidByIndex(const index: DWORD;
                                        out subType: TGUID;
                                        out clsid: CLSID): HResult; stdcall;

      function SetPreferredClsid(const subType: REFGUID;
                                 const clsid: CLSID): HResult; stdcall;

      function IsDisabled(const clsid: REFCLSID): HResult; stdcall;

      function GetDisabledByIndex(index: DWORD;
                                  out clsid: CLSID): HResult; stdcall;

      function SetDisabled(const clsid: REFCLSID;
                           disabled: BOOL): HResult; stdcall;

      function IsLegacyDisabled(const dllName: LPCWSTR): HResult; stdcall;

  end;
  IID_IAMPluginControl = IAMPluginControl;
  {$EXTERNALSYM IID_IAMPluginControl}


  // Interface IPinConnection
  // ========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IPinConnection);'}
  {$EXTERNALSYM IPinConnection}
  IPinConnection = interface(IUnknown)
    ['{4a9a62d3-27d4-403d-91e9-89f540e55534}']

      function DynamicQueryAccept(pmt: AM_MEDIA_TYPE): HResult; stdcall;

      function NotifyEndOfStream(hNotifyEvent: THandle): HResult; stdcall;

      function IsEndPin(): HResult; stdcall;

      function DynamicDisconnect(): HResult; stdcall;

  end;
  IID_IPinConnection = IPinConnection;
  {$EXTERNALSYM IID_IPinConnection}


  // Interface IPinFlowControl
  // =========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IPinFlowControl);'}
  {$EXTERNALSYM IPinFlowControl}
  IPinFlowControl = interface(IUnknown)
    ['{c56e9858-dbf3-4f6b-8119-384af2060deb}']

      function Block(dwBlockFlags: DWORD;
                     hEvent: THandle): HResult; stdcall;

  end;
  IID_IPinFlowControl = IPinFlowControl;
  {$EXTERNALSYM IID_IPinFlowControl}


  PAM_PIN_FLOW_CONTROL_BLOCK_FLAGS = ^_AM_PIN_FLOW_CONTROL_BLOCK_FLAGS;
  _AM_PIN_FLOW_CONTROL_BLOCK_FLAGS = (
    AM_PIN_FLOW_CONTROL_BLOCK  = $1
  );
  {$EXTERNALSYM _AM_PIN_FLOW_CONTROL_BLOCK_FLAGS}
  AM_PIN_FLOW_CONTROL_BLOCK_FLAGS = _AM_PIN_FLOW_CONTROL_BLOCK_FLAGS;


  PAM_GRAPH_CONFIG_RECONNECT_FLAGS = ^AM_GRAPH_CONFIG_RECONNECT_FLAGS;
  _AM_GRAPH_CONFIG_RECONNECT_FLAGS                    = (
    AM_GRAPH_CONFIG_RECONNECT_DIRECTCONNECT           = $1,
    AM_GRAPH_CONFIG_RECONNECT_CACHE_REMOVED_FILTERS   = $2,
    AM_GRAPH_CONFIG_RECONNECT_USE_ONLY_CACHED_FILTERS = $4
  );
  {$EXTERNALSYM _AM_GRAPH_CONFIG_RECONNECT_FLAGS}
  AM_GRAPH_CONFIG_RECONNECT_FLAGS = _AM_GRAPH_CONFIG_RECONNECT_FLAGS;
  {$EXTERNALSYM AM_GRAPH_CONFIG_RECONNECT_FLAGS}


  PREM_FILTER_FLAGS = ^_REM_FILTER_FLAGS;
  _REM_FILTER_FLAGS = (
        REMFILTERF_LEAVECONNECTED  = $1
  );
  {$EXTERNALSYM _REM_FILTER_FLAGS}
  REM_FILTER_FLAGS = _REM_FILTER_FLAGS;


  PAM_FILTER_FLAGS = ^AM_FILTER_FLAGS;
  _AM_FILTER_FLAGS            = (
    AM_FILTER_FLAGS_REMOVABLE = $1
  );
  {$EXTERNALSYM _AM_FILTER_FLAGS}
  AM_FILTER_FLAGS = _AM_FILTER_FLAGS;
  {$EXTERNALSYM AM_FILTER_FLAGS}


  // Interface IGraphConfig
  // ======================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IGraphConfig);'}
  {$EXTERNALSYM IGraphConfig}
  IGraphConfig = interface(IUnknown)
    ['{03A1EB8E-32BF-4245-8502-114D08A9CB88}']

      function Reconnect(pOutputPin: IPin;
                         pInputPin: IPin;
                         pmtFirstConnection: AM_MEDIA_TYPE;
                         pUsingFilter: IBaseFilter;
                         hAbortEvent: THandle;
                         dwFlags: DWORD { PAM_GRAPH_CONFIG_RECONNECT_FLAGS } ): HResult; stdcall;

      function Reconfigure(pCallback: IGraphConfigCallback;
                           pvContext: Pointer;
                           dwFlags: DWORD;
                           hAbortEvent: THandle): HResult; stdcall;

      function AddFilterToCache(pFilter: IBaseFilter): HResult; stdcall;

      function EnumCacheFilter(out pEnum: IEnumFilters): HResult; stdcall;

      function RemoveFilterFromCache(pFilter: IBaseFilter): HResult; stdcall;

      function GetStartTime(out prtStart: REFERENCE_TIME): HResult; stdcall;

      function PushThroughData(pOutputPin: IPin;
                               pConnection: IPinConnection;
                               const hEventAbort: THandle): HResult; stdcall;

      function SetFilterFlags(pFilter: IBaseFilter;
                              const dwFlags: DWORD): HResult; stdcall;

      function GetFilterFlags(pFilter: IBaseFilter;
                              out pdwFlags: DWORD): HResult; stdcall;

      function RemoveFilterEx(pFilter: IBaseFilter;
                              const Flags: DWORD): HResult; stdcall;

  end;
  IID_IGraphConfig = IGraphConfig;
  {$EXTERNALSYM IID_IGraphConfig}


  // Interface IGraphConfigCallback
  // ==============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IGraphConfigCallback);'}
  {$EXTERNALSYM IGraphConfigCallback}
  IGraphConfigCallback = interface(IUnknown)
    ['{ade0fd60-d19d-11d2-abf6-00a0c905f375}']

      function Reconfigure(pvContext: Pointer;
                           dwFlags: DWORD): HResult; stdcall;
  end;
  IID_IGraphConfigCallback = IGraphConfigCallback;
  {$EXTERNALSYM IID_IGraphConfigCallback}


  // Interface IFilterChain
  // ======================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IFilterChain);'}
  {$EXTERNALSYM IFilterChain}
  IFilterChain = interface(IUnknown)
    ['{DCFBDCF6-0DC2-45f5-9AB2-7C330EA09C29}']

      function StartChain(pStartFilter: IBaseFilter;
                          pEndFilter: IBaseFilter): HResult; stdcall;

      function PauseChain(pStartFilter: IBaseFilter;
                          pEndFilter: IBaseFilter): HResult; stdcall;

      function StopChain(pStartFilter: IBaseFilter;
                         pEndFilter: IBaseFilter): HResult; stdcall;

      function RemoveChain(pStartFilter: IBaseFilter;
                           pEndFilter: IBaseFilter): HResult; stdcall;

  end;
  IID_IFilterChain = IFilterChain;
  {$EXTERNALSYM IID_IFilterChain}


  LPDIRECTDRAW7 = ^DWORD;
  {$EXTERNALSYM LPDIRECTDRAW7}
  LPDIRECTDRAWSURFACE7 = ^DWORD;
  {$EXTERNALSYM LPDIRECTDRAWSURFACE7}
  LPDDPIXELFORMAT = ^DWORD;
  {$EXTERNALSYM LPDDPIXELFORMAT}
  LPBITMAPINFOHEADER = ^DWORD;
  {$EXTERNALSYM LPBITMAPINFOHEADER}

  PDdcolorkey = ^DDCOLORKEY;
  DDCOLORKEY = record
    dw1: DWORD;
    dw2: DWORD;
  end;
  {$EXTERNALSYM DDCOLORKEY}

  LPDDCOLORKEY = ^DDCOLORKEY;

  PVMRPresentationFlags = ^VMRPresentationFlags;
  VMRPresentationFlags         = (
    VMRSample_SyncPoint        = $1,
    VMRSample_Preroll          = $2,
    VMRSample_Discontinuity    = $4,
    VMRSample_TimeValid        = $8,
    VMRSample_SrcDstRectsValid = $10);
  {$EXTERNALSYM VMRPresentationFlags}

  PVMRPRESENTATIONINFO = ^VMRPRESENTATIONINFO;
  tagVMRPRESENTATIONINFO = record
    dwFlags: DWORD;
    lpSurf: LPDIRECTDRAWSURFACE7;
    rtStart: REFERENCE_TIME;
    rtEnd: REFERENCE_TIME;
    szAspectRatio: SIZE;
    rcSrc: TRect;
    rcDst: TRect;
    dwTypeSpecificFlags: DWORD;
    dwInterlaceFlags: DWORD;
  end;
  {$EXTERNALSYM tagVMRPRESENTATIONINFO}
  VMRPRESENTATIONINFO = tagVMRPRESENTATIONINFO;
  {$EXTERNALSYM VMRPRESENTATIONINFO}


  // Interface IVMRImagePresenter
  // ============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IVMRImagePresenter);'}
  {$EXTERNALSYM IVMRImagePresenter}
  IVMRImagePresenter = interface(IUnknown)
    ['{CE704FE7-E71E-41fb-BAA2-C4403E1182F5}']

      function StartPresenting(dwUserID: DWORD_PTR): HResult; stdcall;

      function StopPresenting(dwUserID: DWORD_PTR): HResult; stdcall;

      function PresentImage(dwUserID: DWORD_PTR;
                            lpPresInfo: VMRPRESENTATIONINFO): HResult; stdcall;

  end;
  IID_IVMRImagePresenter = IVMRImagePresenter;
  {$EXTERNALSYM IID_IVMRImagePresenter}


  PVMRSurfaceAllocationFlags = ^VMRSurfaceAllocationFlags;
  VMRSurfaceAllocationFlags = (
    AMAP_PIXELFORMAT_VALID = $1,
    AMAP_3D_TARGET         = $2,
    AMAP_ALLOW_SYSMEM      = $4,
    AMAP_FORCE_SYSMEM      = $8,
    AMAP_DIRECTED_FLIP     = $10,
    AMAP_DXVA_TARGET       = $20);
  {$EXTERNALSYM VMRSurfaceAllocationFlags}

  PVMRALLOCATIONINFO = ^VMRALLOCATIONINFO;
  tagVMRALLOCATIONINFO = record
    dwFlags: DWORD;
    lpHdr: LPBITMAPINFOHEADER;
    lpPixFmt: LPDDPIXELFORMAT;
    szAspectRatio: SIZE;
    dwMinBuffers: DWORD;
    dwMaxBuffers: DWORD;
    dwInterlaceFlags: DWORD;
    szNativeSize: SIZE;
  end;
  {$EXTERNALSYM tagVMRALLOCATIONINFO}
  VMRALLOCATIONINFO = tagVMRALLOCATIONINFO;
  {$EXTERNALSYM VMRALLOCATIONINFO}


  // Interface IVMRSurfaceAllocator
  // ==============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IVMRSurfaceAllocator);'}
  {$EXTERNALSYM IVMRSurfaceAllocator}
  IVMRSurfaceAllocator = interface(IUnknown)
    ['{31ce832e-4484-458b-8cca-f4d7e3db0b52}']

      function AllocateSurface(dwUserID: DWORD_PTR;
                               lpAllocInfo: VMRALLOCATIONINFO;
                               var lpdwActualBuffers: DWORD;
                               out lplpSurface: LPDIRECTDRAWSURFACE7): HResult; stdcall;

      function FreeSurface(dwID: DWORD_PTR): HResult; stdcall;

      function PrepareSurface(dwUserID: DWORD_PTR;
                              lpSurface: LPDIRECTDRAWSURFACE7;
                              dwSurfaceFlags: DWORD): HResult; stdcall;

      function AdviseNotify(lpIVMRSurfAllocNotify: IVMRSurfaceAllocatorNotify): HResult; stdcall;

  end;
  IID_IVMRSurfaceAllocator = IVMRSurfaceAllocator;
  {$EXTERNALSYM IID_IVMRSurfaceAllocator}


  // Interface IVMRSurfaceAllocatorNotify
  // ====================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IVMRSurfaceAllocatorNotify);'}
  {$EXTERNALSYM IVMRSurfaceAllocatorNotify}
  IVMRSurfaceAllocatorNotify = interface(IUnknown)
    ['{aada05a8-5a4e-4729-af0b-cea27aed51e2}']

      function AdviseSurfaceAllocator(dwUserID: DWORD_PTR;
                                      lpIVRMSurfaceAllocator: IVMRSurfaceAllocator): HResult; stdcall;

      function SetDDrawDevice(lpDDrawDevice: LPDIRECTDRAW7;
                              hMonitor: HMONITOR): HResult; stdcall;

      function ChangeDDrawDevice(lpDDrawDevice: LPDIRECTDRAW7;
                                 hMonitor: HMONITOR): HResult; stdcall;

      function RestoreDDrawSurfaces(): HResult; stdcall;

      function NotifyEvent(EventCode: LONG;
                           Param1: LONG_PTR;
                           Param2: LONG_PTR): HResult; stdcall;

      function SetBorderColor(clrBorder: COLORREF): HResult; stdcall;

  end;
  IID_IVMRSurfaceAllocatorNotify = IVMRSurfaceAllocatorNotify;
  {$EXTERNALSYM IID_IVMRSurfaceAllocatorNotify}


  PVMR_ASPECT_RATIO_MODE = ^VMR_ASPECT_RATIO_MODE;
  VMR_ASPECT_RATIO_MODE   = (
    VMR_ARMODE_NONE       = 0,
    VMR_ARMODE_LETTER_BOX = (VMR_ARMODE_NONE + 1));
  {$EXTERNALSYM VMR_ASPECT_RATIO_MODE}


  // Interface IVMRWindowlessControl
  // ===============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IVMRWindowlessControl);'}
  {$EXTERNALSYM IVMRWindowlessControl}
  IVMRWindowlessControl = interface(IUnknown)
    ['{0eb1088c-4dcd-46f0-878f-39dae86a51b7}']

      function GetNativeVideoSize(out lpWidth: LONG;
                                  out lpHeight: LONG;
                                  out lpARWidth: LONG;
                                  out lpARHeight: LONG): HResult; stdcall;

      function GetMinIdealVideoSize(out lpWidth: LONG;
                                    out lpHeight: LONG): HResult; stdcall;

      function GetMaxIdealVideoSize(out lpWidth: LONG;
                                    out lpHeight: LONG): HResult; stdcall;

      function SetVideoPosition(lpSRCRect: LPRECT;
                                lpDSTRect: LPRECT): HResult; stdcall;

      function GetVideoPosition(out lpSRCRect: LPRECT;
                                out lpDSTRect: LPRECT): HResult; stdcall;

      function GetAspectRatioMode(out lpAspectRatioMode: DWORD): HResult; stdcall;

      function SetAspectRatioMode(AspectRatioMode: DWORD): HResult; stdcall;

      function SetVideoClippingWindow(const hwnd: HWND): HResult; stdcall;

      function RepaintVideo(const hwnd: HWND;
                            const hdc: HDC): HResult; stdcall;

      function DisplayModeChanged(): HResult; stdcall;

      function GetCurrentImage(out lpDib: PByte): HResult; stdcall;

      function SetBorderColor(Clr: COLORREF): HResult; stdcall;

      function GetBorderColor(out lpClr: COLORREF): HResult; stdcall;

      function SetColorKey(Clr: COLORREF): HResult; stdcall;

      function GetColorKey(out lpClr: COLORREF): HResult; stdcall;

  end;
  IID_IVMRWindowlessControl = IVMRWindowlessControl;
  {$EXTERNALSYM IID_IVMRWindowlessControl}


  PVMRMixerPrefs = ^VMRMixerPrefs;
  VMRMixerPrefs                    = (
    MixerPref_NoDecimation         = $1,
    MixerPref_DecimateOutput       = $2,
    MixerPref_ARAdjustXorY         = $4,
    MixerPref_DecimationReserved   = $8,
    MixerPref_DecimateMask         = $F,
    MixerPref_BiLinearFiltering    = $10,
    MixerPref_PointFiltering       = $20,
    MixerPref_FilteringMask        = $F0,
    MixerPref_RenderTargetRGB      = $100,
    MixerPref_RenderTargetYUV      = $1000,
    MixerPref_RenderTargetYUV420   = $200,
    MixerPref_RenderTargetYUV422   = $400,
    MixerPref_RenderTargetYUV444   = $800,
    MixerPref_RenderTargetReserved = $E000,
    MixerPref_RenderTargetMask     = $FF00,
    MixerPref_DynamicSwitchToBOB   = $10000,
    MixerPref_DynamicDecimateBy2   = $20000,
    MixerPref_DynamicReserved      = $C0000,
    MixerPref_DynamicMask          = $F0000);
  {$EXTERNALSYM VMRMixerPrefs}

  // same as IMFVideoMediaType
{$IFNDEF MFVideoNormalizedRect}
  PNORMALIZEDRECT = ^NORMALIZEDRECT;
  _NORMALIZEDRECT = record
    left: Single;
    top: Single;
    right: Single;
    bottom: Single;
  end;
  NORMALIZEDRECT = _NORMALIZEDRECT;
{$ELSE}
  PNORMALIZEDRECT = ^NORMALIZEDRECT;
  NORMALIZEDRECT = MFVideoNormalizedRect;
  {$EXTERNALSYM NORMALIZEDRECT}
{$ENDIF}


  // Interface IVMRMixerControl
  // ==========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IVMRMixerControl);'}
  {$EXTERNALSYM IVMRMixerControl}
  IVMRMixerControl = interface(IUnknown)
    ['{1c1a17b0-bed0-415d-974b-dc6696131599}']

      function SetAlpha(dwStreamID: DWORD;
                        Alpha: Single): HResult; stdcall;

      function GetAlpha(dwStreamID: DWORD;
                        out pAlpha: Single): HResult; stdcall;

      function SetZOrder(dwStreamID: DWORD;
                         dwZ: DWORD): HResult; stdcall;

      function GetZOrder(dwStreamID: DWORD;
                         out pZ: DWORD): HResult; stdcall;

      function SetOutputRect(dwStreamID: DWORD;
                             pRect: NORMALIZEDRECT): HResult; stdcall;

      function GetOutputRect(dwStreamID: DWORD;
                             out pRect: NORMALIZEDRECT): HResult; stdcall;

      function SetBackgroundClr(const ClrBkg: COLORREF): HResult; stdcall;

      function GetBackgroundClr(out lpClrBkg: COLORREF): HResult; stdcall;

      function SetMixingPrefs(dwMixerPrefs: DWORD): HResult; stdcall;

      function GetMixingPrefs(out pdwMixerPrefs: DWORD): HResult; stdcall;

  end;
  IID_IVMRMixerControl = IVMRMixerControl;
  {$EXTERNALSYM IID_IVMRMixerControl}


  PVMRGUID = ^VMRGUID;
  tagVMRGUID = record
    ppGUID: PGUID;
    ttGUID: TGUID;
  end;
  {$EXTERNALSYM tagVMRGUID}
  VMRGUID = tagVMRGUID;
  {$EXTERNALSYM VMRGUID}


  PVMRMONITORINFO = ^VMRMONITORINFO;
  tagVMRMONITORINFO = record
    guid: VMRGUID;
    rcMonitor: TRect;
    hMon: HMONITOR;
    dwFlags: DWORD;
    szDevice: array[0..31] of WideChar;
    szDescription: array[0..255] of WideChar;
    liDriverVersion: LARGE_INTEGER;
    dwVendorId: DWORD;
    dwDeviceId: DWORD;
    dwSubSysId: DWORD;
    dwRevision: DWORD;
  end;
  {$EXTERNALSYM tagVMRMONITORINFO}
  VMRMONITORINFO = tagVMRMONITORINFO;
  {$EXTERNALSYM VMRMONITORINFO}


  // Interface IVMRMonitorConfig
  // ===========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IVMRMonitorConfig);'}
  {$EXTERNALSYM IVMRMonitorConfig}
  IVMRMonitorConfig = interface(IUnknown)
    ['{9cf0b1b6-fbaa-4b7f-88cf-cf1f130a0dce}']

      function SetMonitor(const pGUID: VMRGUID): HResult; stdcall;

      function GetMonitor(out pGUID: VMRGUID): HResult; stdcall;

      function SetDefaultMonitor(const pGUID: VMRGUID): HResult; stdcall;

      function GetDefaultMonitor(out pGUID: VMRGUID): HResult; stdcall;

      function GetAvailableMonitors(out pInfo: VMRMONITORINFO;
                                    dwMaxInfoArraySize: DWORD;
                                    out pdwNumDevices: DWORD): HResult; stdcall;

  end;
  IID_IVMRMonitorConfig = IVMRMonitorConfig;
  {$EXTERNALSYM IID_IVMRMonitorConfig}


  PVMRRenderPrefs = ^VMRRenderPrefs;
  VMRRenderPrefs                             = (
    RenderPrefs_RestrictToInitialMonitor     = 0,
    RenderPrefs_ForceOffscreen               = $1,
    RenderPrefs_ForceOverlays                = $2,
    RenderPrefs_AllowOverlays                = 0,
    RenderPrefs_AllowOffscreen               = 0,
    RenderPrefs_DoNotRenderColorKeyAndBorder = $8,
    RenderPrefs_Reserved                     = $10,
    RenderPrefs_PreferAGPMemWhenMixing       = $20,
    RenderPrefs_Mask                         = $3F);
  {$EXTERNALSYM VMRRenderPrefs}


  PVMRMode = ^VMRMode;
  VMRMode              = (
    VMRMode_Windowed   = $1,
    VMRMode_Windowless = $2,
    VMRMode_Renderless = $4,
    VMRMode_Mask       = $7);
  {$EXTERNALSYM VMRMode}


  // Interface IVMRFilterConfig
  // ==========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IVMRFilterConfig);'}
  {$EXTERNALSYM IVMRFilterConfig}
  IVMRFilterConfig = interface(IUnknown)
    ['{9e5530c5-7034-48b4-bb46-0b8a6efc8e36}']

      function SetImageCompositor(lpVMRImgCompositor: IVMRImageCompositor): HResult; stdcall;

      function SetNumberOfStreams(dwMaxStreams: DWORD): HResult; stdcall;

      function GetNumberOfStreams(out pdwMaxStreams: DWORD): HResult; stdcall;

      function SetRenderingPrefs(dwRenderFlags: DWORD): HResult; stdcall;

      function GetRenderingPrefs(out pdwRenderFlags: DWORD): HResult; stdcall;

      function SetRenderingMode(Mode: DWORD): HResult; stdcall;

      function GetRenderingMode(out pMode: DWORD): HResult; stdcall;

  end;
  IID_IVMRFilterConfig = IVMRFilterConfig;
  {$EXTERNALSYM IID_IVMRFilterConfig}


  // Interface IVMRAspectRatioControl
  // ================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IVMRAspectRatioControl);'}
  {$EXTERNALSYM IVMRAspectRatioControl}
  IVMRAspectRatioControl = interface(IUnknown)
    ['{ede80b5c-bad6-4623-b537-65586c9f8dfd}']

      function GetAspectRatioMode(out lpdwARMode: DWORD): HResult; stdcall;

      function SetAspectRatioMode(dwARMode: DWORD): HResult; stdcall;

  end;
  IID_IVMRAspectRatioControl = IVMRAspectRatioControl;
  {$EXTERNALSYM IID_IVMRAspectRatioControl}


  PVMRDeinterlacePrefs = ^VMRDeinterlacePrefs;
  VMRDeinterlacePrefs        = (
    DeinterlacePref_NextBest = $1,
    DeinterlacePref_BOB      = $2,
    DeinterlacePref_Weave    = $4,
    DeinterlacePref_Mask     = $7);
  {$EXTERNALSYM VMRDeinterlacePrefs}


  PVMRDeinterlaceTech = ^VMRDeinterlaceTech;
  VMRDeinterlaceTech                    = (
    DeinterlaceTech_Unknown             = 0,
    DeinterlaceTech_BOBLineReplicate    = $1,
    DeinterlaceTech_BOBVerticalStretch  = $2,
    DeinterlaceTech_MedianFiltering     = $4,
    DeinterlaceTech_EdgeFiltering       = $10,
    DeinterlaceTech_FieldAdaptive       = $20,
    DeinterlaceTech_PixelAdaptive       = $40,
    DeinterlaceTech_MotionVectorSteered = $80);
  {$EXTERNALSYM VMRDeinterlaceTech}


  PVMRFrequency = ^VMRFrequency;
  _VMRFrequency = record
    dwNumerator: DWORD;
    dwDenominator: DWORD;
  end;
  {$EXTERNALSYM _VMRFrequency}
  VMRFrequency = _VMRFrequency;
  {$EXTERNALSYM VMRFrequency}


  PVMRVideoDesc = ^VMRVideoDesc;
  _VMRVideoDesc = record
    dwSize: DWORD;
    dwSampleWidth: DWORD;
    dwSampleHeight: DWORD;
    SingleFieldPerSample: BOOL;
    dwFourCC: DWORD;
    InputSampleFreq: VMRFrequency;
    OutputFrameFreq: VMRFrequency;
  end;
  {$EXTERNALSYM _VMRVideoDesc}
  VMRVideoDesc = _VMRVideoDesc;
  {$EXTERNALSYM VMRVideoDesc}


  PVMRDeinterlaceCaps = ^VMRDeinterlaceCaps;
  _VMRDeinterlaceCaps = record
    dwSize: DWORD;
    dwNumPreviousOutputFrames: DWORD;
    dwNumForwardRefSamples: DWORD;
    dwNumBackwardRefSamples: DWORD;
    DeinterlaceTechnology: VMRDeinterlaceTech;
  end;
  {$EXTERNALSYM _VMRDeinterlaceCaps}
  VMRDeinterlaceCaps = _VMRDeinterlaceCaps;
  {$EXTERNALSYM VMRDeinterlaceCaps}


  // Interface IVMRDeinterlaceControl
  // ================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IVMRDeinterlaceControl);'}
  {$EXTERNALSYM IVMRDeinterlaceControl}
  IVMRDeinterlaceControl = interface(IUnknown)
    ['{bb057577-0db8-4e6a-87a7-1a8c9a505a0f}']

      function GetNumberOfDeinterlaceModes(lpVideoDescription: VMRVideoDesc;
                                           var lpdwNumDeinterlaceModes: DWORD;
                                           const lpDeinterlaceModes: LPGUID): HResult; stdcall;

      function GetDeinterlaceModeCaps(const lpDeinterlaceMode: TGUID;
                                      lpVideoDescription: VMRVideoDesc;
                                      var lpDeinterlaceCaps: VMRDeinterlaceCaps): HResult; stdcall;

      function GetDeinterlaceMode(dwStreamID: DWORD;
                                  out lpDeinterlaceMode: TGUID): HResult; stdcall;

      function SetDeinterlaceMode(dwStreamID: DWORD;
                                  const lpDeinterlaceMode: TGUID): HResult; stdcall;

      function GetDeinterlacePrefs(out lpdwDeinterlacePrefs: DWORD): HResult; stdcall;

      function SetDeinterlacePrefs(dwDeinterlacePrefs: DWORD): HResult; stdcall;

      function GetActualDeinterlaceMode(dwStreamID: DWORD;
                                        out lpDeinterlaceMode: TGUID): HResult; stdcall;

  end;
  IID_IVMRDeinterlaceControl = IVMRDeinterlaceControl;
  {$EXTERNALSYM IID_IVMRDeinterlaceControl}


  PVMRALPHABITMAP = ^VMRALPHABITMAP;
  _VMRALPHABITMAP = record
    dwFlags: DWORD;
    hdc: HDC;
    pDDS: LPDIRECTDRAWSURFACE7;
    rSrc: TRect;
    rDest: NORMALIZEDRECT; // = MFVideoNormalizedRect = TRectF
    fAlpha: FLOAT;
    clrSrcKey: COLORREF;
  end;
  {$EXTERNALSYM _VMRALPHABITMAP}
  VMRALPHABITMAP = _VMRALPHABITMAP;
  {$EXTERNALSYM VMRALPHABITMAP}


  // Interface IVMRMixerBitmap
  // =========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IVMRMixerBitmap);'}
  {$EXTERNALSYM IVMRMixerBitmap}
  IVMRMixerBitmap = interface(IUnknown)
    ['{1E673275-0257-40aa-AF20-7C608D4A0428}']

      function SetAlphaBitmap(pBmpParms: VMRALPHABITMAP): HResult; stdcall;

      function UpdateAlphaBitmapParameters(pBmpParms: PVMRALPHABITMAP): HResult; stdcall;

      function GetAlphaBitmapParameters(out pBmpParms: PVMRALPHABITMAP): HResult; stdcall;

  end;
  IID_IVMRMixerBitmap = IVMRMixerBitmap;
  {$EXTERNALSYM IID_IVMRMixerBitmap}


  PVMRVIDEOSTREAMINFO = ^VMRVIDEOSTREAMINFO;
  _VMRVIDEOSTREAMINFO = record
    pddsVideoSurface: LPDIRECTDRAWSURFACE7;
    dwWidth: DWORD;
    dwHeight: DWORD;
    dwStrmID: DWORD;
    fAlpha: FLOAT;
    ddClrKey: DDCOLORKEY;
    rNormal: NORMALIZEDRECT;
  end;
  {$EXTERNALSYM _VMRVIDEOSTREAMINFO}
  VMRVIDEOSTREAMINFO = _VMRVIDEOSTREAMINFO;
  {$EXTERNALSYM VMRVIDEOSTREAMINFO}


  // Interface IVMRImageCompositor
  // =============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IVMRImageCompositor);'}
  {$EXTERNALSYM IVMRImageCompositor}
  IVMRImageCompositor = interface(IUnknown)
    ['{7a4fb5af-479f-4074-bb40-ce6722e43c82}']

      function InitCompositionTarget(pD3DDevice: IUnknown;
                                     pddsRenderTarget: LPDIRECTDRAWSURFACE7): HResult; stdcall;

      function TermCompositionTarget(pD3DDevice: IUnknown;
                                     pddsRenderTarget: LPDIRECTDRAWSURFACE7): HResult; stdcall;

      function SetStreamMediaType(dwStrmID: DWORD;
                                  pmt: AM_MEDIA_TYPE;
                                  fTexture: BOOL): HResult; stdcall;

      function CompositeImage(const pD3DDevice: IUnknown;
                              pddsRenderTarget: LPDIRECTDRAWSURFACE7;
                              pmtRenderTarget: AM_MEDIA_TYPE;
                              rtStart: REFERENCE_TIME;
                              rtEnd: REFERENCE_TIME;
                              dwClrBkGnd: DWORD;
                              pVideoStreamInfo: VMRVIDEOSTREAMINFO;
                              cStreams: UINT): HResult; stdcall;

  end;
  IID_IVMRImageCompositor = IVMRImageCompositor;
  {$EXTERNALSYM IID_IVMRImageCompositor}


  // Interface IVMRVideoStreamControl
  // ================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IVMRVideoStreamControl);'}
  {$EXTERNALSYM IVMRVideoStreamControl}
  IVMRVideoStreamControl = interface(IUnknown)
    ['{058d1f11-2a54-4bef-bd54-df706626b727}']

      function SetColorKey(lpClrKey: LPDDCOLORKEY): HResult; stdcall;

      function GetColorKey(out lpClrKey: LPDDCOLORKEY): HResult; stdcall;

      function SetStreamActiveState(fActive: BOOL): HResult; stdcall;

      function GetStreamActiveState(out lpfActive: BOOL): HResult; stdcall;

  end;
  IID_IVMRVideoStreamControl = IVMRVideoStreamControl;
  {$EXTERNALSYM IID_IVMRVideoStreamControl}


  // Interface IVMRSurface
  // =====================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IVMRSurface);'}
  {$EXTERNALSYM IVMRSurface}
  IVMRSurface = interface(IUnknown)
    ['{a9849bbe-9ec8-4263-b764-62730f0d15d0}']

      function IsSurfaceLocked(): HResult; stdcall;

      function LockSurface(out lpSurface: PByte): HResult; stdcall;

      function UnlockSurface(): HResult; stdcall;

      function GetSurface(out lplpSurface: LPDIRECTDRAWSURFACE7): HResult; stdcall;

  end;
  IID_IVMRSurface = IVMRSurface;
  {$EXTERNALSYM IID_IVMRSurface}


  // Interface IVMRImagePresenterConfig
  // ==================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IVMRImagePresenterConfig);'}
  {$EXTERNALSYM IVMRImagePresenterConfig}
  IVMRImagePresenterConfig = interface(IUnknown)
    ['{9f3a1c85-8555-49ba-935f-be5b5b29d178}']

      function SetRenderingPrefs(dwRenderFlags: DWORD): HResult; stdcall;

      function GetRenderingPrefs(out dwRenderFlags: DWORD): HResult; stdcall;

  end;
  IID_IVMRImagePresenterConfig = IVMRImagePresenterConfig;
  {$EXTERNALSYM IID_IVMRImagePresenterConfig}


  // Interface IVMRImagePresenterExclModeConfig
  // ==========================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IVMRImagePresenterExclModeConfig);'}
  {$EXTERNALSYM IVMRImagePresenterExclModeConfig}
  IVMRImagePresenterExclModeConfig = interface(IVMRImagePresenterConfig)
    ['{e6f7ce40-4673-44f1-8f77-5499d68cb4ea}']

      function SetXlcModeDDObjAndPrimarySurface(lpDDObj: LPDIRECTDRAW7;
                                                lpPrimarySurf: LPDIRECTDRAWSURFACE7): HResult; stdcall;

      function GetXlcModeDDObjAndPrimarySurface(out lpDDObj: LPDIRECTDRAW7;
                                                out lpPrimarySurf: LPDIRECTDRAWSURFACE7): HResult; stdcall;

  end;
  IID_IVMRImagePresenterExclModeConfig = IVMRImagePresenterExclModeConfig;
  {$EXTERNALSYM IID_IVMRImagePresenterExclModeConfig}



  // Interface IVPManager
  // ====================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IVPManager);'}
  {$EXTERNALSYM IVPManager}
  IVPManager = interface(IUnknown)
    ['{aac18c18-e186-46d2-825d-a1f8dc8e395a}']

      function SetVideoPortIndex(dwVideoPortIndex: DWORD): HResult; stdcall;

      function GetVideoPortIndex(out pdwVideoPortIndex: DWORD): HResult; stdcall;

  end;
  IID_IVPManager = IVPManager;
  {$EXTERNALSYM IID_IVPManager}


  PDVD_DOMAIN = ^DVD_DOMAIN;
  tagDVD_DOMAIN                  = (
    DVD_DOMAIN_FirstPlay         = 1,
    DVD_DOMAIN_VideoManagerMenu  = (DVD_DOMAIN_FirstPlay + 1),
    DVD_DOMAIN_VideoTitleSetMenu = (DVD_DOMAIN_VideoManagerMenu + 1),
    DVD_DOMAIN_Title             = (DVD_DOMAIN_VideoTitleSetMenu + 1),
    DVD_DOMAIN_Stop              = (DVD_DOMAIN_Title + 1));
  {$EXTERNALSYM tagDVD_DOMAIN}
  DVD_DOMAIN = tagDVD_DOMAIN;
  {$EXTERNALSYM DVD_DOMAIN}


  PDVD_MENU_ID = ^DVD_MENU_ID;
  tagDVD_MENU_ID        = (
    DVD_MENU_Title      = 2,
    DVD_MENU_Root       = 3,
    DVD_MENU_Subpicture = 4,
    DVD_MENU_Audio      = 5,
    DVD_MENU_Angle      = 6,
    DVD_MENU_Chapter    = 7);
  {$EXTERNALSYM tagDVD_MENU_ID}
  DVD_MENU_ID = tagDVD_MENU_ID;
  {$EXTERNALSYM DVD_MENU_ID}


  PDVD_DISC_SIDE = ^DVD_DISC_SIDE;
  tagDVD_DISC_SIDE = (
    DVD_SIDE_A = 1,
    DVD_SIDE_B = 2);
  {$EXTERNALSYM tagDVD_DISC_SIDE}
  DVD_DISC_SIDE = tagDVD_DISC_SIDE;
  {$EXTERNALSYM DVD_DISC_SIDE}


  PDVD_PREFERRED_DISPLAY_MODE = ^DVD_PREFERRED_DISPLAY_MODE;
  tagDVD_PREFERRED_DISPLAY_MODE     = (
    DISPLAY_CONTENT_DEFAULT         = 0,
    DISPLAY_16x9                    = 1,
    DISPLAY_4x3_PANSCAN_PREFERRED   = 2,
    DISPLAY_4x3_LETTERBOX_PREFERRED = 3);
  {$EXTERNALSYM tagDVD_PREFERRED_DISPLAY_MODE}
  DVD_PREFERRED_DISPLAY_MODE = tagDVD_PREFERRED_DISPLAY_MODE;
  {$EXTERNALSYM DVD_PREFERRED_DISPLAY_MODE}


  PDVD_REGISTER = ^DVD_REGISTER;
  DVD_REGISTER = WORD;
  {$EXTERNALSYM DVD_REGISTER}


  GPRMARRAY = array[0..15] of DVD_REGISTER;
  {$EXTERNALSYM GPRMARRAY}

  SPRMARRAY = array[0..23] of DVD_REGISTER;
  {$EXTERNALSYM SPRMARRAY}

  PDVD_ATR = ^DVD_ATR;
  tagDVD_ATR = record
    ulCAT: ULONG;
    pbATRI: array[0..767] of Byte;
  end;
  {$EXTERNALSYM tagDVD_ATR}
  DVD_ATR = tagDVD_ATR;
  {$EXTERNALSYM DVD_ATR}


  DVD_VideoATR = array[0..1] of Byte;
  {$EXTERNALSYM DVD_VideoATR}

  DVD_AudioATR = array[0..7] of Byte;
  {$EXTERNALSYM DVD_AudioATR}

  DVD_SubpictureATR = array[0..5] of Byte;
  {$EXTERNALSYM DVD_SubpictureATR}


  PDVD_FRAMERATE = ^DVD_FRAMERATE;
  tagDVD_FRAMERATE    = (
    DVD_FPS_25        = 1,
    DVD_FPS_30NonDrop = 3);
  {$EXTERNALSYM tagDVD_FRAMERATE}
  DVD_FRAMERATE = tagDVD_FRAMERATE;
  {$EXTERNALSYM DVD_FRAMERATE}


  PDVD_TIMECODE = ^DVD_TIMECODE;
  tagDVD_TIMECODE = record
    Hours1: ULONG;      //:4; // Hours
    Hours10: ULONG;     //:4; // Tens of Hours

    Minutes1: ULONG;    //:4; // Minutes
    Minutes10: ULONG;   //:4; // Tens of Minutes

    Seconds1: ULONG;    //:4; // Seconds
    Seconds10: ULONG;   //:4; // Tens of Seconds

    Frames1: ULONG;     //:4; // Frames
    Frames10: ULONG;    //:2; // Tens of Frames

    FrameRateCode: ULONG; //:2; // use DVD_FRAMERATE to indicate frames/sec and drop/non-drop
  end;
  {$EXTERNALSYM tagDVD_TIMECODE}
  DVD_TIMECODE = tagDVD_TIMECODE;
  {$EXTERNALSYM DVD_TIMECODE}


  PDVD_NavCmdType = ^DVD_NavCmdType;
  tagDVD_NavCmdType       = (
    DVD_NavCmdType_Pre    = 1,
    DVD_NavCmdType_Post   = 2,
    DVD_NavCmdType_Cell   = 3,
    DVD_NavCmdType_Button = 4);
  {$EXTERNALSYM tagDVD_NavCmdType}
  DVD_NavCmdType = tagDVD_NavCmdType;
  {$EXTERNALSYM DVD_NavCmdType}

  PDVD_TIMECODE_FLAGS = ^DVD_TIMECODE_FLAGS;
  tagDVD_TIMECODE_FLAGS      = (
    DVD_TC_FLAG_25fps        = $1,
    DVD_TC_FLAG_30fps        = $2,
    DVD_TC_FLAG_DropFrame    = $4,
    DVD_TC_FLAG_Interpolated = $8);
  {$EXTERNALSYM tagDVD_TIMECODE_FLAGS}
  DVD_TIMECODE_FLAGS = tagDVD_TIMECODE_FLAGS;
  {$EXTERNALSYM DVD_TIMECODE_FLAGS}


  PDVD_HMSF_TIMECODE = ^DVD_HMSF_TIMECODE;
  tagDVD_HMSF_TIMECODE = record
    bHours: Byte;
    bMinutes: Byte;
    bSeconds: Byte;
    bFrames: Byte;
  end;
  {$EXTERNALSYM tagDVD_HMSF_TIMECODE}
  DVD_HMSF_TIMECODE = tagDVD_HMSF_TIMECODE;
  {$EXTERNALSYM DVD_HMSF_TIMECODE}

  PDVD_PLAYBACK_LOCATION2 = ^DVD_PLAYBACK_LOCATION2;
  tagDVD_PLAYBACK_LOCATION2 = record
    TitleNum: ULONG;
    ChapterNum: ULONG;
    TimeCode: DVD_HMSF_TIMECODE;
    TimeCodeFlags: ULONG;
  end;
  {$EXTERNALSYM tagDVD_PLAYBACK_LOCATION2}
  DVD_PLAYBACK_LOCATION2 = tagDVD_PLAYBACK_LOCATION2;
  {$EXTERNALSYM DVD_PLAYBACK_LOCATION2}

  PDVD_PLAYBACK_LOCATION = ^DVD_PLAYBACK_LOCATION;
  tagDVD_PLAYBACK_LOCATION = record
    TitleNum: ULONG;
    ChapterNum: ULONG;
    TimeCode: ULONG;
  end;
  {$EXTERNALSYM tagDVD_PLAYBACK_LOCATION}
  DVD_PLAYBACK_LOCATION = tagDVD_PLAYBACK_LOCATION;
  {$EXTERNALSYM DVD_PLAYBACK_LOCATION}

  PVALID_UOP_SOMTHING_OR_OTHER = ^VALID_UOP_SOMTHING_OR_OTHER;
  VALID_UOP_SOMTHING_OR_OTHER = DWORD;
  {$EXTERNALSYM VALID_UOP_SOMTHING_OR_OTHER}

  PVALID_UOP_FLAG = ^__MIDL___MIDL_itf_strmif_0000_0132_0001;
  __MIDL___MIDL_itf_strmif_0000_0132_0001           = (
    UOP_FLAG_Play_Title_Or_AtTime                   = $1,
    UOP_FLAG_Play_Chapter                           = $2,
    UOP_FLAG_Play_Title                             = $4,
    UOP_FLAG_Stop                                   = $8,
    UOP_FLAG_ReturnFromSubMenu                      = $10,
    UOP_FLAG_Play_Chapter_Or_AtTime                 = $20,
    UOP_FLAG_PlayPrev_Or_Replay_Chapter             = $40,
    UOP_FLAG_PlayNext_Chapter                       = $80,
    UOP_FLAG_Play_Forwards                          = $100,
    UOP_FLAG_Play_Backwards                         = $200,
    UOP_FLAG_ShowMenu_Title                         = $400,
    UOP_FLAG_ShowMenu_Root                          = $800,
    UOP_FLAG_ShowMenu_SubPic                        = $1000,
    UOP_FLAG_ShowMenu_Audio                         = $2000,
    UOP_FLAG_ShowMenu_Angle                         = $4000,
    UOP_FLAG_ShowMenu_Chapter                       = $8000,
    UOP_FLAG_Resume                                 = $10000,
    UOP_FLAG_Select_Or_Activate_Button              = $20000,
    UOP_FLAG_Still_Off                              = $40000,
    UOP_FLAG_Pause_On                               = $80000,
    UOP_FLAG_Select_Audio_Stream                    = $100000,
    UOP_FLAG_Select_SubPic_Stream                   = $200000,
    UOP_FLAG_Select_Angle                           = $400000,
    UOP_FLAG_Select_Karaoke_Audio_Presentation_Mode = $800000,
    UOP_FLAG_Select_Video_Mode_Preference           = $1000000);
  {$EXTERNALSYM __MIDL___MIDL_itf_strmif_0000_0132_0001}
  VALID_UOP_FLAG = __MIDL___MIDL_itf_strmif_0000_0132_0001;
  {$EXTERNALSYM VALID_UOP_FLAG}


  PDVD_CMD_FLAGS = ^__MIDL___MIDL_itf_strmif_0000_0132_0002;
  __MIDL___MIDL_itf_strmif_0000_0132_0002 = (
    DVD_CMD_FLAG_None              = 0,
    DVD_CMD_FLAG_Flush             = $1,
    DVD_CMD_FLAG_SendEvents        = $2,
    DVD_CMD_FLAG_Block             = $4,
    DVD_CMD_FLAG_StartWhenRendered = $8,
    DVD_CMD_FLAG_EndAfterRendered  = $10);
  {$EXTERNALSYM __MIDL___MIDL_itf_strmif_0000_0132_0002}
  DVD_CMD_FLAGS = __MIDL___MIDL_itf_strmif_0000_0132_0002;
  {$EXTERNALSYM DVD_CMD_FLAGS}


  PDVD_OPTION_FLAG = ^__MIDL___MIDL_itf_strmif_0000_0132_0003;
  __MIDL___MIDL_itf_strmif_0000_0132_0003 = (
    DVD_ResetOnStop                     = 1,
    DVD_NotifyParentalLevelChange       = 2,
    DVD_HMSF_TimeCodeEvents             = 3,
    DVD_AudioDuringFFwdRew              = 4,
    DVD_EnableNonblockingAPIs           = 5,
    DVD_CacheSizeInMB                   = 6,
    DVD_EnablePortableBookmarks         = 7,
    DVD_EnableExtendedCopyProtectErrors = 8,
    DVD_NotifyPositionChange            = 9,
    DVD_IncreaseOutputControl           = 10,
    DVD_EnableStreaming                 = 11,
    DVD_EnableESOutput                  = 12,
    DVD_EnableTitleLength               = 13,
    DVD_DisableStillThrottle            = 14,
    DVD_EnableLoggingEvents             = 15,
    DVD_MaxReadBurstInKB                = 16,
    DVD_ReadBurstPeriodInMS             = 17,
    DVD_RestartDisc                     = 18,
    DVD_EnableCC                        = 19);
  {$EXTERNALSYM __MIDL___MIDL_itf_strmif_0000_0132_0003}
  DVD_OPTION_FLAG = __MIDL___MIDL_itf_strmif_0000_0132_0003;
  {$EXTERNALSYM DVD_OPTION_FLAG}


  PDVD_RELATIVE_BUTTON = ^__MIDL___MIDL_itf_strmif_0000_0132_0004;
  __MIDL___MIDL_itf_strmif_0000_0132_0004 = (
    DVD_Relative_Upper = 1,
    DVD_Relative_Lower = 2,
    DVD_Relative_Left  = 3,
    DVD_Relative_Right = 4);
  {$EXTERNALSYM __MIDL___MIDL_itf_strmif_0000_0132_0004}
  DVD_RELATIVE_BUTTON = __MIDL___MIDL_itf_strmif_0000_0132_0004;
  {$EXTERNALSYM DVD_RELATIVE_BUTTON}


  PDVD_PARENTAL_LEVEL = ^DVD_PARENTAL_LEVEL;
  tagDVD_PARENTAL_LEVEL  = (
    DVD_PARENTAL_LEVEL_8 = $8000,
    DVD_PARENTAL_LEVEL_7 = $4000,
    DVD_PARENTAL_LEVEL_6 = $2000,
    DVD_PARENTAL_LEVEL_5 = $1000,
    DVD_PARENTAL_LEVEL_4 = $800,
    DVD_PARENTAL_LEVEL_3 = $400,
    DVD_PARENTAL_LEVEL_2 = $200,
    DVD_PARENTAL_LEVEL_1 = $100);
  {$EXTERNALSYM tagDVD_PARENTAL_LEVEL}
  DVD_PARENTAL_LEVEL = tagDVD_PARENTAL_LEVEL;
  {$EXTERNALSYM DVD_PARENTAL_LEVEL}

  PDVD_AUDIO_LANG_EXT = ^DVD_AUDIO_LANG_EXT;
  tagDVD_AUDIO_LANG_EXT           = (
    DVD_AUD_EXT_NotSpecified      = 0,
    DVD_AUD_EXT_Captions          = 1,
    DVD_AUD_EXT_VisuallyImpaired  = 2,
    DVD_AUD_EXT_DirectorComments1 = 3,
    DVD_AUD_EXT_DirectorComments2 = 4);
  {$EXTERNALSYM tagDVD_AUDIO_LANG_EXT}
  DVD_AUDIO_LANG_EXT = tagDVD_AUDIO_LANG_EXT;
  {$EXTERNALSYM DVD_AUDIO_LANG_EXT}

  PDVD_SUBPICTURE_LANG_EXT = ^DVD_SUBPICTURE_LANG_EXT;
  tagDVD_SUBPICTURE_LANG_EXT             = (
    DVD_SP_EXT_NotSpecified              = 0,
    DVD_SP_EXT_Caption_Normal            = 1,
    DVD_SP_EXT_Caption_Big               = 2,
    DVD_SP_EXT_Caption_Children          = 3,
    DVD_SP_EXT_CC_Normal                 = 5,
    DVD_SP_EXT_CC_Big                    = 6,
    DVD_SP_EXT_CC_Children               = 7,
    DVD_SP_EXT_Forced                    = 9,
    DVD_SP_EXT_DirectorComments_Normal   = 13,
    DVD_SP_EXT_DirectorComments_Big      = 14,
    DVD_SP_EXT_DirectorComments_Children = 15);
  {$EXTERNALSYM tagDVD_SUBPICTURE_LANG_EXT}
  DVD_SUBPICTURE_LANG_EXT = tagDVD_SUBPICTURE_LANG_EXT;
  {$EXTERNALSYM DVD_SUBPICTURE_LANG_EXT}

  PDVD_AUDIO_APPMODE = ^DVD_AUDIO_APPMODE;
  tagDVD_AUDIO_APPMODE     = (
    DVD_AudioMode_None     = 0,
    DVD_AudioMode_Karaoke  = 1,
    DVD_AudioMode_Surround = 2,
    DVD_AudioMode_Other    = 3);
  {$EXTERNALSYM tagDVD_AUDIO_APPMODE}
  DVD_AUDIO_APPMODE = tagDVD_AUDIO_APPMODE;
  {$EXTERNALSYM DVD_AUDIO_APPMODE}

  PDVD_AUDIO_FORMAT = ^DVD_AUDIO_FORMAT;
  tagDVD_AUDIO_FORMAT         = (
    DVD_AudioFormat_AC3       = 0,
    DVD_AudioFormat_MPEG1     = 1,
    DVD_AudioFormat_MPEG1_DRC = 2,
    DVD_AudioFormat_MPEG2     = 3,
    DVD_AudioFormat_MPEG2_DRC = 4,
    DVD_AudioFormat_LPCM      = 5,
    DVD_AudioFormat_DTS       = 6,
    DVD_AudioFormat_SDDS      = 7,
    DVD_AudioFormat_Other     = 8);
  {$EXTERNALSYM tagDVD_AUDIO_FORMAT}
  DVD_AUDIO_FORMAT = tagDVD_AUDIO_FORMAT;
  {$EXTERNALSYM DVD_AUDIO_FORMAT}

  PDVD_KARAOKE_DOWNMIX = ^DVD_KARAOKE_DOWNMIX;
  tagDVD_KARAOKE_DOWNMIX = (
    DVD_Mix_0to0 = $1,
    DVD_Mix_1to0 = $2,
    DVD_Mix_2to0 = $4,
    DVD_Mix_3to0 = $8,
    DVD_Mix_4to0 = $10,
    DVD_Mix_Lto0 = $20,
    DVD_Mix_Rto0 = $40,
    DVD_Mix_0to1 = $100,
    DVD_Mix_1to1 = $200,
    DVD_Mix_2to1 = $400,
    DVD_Mix_3to1 = $800,
    DVD_Mix_4to1 = $1000,
    DVD_Mix_Lto1 = $2000,
    DVD_Mix_Rto1 = $4000);
  {$EXTERNALSYM tagDVD_KARAOKE_DOWNMIX}
  DVD_KARAOKE_DOWNMIX = tagDVD_KARAOKE_DOWNMIX;
  {$EXTERNALSYM DVD_KARAOKE_DOWNMIX}

  PDVD_AudioAttributes = ^DVD_AudioAttributes;
  tagDVD_AudioAttributes = record
    AppMode: DVD_AUDIO_APPMODE;
    AppModeData: Byte;
    AudioFormat: DVD_AUDIO_FORMAT;
    Language: LCID;
    LanguageExtension: DVD_AUDIO_LANG_EXT;
    fHasMultichannelInfo: BOOL;
    dwFrequency: DWORD;
    bQuantization: Byte;
    bNumberOfChannels: Byte;
    dwReserved: array[0..1] of DWORD;
  end;
  {$EXTERNALSYM tagDVD_AudioAttributes}
  DVD_AudioAttributes = tagDVD_AudioAttributes;
  {$EXTERNALSYM DVD_AudioAttributes}


  PDVD_MUA_MixingInfo = ^DVD_MUA_MixingInfo;
  tagDVD_MUA_MixingInfo = record
    fMixTo0: BOOL;
    fMixTo1: BOOL;
    fMix0InPhase: BOOL;
    fMix1InPhase: BOOL;
    dwSpeakerPosition: DWORD;
  end;
  {$EXTERNALSYM tagDVD_MUA_MixingInfo}
  DVD_MUA_MixingInfo = tagDVD_MUA_MixingInfo;
  {$EXTERNALSYM DVD_MUA_MixingInfo}


  PDVD_MUA_Coeff = ^DVD_MUA_Coeff;
  tagDVD_MUA_Coeff = record
    log2_alpha: Double;
    log2_beta: Double;
  end;
  {$EXTERNALSYM tagDVD_MUA_Coeff}
  DVD_MUA_Coeff = tagDVD_MUA_Coeff;
  {$EXTERNALSYM DVD_MUA_Coeff}


  PDVD_MultichannelAudioAttributes = ^DVD_MultichannelAudioAttributes;
  tagDVD_MultichannelAudioAttributes = record
    Info: array[0..7] of DVD_MUA_MixingInfo;
    Coeff: array[0..7] of DVD_MUA_Coeff;
  end;
  {$EXTERNALSYM tagDVD_MultichannelAudioAttributes}
  DVD_MultichannelAudioAttributes = tagDVD_MultichannelAudioAttributes;
  {$EXTERNALSYM DVD_MultichannelAudioAttributes}


  PDVD_KARAOKE_CONTENTS = ^DVD_KARAOKE_CONTENTS;
  tagDVD_KARAOKE_CONTENTS    = (
    DVD_Karaoke_GuideVocal1  = $1,
    DVD_Karaoke_GuideVocal2  = $2,
    DVD_Karaoke_GuideMelody1 = $4,
    DVD_Karaoke_GuideMelody2 = $8,
    DVD_Karaoke_GuideMelodyA = $10,
    DVD_Karaoke_GuideMelodyB = $20,
    DVD_Karaoke_SoundEffectA = $40,
    DVD_Karaoke_SoundEffectB = $80);
  {$EXTERNALSYM tagDVD_KARAOKE_CONTENTS}
  DVD_KARAOKE_CONTENTS = tagDVD_KARAOKE_CONTENTS;
  {$EXTERNALSYM DVD_KARAOKE_CONTENTS}

  PDVD_KARAOKE_ASSIGNMENT = ^DVD_KARAOKE_ASSIGNMENT;
  tagDVD_KARAOKE_ASSIGNMENT  = (
    DVD_Assignment_reserved0 = 0,
    DVD_Assignment_reserved1 = 1,
    DVD_Assignment_LR        = 2,
    DVD_Assignment_LRM       = 3,
    DVD_Assignment_LR1       = 4,
    DVD_Assignment_LRM1      = 5,
    DVD_Assignment_LR12      = 6,
    DVD_Assignment_LRM12     = 7);
  {$EXTERNALSYM tagDVD_KARAOKE_ASSIGNMENT}
  DVD_KARAOKE_ASSIGNMENT = tagDVD_KARAOKE_ASSIGNMENT;
  {$EXTERNALSYM DVD_KARAOKE_ASSIGNMENT}

  PDVD_KaraokeAttributes = ^DVD_KaraokeAttributes;
  tagDVD_KaraokeAttributes = record
    bVersion: Byte;
    fMasterOfCeremoniesInGuideVocal1: BOOL;
    fDuet: BOOL;
    ChannelAssignment: DVD_KARAOKE_ASSIGNMENT;
    wChannelContents: array[0..7] of WORD;
  end;
  {$EXTERNALSYM tagDVD_KaraokeAttributes}
  DVD_KaraokeAttributes = tagDVD_KaraokeAttributes;
  {$EXTERNALSYM DVD_KaraokeAttributes}


  PDVD_VIDEO_COMPRESSION = ^DVD_VIDEO_COMPRESSION;
  tagDVD_VIDEO_COMPRESSION     = (
    DVD_VideoCompression_Other = 0,
    DVD_VideoCompression_MPEG1 = 1,
    DVD_VideoCompression_MPEG2 = 2);
  {$EXTERNALSYM tagDVD_VIDEO_COMPRESSION}
  DVD_VIDEO_COMPRESSION = tagDVD_VIDEO_COMPRESSION;
  {$EXTERNALSYM DVD_VIDEO_COMPRESSION}

  PDVD_VideoAttributes = ^DVD_VideoAttributes;
  tagDVD_VideoAttributes = record
    fPanscanPermitted: BOOL;
    fLetterboxPermitted: BOOL;
    ulAspectX: ULONG;
    ulAspectY: ULONG;
    ulFrameRate: ULONG;
    ulFrameHeight: ULONG;
    Compression: DVD_VIDEO_COMPRESSION;
    fLine21Field1InGOP: BOOL;
    fLine21Field2InGOP: BOOL;
    ulSourceResolutionX: ULONG;
    ulSourceResolutionY: ULONG;
    fIsSourceLetterboxed: BOOL;
    fIsFilmMode: BOOL;
  end;
  {$EXTERNALSYM tagDVD_VideoAttributes}
  DVD_VideoAttributes = tagDVD_VideoAttributes;
  {$EXTERNALSYM DVD_VideoAttributes}

  PDVD_SUBPICTURE_TYPE = ^DVD_SUBPICTURE_TYPE;
  tagDVD_SUBPICTURE_TYPE    = (
    DVD_SPType_NotSpecified = 0,
    DVD_SPType_Language     = 1,
    DVD_SPType_Other        = 2);
  {$EXTERNALSYM tagDVD_SUBPICTURE_TYPE}
  DVD_SUBPICTURE_TYPE = tagDVD_SUBPICTURE_TYPE;
  {$EXTERNALSYM DVD_SUBPICTURE_TYPE}

  PDVD_SUBPICTURE_CODING = ^DVD_SUBPICTURE_CODING;
  tagDVD_SUBPICTURE_CODING = (
    DVD_SPCoding_RunLength = 0,
    DVD_SPCoding_Extended  = 1,
    DVD_SPCoding_Other     = 2);
  {$EXTERNALSYM tagDVD_SUBPICTURE_CODING}
  DVD_SUBPICTURE_CODING = tagDVD_SUBPICTURE_CODING;
  {$EXTERNALSYM DVD_SUBPICTURE_CODING}

  PDVD_SubpictureAttributes = ^DVD_SubpictureAttributes;
  tagDVD_SubpictureAttributes = record
    _Type: DVD_SUBPICTURE_TYPE;
    CodingMode: DVD_SUBPICTURE_CODING;
    Language: LCID;
    LanguageExtension: DVD_SUBPICTURE_LANG_EXT;
  end;
  {$EXTERNALSYM tagDVD_SubpictureAttributes}
  DVD_SubpictureAttributes = tagDVD_SubpictureAttributes;
  {$EXTERNALSYM DVD_SubpictureAttributes}

  PDVD_TITLE_APPMODE = ^DVD_TITLE_APPMODE;
  tagDVD_TITLE_APPMODE        = (
    DVD_AppMode_Not_Specified = 0,
    DVD_AppMode_Karaoke       = 1,
    DVD_AppMode_Other         = 3);
  {$EXTERNALSYM tagDVD_TITLE_APPMODE}
  DVD_TITLE_APPMODE = tagDVD_TITLE_APPMODE;
  {$EXTERNALSYM DVD_TITLE_APPMODE}

  PDVD_TitleAttributes = ^DVD_TitleAttributes;
  tagDVD_TitleMainAttributes = record
    case integer of
      0: (AppMode: DVD_TITLE_APPMODE );
      1: (TitleLength: DVD_HMSF_TIMECODE );
      2: (VideoAttributes: DVD_VideoAttributes;
          ulNumberOfAudioStreams: ULONG;
          AudioAttributes: array [0..7] of DVD_AudioAttributes;
          MultichannelAudioAttributes: array [0..7] of DVD_MultichannelAudioAttributes;
          ulNumberOfSubpictureStreams: ULONG;
          SubpictureAttributes: array [0..31] of DVD_SubpictureAttributes );
    end;
  {$EXTERNALSYM tagDVD_TitleMainAttributes}
  DVD_TitleAttributes = tagDVD_TitleMainAttributes;
  {$EXTERNALSYM DVD_TitleAttributes}

  PDVD_MenuAttributes = ^DVD_MenuAttributes;
  tagDVD_MenuAttributes = record
    fCompatibleRegion: array[0..7] of BOOL;
    VideoAttributes: DVD_VideoAttributes;
    fAudioPresent: BOOL;
    AudioAttributes: DVD_AudioAttributes;
    fSubpicturePresent: BOOL;
    SubpictureAttributes: DVD_SubpictureAttributes;
  end;
  {$EXTERNALSYM tagDVD_MenuAttributes}
  DVD_MenuAttributes = tagDVD_MenuAttributes;
  {$EXTERNALSYM DVD_MenuAttributes}


  // Interface IDvdControl
  // =====================
  // Note  This interface has been deprecated.
  // It will continue to be supported for backward compatibility with existing applications,
  // but new applications should use IDvdControl2.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDvdControl);'}
  {$EXTERNALSYM IDvdControl}
  IDvdControl = interface(IUnknown)
    ['{A70EFE61-E2A3-11d0-A9BE-00AA0061BE93}']

      function TitlePlay(ulTitle: ULONG): HResult; stdcall;

      function ChapterPlay(ulTitle: ULONG;
                           ulChapter: ULONG): HResult; stdcall;

      function TimePlay(ulTitle: ULONG;
                        bcdTime: ULONG): HResult; stdcall;

      function StopForResume(): HResult; stdcall;

      function GoUp(): HResult; stdcall;

      function TimeSearch(bcdTime: ULONG): HResult; stdcall;

      function ChapterSearch(ulChapter: ULONG): HResult; stdcall;

      function PrevPGSearch(): HResult; stdcall;

      function TopPGSearch(): HResult; stdcall;

      function NextPGSearch(): HResult; stdcall;

      function ForwardScan(dwSpeed: Double): HResult; stdcall;

      function BackwardScan(dwSpeed: Double): HResult; stdcall;

      function MenuCall(MenuID: DVD_MENU_ID): HResult; stdcall;

      function Resume(): HResult; stdcall;

      function UpperButtonSelect(): HResult; stdcall;

      function LowerButtonSelect(): HResult; stdcall;

      function LeftButtonSelect(): HResult; stdcall;

      function RightButtonSelect(): HResult; stdcall;

      function ButtonActivate(): HResult; stdcall;

      function ButtonSelectAndActivate(ulButton: ULONG): HResult; stdcall;

      function StillOff(): HResult; stdcall;

      function PauseOn(): HResult; stdcall;

      function PauseOff(): HResult; stdcall;

      function MenuLanguageSelect(const Language: LCID): HResult; stdcall;

      function AudioStreamChange(ulAudio: ULONG): HResult; stdcall;

      function SubpictureStreamChange(ulSubPicture: ULONG;
                                      bDisplay: BOOL): HResult; stdcall;

      function AngleChange(ulAngle: ULONG): HResult; stdcall;

      function ParentalLevelSelect(ulParentalLevel: ULONG): HResult; stdcall;

      function ParentalCountrySelect(wCountry: WORD): HResult; stdcall;

      function KaraokeAudioPresentationModeChange(const ulMode: ULONG): HResult; stdcall;

      function VideoModePreferrence(ulPreferredDisplayMode: ULONG): HResult; stdcall;

      function SetRoot(const pszPath: LPOLESTR): HResult; stdcall;

      function MouseActivate(point: POINT): HResult; stdcall;

      function MouseSelect(point: POINT): HResult; stdcall;

      function ChapterPlayAutoStop(ulTitle: ULONG;
                                   ulChapter: ULONG;
                                   ulChaptersToPlay: ULONG): HResult; stdcall;

  end;
  IID_IDvdControl = IDvdControl;
  {$EXTERNALSYM IID_IDvdControl}


  // Interface IDvdInfo
  // ==================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDvdInfo);'}
  {$EXTERNALSYM IDvdInfo}
  IDvdInfo = interface(IUnknown)
    ['{A70EFE60-E2A3-11d0-A9BE-00AA0061BE93}']

      function GetCurrentDomain(out pDomain: DVD_DOMAIN): HResult; stdcall;

      function GetCurrentLocation(out pLocation: DVD_PLAYBACK_LOCATION): HResult; stdcall;

      function GetTotalTitleTime(out pulTotalTime: ULONG): HResult; stdcall;

      function GetCurrentButton(out pulButtonsAvailable: ULONG;
                                out pulCurrentButton: ULONG): HResult; stdcall;

      function GetCurrentAngle(out pulAnglesAvailable: ULONG;
                               out pulCurrentAngle: ULONG): HResult; stdcall;

      function GetCurrentAudio(out pulStreamsAvailable: ULONG;
                               out pulCurrentStream: ULONG): HResult; stdcall;

      function GetCurrentSubpicture(out pulStreamsAvailable: ULONG;
                                    out pulCurrentStream: ULONG;
                                    out pIsDisabled: BOOL): HResult; stdcall;

      function GetCurrentUOPS(out pUOP: VALID_UOP_SOMTHING_OR_OTHER): HResult; stdcall;

      function GetAllSPRMs(out pRegisterArray: SPRMARRAY): HResult; stdcall;

      function GetAllGPRMs(out pRegisterArray: GPRMARRAY): HResult; stdcall;

      function GetAudioLanguage(ulStream: ULONG;
                                out pLanguage: LCID): HResult; stdcall;

      function GetSubpictureLanguage(ulStream: ULONG;
                                     out pLanguage: LCID): HResult; stdcall;

      function GetTitleAttributes(ulTitle: ULONG;
                                  out pATR: DVD_ATR): HResult; stdcall;

      function GetVMGAttributes(out pATR: DVD_ATR): HResult; stdcall;

      function GetCurrentVideoAttributes(out pATR: DVD_VideoATR): HResult; stdcall;

      function GetCurrentAudioAttributes(out pATR: DVD_AudioATR): HResult; stdcall;

      function GetCurrentSubpictureAttributes(out pATR: DVD_SubpictureATR): HResult; stdcall;

      function GetCurrentVolumeInfo(out pulNumOfVol: ULONG;
                                    out pulThisVolNum: ULONG;
                                    out pSide: DVD_DISC_SIDE;
                                    out pulNumOfTitles: ULONG): HResult; stdcall;

      function GetDVDTextInfo({out} pTextManager: PByte;
                              ulBufSize: ULONG;
                              out pulActualSize: ULONG): HResult; stdcall;

      function GetPlayerParentalLevel(out pulParentalLevel: ULONG;
                                      out pulCountryCode: ULONG): HResult; stdcall;

      function GetNumberOfChapters(ulTitle: ULONG;
                                   out pulNumberOfChapters: ULONG): HResult; stdcall;

      function GetTitleParentalLevels(ulTitle: ULONG;
                                      out pulParentalLevels: ULONG): HResult; stdcall;

      function GetRoot({out} pRoot: PAnsiChar;
                       const ulBufSize: ULONG;
                       out pulActualSize: ULONG): HResult; stdcall;

  end;
  IID_IDvdInfo = IDvdInfo;
  {$EXTERNALSYM IID_IDvdInfo}


  // Interface IDvdCmd
  // =================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDvdCmd);'}
  {$EXTERNALSYM IDvdCmd}
  IDvdCmd = interface(IUnknown)
    ['{5a4a97e4-94ee-4a55-9751-74b5643aa27d}']

      function WaitForStart(): HResult; stdcall;

      function WaitForEnd(): HResult; stdcall;

  end;
  IID_IDvdCmd = IDvdCmd;
  {$EXTERNALSYM IID_IDvdCmd}


  // Interface IDvdState
  // ===================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDvdState);'}
  {$EXTERNALSYM IDvdState}
  IDvdState = interface(IUnknown)
    ['{86303d6d-1c4a-4087-ab42-f711167048ef}']

      function GetDiscID(out pullUniqueID: ULONGLONG): HResult; stdcall;

      function GetParentalLevel(out pulParentalLevel: ULONG): HResult; stdcall;

  end;


  // Interface IDvdControl2
  // ======================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDvdControl2);'}
  {$EXTERNALSYM IDvdControl2}
  IDvdControl2 = interface(IUnknown)
    ['{33BC7430-EEC0-11D2-8201-00A0C9D74842}']

      function PlayTitle(ulTitle: ULONG;
                         dwFlags: DWORD;
                         out ppCmd: IDvdCmd): HResult; stdcall;

      function PlayChapterInTitle(ulTitle: ULONG;
                                  ulChapter: ULONG;
                                  dwFlags: DWORD;
                                  out ppCmd: IDvdCmd): HResult; stdcall;

      function PlayAtTimeInTitle(ulTitle: ULONG;
                                 pStartTime: DVD_HMSF_TIMECODE;
                                 dwFlags: DWORD;
                                 out ppCmd: IDvdCmd): HResult; stdcall;

      function Stop(): HResult; stdcall;

      function ReturnFromSubmenu(dwFlags: DWORD;
                                 out ppCmd: IDvdCmd): HResult; stdcall;

      function PlayAtTime(pTime: DVD_HMSF_TIMECODE;
                          dwFlags: DWORD;
                          out ppCmd: IDvdCmd): HResult; stdcall;

      function PlayChapter(ulChapter: ULONG;
                           dwFlags: DWORD;
                           out ppCmd: IDvdCmd): HResult; stdcall;

      function PlayPrevChapter(dwFlags: DWORD;
                               out ppCmd: IDvdCmd): HResult; stdcall;

      function ReplayChapter(dwFlags: DWORD;
                             out ppCmd: IDvdCmd): HResult; stdcall;

      function PlayNextChapter(dwFlags: DWORD;
                               out ppCmd: IDvdCmd): HResult; stdcall;

      function PlayForwards(dSpeed: Double;
                            dwFlags: DWORD;
                            out ppCmd: IDvdCmd): HResult; stdcall;

      function PlayBackwards(dSpeed: Double;
                             dwFlags: DWORD;
                             var ppCmd: IDvdCmd): HResult; stdcall;

      function ShowMenu(MenuID: DVD_MENU_ID;
                        dwFlags: DWORD;
                        out ppCmd: IDvdCmd): HResult; stdcall;

      function Resume(const dwFlags: DWORD;
                      out ppCmd: IDvdCmd): HResult; stdcall;

      function SelectRelativeButton(buttonDir: DVD_RELATIVE_BUTTON): HResult; stdcall;

      function ActivateButton(): HResult; stdcall;

      function SelectButton(ulButton: ULONG): HResult; stdcall;

      function SelectAndActivateButton(const ulButton: ULONG): HResult; stdcall;

      function StillOff(): HResult; stdcall;

      function Pause(bState: BOOL): HResult; stdcall;

      function SelectAudioStream(ulAudio: ULONG;
                                 dwFlags: DWORD;
                                 out ppCmd: IDvdCmd): HResult; stdcall;

      function SelectSubpictureStream(ulSubPicture: ULONG;
                                      dwFlags: DWORD;
                                      out ppCmd: IDvdCmd): HResult; stdcall;

      function SetSubpictureState(bState: BOOL;
                                  dwFlags: DWORD;
                                  var ppCmd: IDvdCmd): HResult; stdcall;

      function SelectAngle(ulAngle: ULONG;
                           dwFlags: DWORD;
                           out ppCmd: IDvdCmd): HResult; stdcall;

      function SelectParentalLevel(ulParentalLevel: ULONG): HResult; stdcall;

      function SelectParentalCountry(bCountry: PByte): HResult; stdcall;

      function SelectKaraokeAudioPresentationMode(ulMode: ULONG): HResult; stdcall;

      function SelectVideoModePreference(ulPreferredDisplayMode: ULONG): HResult; stdcall;

      function SetDVDDirectory(const pszwPath: LPCWSTR): HResult; stdcall;

      function ActivateAtPosition(point: POINT): HResult; stdcall;

      function SelectAtPosition(point: POINT): HResult; stdcall;

      function PlayChaptersAutoStop(ulTitle: ULONG;
                                    ulChapter: ULONG;
                                    ulChaptersToPlay: ULONG;
                                    dwFlags: DWORD;
                                    out ppCmd: IDvdCmd): HResult; stdcall;

      function AcceptParentalLevelChange(bAccept: BOOL): HResult; stdcall;

      function SetOption(flag: DVD_OPTION_FLAG;
                         fState: BOOL): HResult; stdcall;

      function SetState(pState: IDvdState;
                        dwFlags: DWORD;
                        out ppCmd: IDvdCmd): HResult; stdcall;

      function PlayPeriodInTitleAutoStop(ulTitle: ULONG;
                                         pStartTime: DVD_HMSF_TIMECODE;
                                         pEndTime: DVD_HMSF_TIMECODE;
                                         dwFlags: DWORD;
                                         out ppCmd: IDvdCmd): HResult; stdcall;

      function SetGPRM(ulIndex: ULONG;
                       wValue: WORD;
                       dwFlags: DWORD;
                       out ppCmd: IDvdCmd): HResult; stdcall;

      function SelectDefaultMenuLanguage(const Language: LCID): HResult; stdcall;

      function SelectDefaultAudioLanguage(const Language: LCID;
                                          audioExtension: DVD_AUDIO_LANG_EXT): HResult; stdcall;

      function SelectDefaultSubpictureLanguage(const Language: LCID;
                                               subpictureExtension: DVD_SUBPICTURE_LANG_EXT): HResult; stdcall;

  end;
  IID_IDvdControl2 = IDvdControl2;
  {$EXTERNALSYM IID_IDvdControl2}


  PDVD_TextStringType = ^DVD_TextStringType;
  DVD_TextStringType = (
        DVD_Struct_Volume        = $1,
        DVD_Struct_Title         = $2,
        DVD_Struct_ParentalID    = $3,
        DVD_Struct_PartOfTitle   = $4,
        DVD_Struct_Cell          = $5,
        DVD_Stream_Audio         = $10,
        DVD_Stream_Subpicture    = $11,
        DVD_Stream_Angle         = $12,
        DVD_Channel_Audio        = $20,
        DVD_General_Name         = $30,
        DVD_General_Comments     = 431,
        DVD_Title_Series         = $38,
        DVD_Title_Movie          = $39,
        DVD_Title_Video          = $3a,
        DVD_Title_Album          = $3b,
        DVD_Title_Song           = $3c,
        DVD_Title_Other          = $3f,
        DVD_Title_Sub_Series     = $40,
        DVD_Title_Sub_Movie      = 441,
        DVD_Title_Sub_Video      = $42,
        DVD_Title_Sub_Album      = $43,
        DVD_Title_Sub_Song       = $44,
        DVD_Title_Sub_Other      = $47,
        DVD_Title_Orig_Series    = $48,
        DVD_Title_Orig_Movie     = $49,
        DVD_Title_Orig_Video     = $4a,
        DVD_Title_Orig_Album     = $4b,
        DVD_Title_Orig_Song      = $4c,
        DVD_Title_Orig_Other     = $4f,
        DVD_Other_Scene          = $50,
        DVD_Other_Cut            = $51,
        DVD_Other_Take           = $52);
  {$EXTERNALSYM DVD_TextStringType}

  PDVD_TextCharSet = ^DVD_TextCharSet;
  DVD_TextCharSet = (
    DVD_CharSet_Unicode                        = 0,
    DVD_CharSet_ISO646                         = 1,
    DVD_CharSet_JIS_Roman_Kanji                = 2,
    DVD_CharSet_ISO8859_1                      = 3,
    DVD_CharSet_ShiftJIS_Kanji_Roman_Katakana  = 4);
  {$EXTERNALSYM DVD_TextCharSet}

  PDVD_DECODER_CAPS = ^DVD_DECODER_CAPS;
  tagDVD_DECODER_CAPS = record
    dwSize: DWORD;
    dwAudioCaps: DWORD;
    dFwdMaxRateVideo: Double;
    dFwdMaxRateAudio: Double;
    dFwdMaxRateSP: Double;
    dBwdMaxRateVideo: Double;
    dBwdMaxRateAudio: Double;
    dBwdMaxRateSP: Double;
    dwRes1: DWORD;
    dwRes2: DWORD;
    dwRes3: DWORD;
    dwRes4: DWORD;
  end;
  {$EXTERNALSYM tagDVD_DECODER_CAPS}
  DVD_DECODER_CAPS = tagDVD_DECODER_CAPS;
  {$EXTERNALSYM DVD_DECODER_CAPS}


  // Interface IDvdInfo2
  // ===================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDvdInfo2);'}
  {$EXTERNALSYM IDvdInfo2}
  IDvdInfo2 = interface(IUnknown)
    ['{34151510-EEC0-11D2-8201-00A0C9D74842}']

      function GetCurrentDomain(out pDomain: DVD_DOMAIN): HResult; stdcall;

      function GetCurrentLocation(out pLocation: DVD_PLAYBACK_LOCATION2): HResult; stdcall;

      function GetTotalTitleTime(out pTotalTime: DVD_HMSF_TIMECODE;
                                 out ulTimeCodeFlags: ULONG): HResult; stdcall;

      function GetCurrentButton(out pulButtonsAvailable: ULONG;
                                out pulCurrentButton: ULONG): HResult; stdcall;

      function GetCurrentAngle(out pulAnglesAvailable: ULONG;
                               out pulCurrentAngle: ULONG): HResult; stdcall;

      function GetCurrentAudio(out pulStreamsAvailable: ULONG;
                               out pulCurrentStream: ULONG): HResult; stdcall;

      function GetCurrentSubpicture(out pulStreamsAvailable: ULONG;
                                    out pulCurrentStream: ULONG;
                                    out pbIsDisabled: BOOL): HResult; stdcall;

      function GetCurrentUOPS(out pulUOPs: ULONG): HResult; stdcall;

      function GetAllSPRMs(out pRegisterArray: SPRMARRAY): HResult; stdcall;

      function GetAllGPRMs(out pRegisterArray: GPRMARRAY): HResult; stdcall;

      function GetAudioLanguage(ulStream: ULONG;
                                out pLanguage: LCID): HResult; stdcall;

      function GetSubpictureLanguage(ulStream: ULONG;
                                     out pLanguage: LCID): HResult; stdcall;

      function GetTitleAttributes(ulTitle: ULONG;
                                  out pMenu: DVD_MenuAttributes;
                                  out pTitle: DVD_TitleAttributes): HResult; stdcall;

      function GetVMGAttributes(out pATR: DVD_MenuAttributes): HResult; stdcall;

      function GetCurrentVideoAttributes(out pATR: DVD_VideoAttributes): HResult; stdcall;

      function GetAudioAttributes(ulStream: ULONG;
                                  out pATR: DVD_AudioAttributes): HResult; stdcall;

      function GetKaraokeAttributes(ulStream: ULONG;
                                    out pAttributes: DVD_KaraokeAttributes): HResult; stdcall;

      function GetSubpictureAttributes(ulStream: ULONG;
                                       out pATR: DVD_SubpictureAttributes): HResult; stdcall;

      function GetDVDVolumeInfo(pulNumOfVolumes: ULONG;
                                pulVolume: ULONG;
                                out pSide: DVD_DISC_SIDE;
                                out pulNumOfTitles: ULONG): HResult; stdcall;

      function GetDVDTextNumberOfLanguages(out pulNumOfLangs: ULONG): HResult; stdcall;

      function GetDVDTextLanguageInfo(ulLangIndex: ULONG;
                                      out pulNumOfStrings: ULONG;
                                      out pLangCode: LCID;
                                      out pbCharacterSet: DVD_TextCharSet): HResult; stdcall;

      function GetDVDTextStringAsNative(ulLangIndex: ULONG;
                                        ulStringIndex: ULONG;
                                        out pbBuffer: PByte;
                                        ulMaxBufferSize: ULONG;
                                        out pulActualSize: ULONG;
                                        out pType: DVD_TextStringType): HResult; stdcall;

      function GetDVDTextStringAsUnicode(ulLangIndex: ULONG;
                                         ulStringIndex: ULONG;
                                         out pchwBuffer: LPCWSTR;
                                         ulMaxBufferSize: ULONG;
                                         out pulActualSize: ULONG;
                                         out pType: DVD_TextStringType): HResult; stdcall;

      function GetPlayerParentalLevel(out pulParentalLevel: ULONG;
                                      out pbCountryCode: PByte): HResult; stdcall;

      function GetNumberOfChapters(ulTitle: ULONG;
                                   out pulNumOfChapters: ULONG): HResult; stdcall;

      function GetTitleParentalLevels(ulTitle: ULONG;
                                      out pulParentalLevels: ULONG): HResult; stdcall;

      function GetDVDDirectory(out pszwPath: LPCWSTR;
                               ulMaxSize: ULONG;
                               out pulActualSize: ULONG): HResult; stdcall;

      function IsAudioStreamEnabled(ulStreamNum: ULONG;
                                    out pbEnabled: BOOL): HResult; stdcall;

      function GetDiscID(const pszwPath: LPCWSTR;
                         out pullDiscID: ULONGLONG): HResult; stdcall;

      function GetState(out pStateData: IDvdState): HResult; stdcall;

      function GetMenuLanguages(out pLanguages: LCID;
                                ulMaxLanguages: ULONG;
                                out pulActualLanguages: ULONG): HResult; stdcall;

      function GetButtonAtPosition(point: POINT;
                                   out pulButtonIndex: ULONG): HResult; stdcall;

      function GetCmdFromEvent(lParam1: LONG_PTR;
                               out pCmdObj: IDvdCmd): HResult; stdcall;

      function GetDefaultMenuLanguage(out pLanguage: LCID): HResult; stdcall;

      function GetDefaultAudioLanguage(out pLanguage: LCID;
                                       out pAudioExtension: DVD_AUDIO_LANG_EXT): HResult; stdcall;

      function GetDefaultSubpictureLanguage(out pLanguage: LCID;
                                            out pSubpictureExtension: DVD_SUBPICTURE_LANG_EXT): HResult; stdcall;

      function GetDecoderCaps(out pCaps: DVD_DECODER_CAPS): HResult; stdcall;

      function GetButtonRect(ulButton: ULONG;
                             out pRect: NORMALIZEDRECT): HResult; stdcall;

      function IsSubpictureStreamEnabled(const ulStreamNum: ULONG;
                                         out pbEnabled: BOOL): HResult; stdcall;

  end;
  IID_IDvdInfo2 = IDvdInfo2;
  {$EXTERNALSYM IID_IDvdInfo2}


  PAM_DVD_GRAPH_FLAGS = ^AM_DVD_GRAPH_FLAGS;
  _AM_DVD_GRAPH_FLAGS   = (
    AM_DVD_HWDEC_PREFER = $1,
    AM_DVD_HWDEC_ONLY   = $2,
    AM_DVD_SWDEC_PREFER = $4,
    AM_DVD_SWDEC_ONLY   = $8,
    AM_DVD_NOVPE        = $100,
    AM_DVD_DO_NOT_CLEAR = $200,
    AM_DVD_VMR9_ONLY    = $800,
    AM_DVD_EVR_ONLY     = $1000,
    AM_DVD_EVR_QOS      = $2000,
    AM_DVD_ADAPT_GRAPH  = $4000,
    AM_DVD_MASK         = $FFFF);
  {$EXTERNALSYM _AM_DVD_GRAPH_FLAGS}
  AM_DVD_GRAPH_FLAGS = _AM_DVD_GRAPH_FLAGS;
  {$EXTERNALSYM AM_DVD_GRAPH_FLAGS}

  PAM_DVD_STREAM_FLAGS = ^AM_DVD_STREAM_FLAGS;
  _AM_DVD_STREAM_FLAGS   = (
    AM_DVD_STREAM_VIDEO  = $1,
    AM_DVD_STREAM_AUDIO  = $2,
    AM_DVD_STREAM_SUBPIC = $4);
  {$EXTERNALSYM _AM_DVD_STREAM_FLAGS}
  AM_DVD_STREAM_FLAGS = _AM_DVD_STREAM_FLAGS;
  {$EXTERNALSYM AM_DVD_STREAM_FLAGS}

  PAM_DVD_RENDERSTATUS = ^__MIDL___MIDL_itf_strmif_0000_0138_0001;
  __MIDL___MIDL_itf_strmif_0000_0138_0001 = record
    hrVPEStatus: HResult;
    bDvdVolInvalid: BOOL;
    bDvdVolUnknown: BOOL;
    bNoLine21In: BOOL;
    bNoLine21Out: BOOL;
    iNumStreams: Integer;
    iNumStreamsFailed: Integer;
    dwFailedStreamsFlag: DWORD;
  end;
  {$EXTERNALSYM __MIDL___MIDL_itf_strmif_0000_0138_0001}
  AM_DVD_RENDERSTATUS = __MIDL___MIDL_itf_strmif_0000_0138_0001;
  {$EXTERNALSYM AM_DVD_RENDERSTATUS}


  // Interface IDvdGraphBuilder
  // ==========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDvdGraphBuilder);'}
  {$EXTERNALSYM IDvdGraphBuilder}
  IDvdGraphBuilder = interface(IUnknown)
    ['{FCC152B6-F372-11d0-8E00-00C04FD7C08B}']

      function GetFiltergraph(out ppGB: IGraphBuilder): HResult; stdcall;

      function GetDvdInterface(const riid: REFIID;
                               out ppvIF): HResult; stdcall;

      function RenderDvdVideoVolume(const lpcwszPathName: LPCWSTR;
                                    dwFlags: DWORD;
                                    out pStatus: AM_DVD_RENDERSTATUS): HResult; stdcall;

  end;
  IID_IDvdGraphBuilder = IDvdGraphBuilder;
  {$EXTERNALSYM IID_IDvdGraphBuilder}


  // Those pointers are not defined in DirectDraw.pas
  PIDirectDraw = ^IDirectDraw;
  PIDirectDrawSurface = ^IDirectDrawSurface;


  // Interface IDDrawExclModeVideo
  // =============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDDrawExclModeVideo);'}
  {$EXTERNALSYM IDDrawExclModeVideo}
  IDDrawExclModeVideo = interface(IUnknown)
    ['{153ACC21-D83B-11d1-82BF-00A0C9696C8F}']

      function SetDDrawObject(pDDrawObject: IDirectDraw): HResult; stdcall;

      function GetDDrawObject(out ppDDrawObject: IDirectDraw;
                              out pbUsingExternal: BOOL): HResult; stdcall;

      function SetDDrawSurface(pDDrawSurface: IDirectDrawSurface): HResult; stdcall;

      function GetDDrawSurface(out ppDDrawSurface: IDirectDrawSurface;
                               out pbUsingExternal: BOOL): HResult; stdcall;

      function SetDrawParameters(prcSource: TRect;
                                 prcTarget: TRect): HResult; stdcall;

      function GetNativeVideoProps(out pdwVideoWidth: DWORD;
                                   out pdwVideoHeight: DWORD;
                                   out pdwPictAspectRatioX: DWORD;
                                   out pdwPictAspectRatioY: DWORD): HResult; stdcall;

      function SetCallbackInterface(pCallback: IDDrawExclModeVideoCallback;
                                    const dwFlags: DWORD): HResult; stdcall;

  end;
  IID_IDDrawExclModeVideo = IDDrawExclModeVideo;
  {$EXTERNALSYM IID_IDDrawExclModeVideo}


  PAM_OVERLAY_NOTIFY_FLAGS = ^_AM_OVERLAY_NOTIFY_FLAGS;
  _AM_OVERLAY_NOTIFY_FLAGS = (
    AM_OVERLAY_NOTIFY_VISIBLE_CHANGE  = $1,
    AM_OVERLAY_NOTIFY_SOURCE_CHANGE    = $2,
    AM_OVERLAY_NOTIFY_DEST_CHANGE      = $4);
  {$EXTERNALSYM _AM_OVERLAY_NOTIFY_FLAGS}
  AM_OVERLAY_NOTIFY_FLAGS = _AM_OVERLAY_NOTIFY_FLAGS;
  {$EXTERNALSYM AM_OVERLAY_NOTIFY_FLAGS}


  // Interface IDDrawExclModeVideoCallback
  // =====================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDDrawExclModeVideoCallback);'}
  {$EXTERNALSYM IDDrawExclModeVideoCallback}
  IDDrawExclModeVideoCallback = interface(IUnknown)
    ['{913c24a0-20ab-11d2-9038-00a0c9697298}']

      function OnUpdateOverlay(bBefore: BOOL;
                               dwFlags: DWORD;
                               bOldVisible: BOOL;
                               prcOldSrc: TRect;
                               prcOldDest: TRect;
                               bNewVisible: BOOL;
                               prcNewSrc: TRect;
                               prcNewDest: TRect): HResult; stdcall;

      function OnUpdateColorKey(pKey: COLORKEY;
                                dwColor: DWORD): HResult; stdcall;

      function OnUpdateSize(dwWidth: DWORD;
                            dwHeight: DWORD;
                            dwARWidth: DWORD;
                            dwARHeight: DWORD): HResult; stdcall;

  end;
  IID_IDDrawExclModeVideoCallback = IDDrawExclModeVideoCallback;
  {$EXTERNALSYM IID_IDDrawExclModeVideoCallback}


  // Additional Prototypes for ALL interfaces

  // End of Additional Prototypes

implementation

  // Implement Additional Prototypes here.

end.
