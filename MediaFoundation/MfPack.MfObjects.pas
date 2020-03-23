// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - MediaFoundation
// Project location: https://sourceforge.net/projects/MFPack
// Module: MfPack.MfObjects.pas
// Kind: Pascal / Delphi unit
// Release date: 29-06-2012
// Language: ENU
//
// Revision Version: 2.6.4
// Description: -
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX), Peter Larson (ozships), (Ciaran), (TopPlay)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 28/05/2020                     Kraftwerk release. (WIN10 April 2020 update, version 20H1)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows Vista or higher.
// 
//         Using packed records is not a recommended practice,
//         because it can prevent compatibility with other languages or
//         platforms, it slows data access, and, in the case of a character array,
//         it affects type compatibility.
//         For more information, see Memory management and Implicit Packing of
//         Fields with a Common Type Specification.
//
// Related objects: -
// Related projects: MfPackX264
// Known Issues: -
//
// Compiler version: 23 up to 33
// SDK version: 10.0.19569.0
//
// Todo: -
//
//==============================================================================
// Source: mfobjects.h
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
unit MfPack.MfObjects;

  {$HPPEMIT ''}
  {$HPPEMIT '#include "mfobjects.h"'}
  {$HPPEMIT ''}

interface

uses
  {WinApi}
  Winapi.Windows,
  WinApi.ActiveX,
  {MfPack}
  MfPack.MfpTypes,
  MfPack.PropSys,
  MfPack.PropIdl,
  MfPack.ObjIdlbase,
  MfPack.Unknwn,
  MfPack.MMReg,
  MfPack.MediaObj;

  {$WEAKPACKAGEUNIT ON}
  {$MINENUMSIZE 4}

  {$IFDEF WIN32}
    {$ALIGN 1}
  {$ELSE}
    {$ALIGN 8} // Win64
  {$ENDIF}

  {$WARN SYMBOL_PLATFORM OFF}

  {$I 'MfPack.inc'}
  {$WARN BOUNDS_ERROR OFF}

const

  // Interface IMFMediaType
  MF_MEDIATYPE_EQUAL_MAJOR_TYPES      = $00000001;
  {$EXTERNALSYM MF_MEDIATYPE_EQUAL_MAJOR_TYPES}
  MF_MEDIATYPE_EQUAL_FORMAT_TYPES     = $00000002;
  {$EXTERNALSYM MF_MEDIATYPE_EQUAL_FORMAT_TYPES}
  MF_MEDIATYPE_EQUAL_FORMAT_DATA      = $00000004;
  {$EXTERNALSYM MF_MEDIATYPE_EQUAL_FORMAT_DATA}
  MF_MEDIATYPE_EQUAL_FORMAT_USER_DATA = $00000008;
  {$EXTERNALSYM MF_MEDIATYPE_EQUAL_FORMAT_USER_DATA}

  // Interface IMFAsyncCallback
  MFASYNC_FAST_IO_PROCESSING_CALLBACK = $00000001;
  {$EXTERNALSYM MFASYNC_FAST_IO_PROCESSING_CALLBACK}
  MFASYNC_SIGNAL_CALLBACK             = $00000002;
  {$EXTERNALSYM MFASYNC_SIGNAL_CALLBACK}
  MFASYNC_CALLBACK_QUEUE_UNDEFINED    = $00000000;
  {$EXTERNALSYM MFASYNC_CALLBACK_QUEUE_UNDEFINED}
  MFASYNC_CALLBACK_QUEUE_STANDARD     = $00000001;
  {$EXTERNALSYM MFASYNC_CALLBACK_QUEUE_STANDARD}
  MFASYNC_CALLBACK_QUEUE_RT           = $00000002;
  {$EXTERNALSYM MFASYNC_CALLBACK_QUEUE_RT}
  MFASYNC_CALLBACK_QUEUE_IO           = $00000003;
  {$EXTERNALSYM MFASYNC_CALLBACK_QUEUE_IO}
  MFASYNC_CALLBACK_QUEUE_TIMER        = $00000004;
  {$EXTERNALSYM MFASYNC_CALLBACK_QUEUE_TIMER}
  MFASYNC_CALLBACK_QUEUE_LONG_FUNCTION= $00000007;
  {$EXTERNALSYM MFASYNC_CALLBACK_QUEUE_LONG_FUNCTION}
  MFASYNC_CALLBACK_QUEUE_PRIVATE_MASK = $FFFF0000;
  {$EXTERNALSYM MFASYNC_CALLBACK_QUEUE_PRIVATE_MASK}
  MFASYNC_CALLBACK_QUEUE_ALL          = MAXDW; // 0xFFFFFFFF
  {$EXTERNALSYM MFASYNC_CALLBACK_QUEUE_ALL}

type
  // MediaEventType = DWORD;
  PMediaEventType = ^MediaEventType;
  MediaEventType = (

  //-------------------------------------------------------------------------
  // generic events
  //-------------------------------------------------------------------------

        MEUnknown	                              = 0,
        MEError	                                = 1,
        MEExtendedType	                        = 2,
        MENonFatalError	                        = 3,
        MEGenericV1Anchor	                      = MENonFatalError,

  //-------------------------------------------------------------------------
  // Media Session events:
  // Events of interest to applications using the Media Session
  //-------------------------------------------------------------------------

        MESessionUnknown	                      = 100,
        MESessionTopologySet	                  = 101,
        MESessionTopologiesCleared	            = 102,
        MESessionStarted	                      = 103,
        MESessionPaused	                        = 104,
        MESessionStopped	                      = 105,
        MESessionClosed	                        = 106,
        MESessionEnded	                        = 107,
        MESessionRateChanged	                  = 108,
        MESessionScrubSampleComplete	          = 109,
        MESessionCapabilitiesChanged	          = 110,
        MESessionTopologyStatus	                = 111,
        MESessionNotifyPresentationTime	        = 112,
        MENewPresentation	                      = 113,
        MELicenseAcquisitionStart	              = 114,
        MELicenseAcquisitionCompleted	          = 115,
        MEIndividualizationStart	              = 116,
        MEIndividualizationCompleted	          = 117,
        MEEnablerProgress	                      = 118,
        MEEnablerCompleted	                    = 119,
        MEPolicyError	                          = 120,
        MEPolicyReport	                        = 121,
        MEBufferingStarted	                    = 122,
        MEBufferingStopped	                    = 123,
        MEConnectStart	                        = 124,
        MEConnectEnd	                          = 125,
        MEReconnectStart	                      = 126,
        MEReconnectEnd	                        = 127,
        MERendererEvent	                        = 128,
        MESessionStreamSinkFormatChanged	      = 129,
        MESessionV1Anchor	                      = MESessionStreamSinkFormatChanged,
        MESourceUnknown	                        = 200,
        MESourceStarted	                        = 201,
        MEStreamStarted	                        = 202,
        MESourceSeeked	                        = 203,
        MEStreamSeeked	                        = 204,
        MENewStream	                            = 205,
        MEUpdatedStream	                        = 206,
        MESourceStopped	                        = 207,
        MEStreamStopped	                        = 208,
        MESourcePaused	                        = 209,
        MEStreamPaused	                        = 210,
        MEEndOfPresentation	                    = 211,
        MEEndOfStream	                          = 212,
        MEMediaSample	                          = 213,  // Sent when a media stream delivers a new sample in response to a call to IMFMediaStream.RequestSample.
        MEStreamTick	                          = 214,  // Signals that a media stream does not have data available at a specified time.
        MEStreamThinMode	                      = 215,
        MEStreamFormatChanged	                  = 216,
        MESourceRateChanged	                    = 217,
        MEEndOfPresentationSegment	            = 218,
        MESourceCharacteristicsChanged	        = 219,
        MESourceRateChangeRequested	            = 220,
        MESourceMetadataChanged	                = 221,
        MESequencerSourceTopologyUpdated	      = 222,
        MESourceV1Anchor	                      = MESequencerSourceTopologyUpdated,
        MESinkUnknown	                          = 300,
        MEStreamSinkStarted	                    = 301,
        MEStreamSinkStopped	                    = 302,
        MEStreamSinkPaused	                    = 303,
        MEStreamSinkRateChanged	                = 304,
        MEStreamSinkRequestSample	              = 305,
        MEStreamSinkMarker	                    = 306,
        MEStreamSinkPrerolled	                  = 307,
        MEStreamSinkScrubSampleComplete	        = 308,
        MEStreamSinkFormatChanged	              = 309,
        MEStreamSinkDeviceChanged	              = 310,
        MEQualityNotify	                        = 311,
        MESinkInvalidated	                      = 312,
        MEAudioSessionNameChanged	              = 313,
        MEAudioSessionVolumeChanged	            = 314,
        MEAudioSessionDeviceRemoved	            = 315,
        MEAudioSessionServerShutdown	          = 316,
        MEAudioSessionGroupingParamChanged	    = 317,
        MEAudioSessionIconChanged	              = 318,
        MEAudioSessionFormatChanged	            = 319,
        MEAudioSessionDisconnected	            = 320,
        MEAudioSessionExclusiveModeOverride	    = 321,
        MESinkV1Anchor	                        = MEAudioSessionExclusiveModeOverride,
        MECaptureAudioSessionVolumeChanged	    = 322,
        MECaptureAudioSessionDeviceRemoved	    = 323,
        MECaptureAudioSessionFormatChanged	    = 324,
        MECaptureAudioSessionDisconnected	      = 325,
        MECaptureAudioSessionExclusiveModeOverride	= 326,
        MECaptureAudioSessionServerShutdown	    = 327,
        MESinkV2Anchor	                        = MECaptureAudioSessionServerShutdown,
        METrustUnknown	                        = 400,
        MEPolicyChanged	                        = 401,
        MEContentProtectionMessage	            = 402,
        MEPolicySet	                            = 403,
        METrustV1Anchor	                        = MEPolicySet,
        MEWMDRMLicenseBackupCompleted	          = 500,
        MEWMDRMLicenseBackupProgress	          = 501,
        MEWMDRMLicenseRestoreCompleted	        = 502,
        MEWMDRMLicenseRestoreProgress	          = 503,
        MEWMDRMLicenseAcquisitionCompleted	    = 506,
        MEWMDRMIndividualizationCompleted	      = 508,
        MEWMDRMIndividualizationProgress	      = 513,
        MEWMDRMProximityCompleted	              = 514,
        MEWMDRMLicenseStoreCleaned	            = 515,
        MEWMDRMRevocationDownloadCompleted	    = 516,
        MEWMDRMV1Anchor	                        = MEWMDRMRevocationDownloadCompleted,
        METransformUnknown	                    = 600,
        METransformNeedInput	                  = (METransformUnknown + 1) ,
        METransformHaveOutput	                  = (METransformNeedInput + 1) ,
        METransformDrainComplete	              = (METransformHaveOutput + 1) ,
        METransformMarker	                      = (METransformDrainComplete + 1) ,
        METransformInputStreamStateChanged      = (METransformMarker + 1) ,
        MEByteStreamCharacteristicsChanged      = 700,
        MEVideoCaptureDeviceRemoved	            = 800,
        MEVideoCaptureDevicePreempted	          = 801,
        MEStreamSinkFormatInvalidated	          = 802,
        MEEncodingParameters                    = 803,
        MEContentProtectionMetadata	            = 900,
        MEDeviceThermalStateChanged	            = 950,
        MEReservedMax	                          = 10000);
  {$EXTERNALSYM MediaEventType}


const
  // Interface IMFMediaEventGenerator
  MF_EVENT_FLAG_NO_WAIT = $00000001;
  {$EXTERNALSYM MF_EVENT_FLAG_NO_WAIT}

  // Interface IMFByteStream
  MFBYTESTREAM_IS_READABLE                 = $00000001;
  {$EXTERNALSYM MFBYTESTREAM_IS_READABLE}
  MFBYTESTREAM_IS_WRITABLE                 = $00000002;
  {$EXTERNALSYM MFBYTESTREAM_IS_WRITABLE}
  MFBYTESTREAM_IS_SEEKABLE                 = $00000004;
  {$EXTERNALSYM MFBYTESTREAM_IS_SEEKABLE}
  MFBYTESTREAM_IS_REMOTE                   = $00000008;
  {$EXTERNALSYM MFBYTESTREAM_IS_REMOTE}
  MFBYTESTREAM_IS_DIRECTORY                = $00000080;
  {$EXTERNALSYM MFBYTESTREAM_IS_DIRECTORY}
  MFBYTESTREAM_HAS_SLOW_SEEK               = $00000100;
  {$EXTERNALSYM MFBYTESTREAM_HAS_SLOW_SEEK}
  MFBYTESTREAM_IS_PARTIALLY_DOWNLOADED     = $00000200;
  {$EXTERNALSYM MFBYTESTREAM_IS_PARTIALLY_DOWNLOADED}
  //>= Windows 7
  //#if (WINVER >= _WIN32_WINNT_WIN7)
  MFBYTESTREAM_SHARE_WRITE                 = $00000400;
  {$EXTERNALSYM MFBYTESTREAM_SHARE_WRITE}
  //#endif // (WINVER >= _WIN32_WINNT_WIN7)
  //end >= Windows 7
  MFBYTESTREAM_SEEK_FLAG_CANCEL_PENDING_IO = $00000001;
  {$EXTERNALSYM MFBYTESTREAM_SEEK_FLAG_CANCEL_PENDING_IO}

  //Interface IMFByteStream
	MF_BYTESTREAM_ORIGIN_NAME                        : TGUID = '{fc358288-3cb6-460c-a424-b6681260375a}';
	{$EXTERNALSYM MF_BYTESTREAM_ORIGIN_NAME}
	MF_BYTESTREAM_CONTENT_TYPE                       : TGUID = '{fc358289-3cb6-460c-a424-b6681260375a}';
	{$EXTERNALSYM MF_BYTESTREAM_CONTENT_TYPE}
	MF_BYTESTREAM_DURATION                        	 : TGUID = '{fc35828a-3cb6-460c-a424-b6681260375a}';
	{$EXTERNALSYM MF_BYTESTREAM_DURATION}
	MF_BYTESTREAM_LAST_MODIFIED_TIME                 : TGUID = '{fc35828b-3cb6-460c-a424-b6681260375a}';
	{$EXTERNALSYM MF_BYTESTREAM_LAST_MODIFIED_TIME}
  // >= Windows 7
	//#if (WINVER >= _WIN32_WINNT_WIN7)
	MF_BYTESTREAM_IFO_FILE_URI                       : TGUID = '{fc35828c-3cb6-460c-a424-b6681260375a}';
	{$EXTERNALSYM MF_BYTESTREAM_IFO_FILE_URI}
	MF_BYTESTREAM_DLNA_PROFILE_ID                    : TGUID = '{fc35828d-3cb6-460c-a424-b6681260375a}';
	{$EXTERNALSYM MF_BYTESTREAM_DLNA_PROFILE_ID}
	//#endif // (WINVER >= _WIN32_WINNT_WIN7)
  //end >= Windows 7

  // Interface IMFByteStream
type
	QWORD = ULONGLONG;
	{$EXTERNALSYM QWORD}

  // PRGBQUAD = ^RGBQUAD;
  // RGBQUAD = DWORD;
  // RGBQUAD is defined ins Winapi.Windows (Wingdi.h) and MfPack.MfTypes.
  // To store a RGBQUAD in a DWORD, use procedure CopyRGBQuadToClrRef in MfPack.MfpUtils

  PMF_ATTRIBUTE_TYPE = ^_MF_ATTRIBUTE_TYPE;
	_MF_ATTRIBUTE_TYPE    = (
	  MF_ATTRIBUTE_UINT32   = VT_UI4,
	  MF_ATTRIBUTE_UINT64   = VT_UI8,
	  MF_ATTRIBUTE_DOUBLE   = VT_R8,
	  MF_ATTRIBUTE_GUID     = VT_CLSID,
	  MF_ATTRIBUTE_STRING   = VT_LPWSTR,
	  MF_ATTRIBUTE_BLOB     = (VT_VECTOR or VT_UI1),
	  MF_ATTRIBUTE_IUNKNOWN = VT_UNKNOWN);
	{$EXTERNALSYM _MF_ATTRIBUTE_TYPE}
	MF_ATTRIBUTE_TYPE = _MF_ATTRIBUTE_TYPE;
	{$EXTERNALSYM MF_ATTRIBUTE_TYPE}


  PMF_ATTRIBUTES_MATCH_TYPE = ^_MF_ATTRIBUTES_MATCH_TYPE;
  _MF_ATTRIBUTES_MATCH_TYPE        = (
   	MF_ATTRIBUTES_MATCH_OUR_ITEMS    = 0,
   	MF_ATTRIBUTES_MATCH_THEIR_ITEMS  = 1,
   	MF_ATTRIBUTES_MATCH_ALL_ITEMS    = 2,
   	MF_ATTRIBUTES_MATCH_INTERSECTION = 3,
   	MF_ATTRIBUTES_MATCH_SMALLER      = 4);
  {$EXTERNALSYM _MF_ATTRIBUTES_MATCH_TYPE}
  MF_ATTRIBUTES_MATCH_TYPE = _MF_ATTRIBUTES_MATCH_TYPE;
  {$EXTERNALSYM MF_ATTRIBUTES_MATCH_TYPE}


  PMF_ATTRIBUTE_SERIALIZE_OPTIONS = ^cwMF_ATTRIBUTE_SERIALIZE_OPTIONS;
  cwMF_ATTRIBUTE_SERIALIZE_OPTIONS     = (
	  MF_ATTRIBUTE_SERIALIZE_UNKNOWN_BYREF = $1);
  {$EXTERNALSYM cwMF_ATTRIBUTE_SERIALIZE_OPTIONS}
  MF_ATTRIBUTE_SERIALIZE_OPTIONS = cwMF_ATTRIBUTE_SERIALIZE_OPTIONS;
  {$EXTERNALSYM MF_ATTRIBUTE_SERIALIZE_OPTIONS}


  PBITMAPINFOHEADER = ^BITMAPINFOHEADER;
  cwBITMAPINFOHEADER = record
	  biSize: DWORD;
	  biWidth: LONG;
   	biHeight: LONG;
	  biPlanes: WORD;
   	biBitCount: WORD;
   	biCompression: DWORD;
   	biSizeImage: DWORD;
   	biXPelsPerMeter: LONG;
    biYPelsPerMeter: LONG;
   	biClrUsed: DWORD;
  	biClrImportant: DWORD;
  end;
  {$EXTERNALSYM cwBITMAPINFOHEADER}
  BITMAPINFOHEADER = cwBITMAPINFOHEADER;
  {$EXTERNALSYM BITMAPINFOHEADER}


  PBitmapinfo = ^TBitmapinfo;
  BITMAPINFO = record
    bmiHeader: BITMAPINFOHEADER;
    bmiColors: array[0..0] of RGBQUAD;
  end;
  {$EXTERNALSYM BITMAPINFO}
  TBitmapinfo = BITMAPINFO;
  {$EXTERNALSYM TBitmapinfo}


  PMFT_REGISTER_TYPE_INFO = ^MFT_REGISTER_TYPE_INFO;
  cwMFT_REGISTER_TYPE_INFO = record
	  guidMajorType: TGUID;
	  guidSubtype: TGUID;
  end;
  {$EXTERNALSYM cwMFT_REGISTER_TYPE_INFO}
  MFT_REGISTER_TYPE_INFO = cwMFT_REGISTER_TYPE_INFO;
  {$EXTERNALSYM MFT_REGISTER_TYPE_INFO}
  TMftRegisterTypeInfoArray = array of MFT_REGISTER_TYPE_INFO;
  {$EXTERNALSYM TMftRegisterTypeInfoArray}


  PMFVideoInterlaceMode = ^MFVideoInterlaceMode;
  _MFVideoInterlaceMode                        = (
	  MFVideoInterlace_Unknown                     = 0,
	  MFVideoInterlace_Progressive                 = 2,
	  MFVideoInterlace_FieldInterleavedUpperFirst  = 3,
   	MFVideoInterlace_FieldInterleavedLowerFirst  = 4,
   	MFVideoInterlace_FieldSingleUpper            = 5,
   	MFVideoInterlace_FieldSingleLower            = 6,
	  MFVideoInterlace_MixedInterlaceOrProgressive = 7,
	  MFVideoInterlace_Last                        = (MFVideoInterlace_MixedInterlaceOrProgressive + 1),
	  MFVideoInterlace_ForceDWORD                  = FORCEDWORD);
  {$EXTERNALSYM _MFVideoInterlaceMode}
  MFVideoInterlaceMode = _MFVideoInterlaceMode;
  {$EXTERNALSYM MFVideoInterlaceMode}

const
  MFVideoInterlace_FieldSingleUpperFirst = MFVideoInterlace_FieldSingleUpper;
  {$EXTERNALSYM MFVideoInterlace_FieldSingleUpperFirst}
  MFVideoInterlace_FieldSingleLowerFirst = MFVideoInterlace_FieldSingleLower;
  {$EXTERNALSYM MFVideoInterlace_FieldSingleLowerFirst}


type

  PMFVideoTransferFunction = ^MFVideoTransferFunction;
  _MFVideoTransferFunction    = (
	  MFVideoTransFunc_Unknown    = 0,
	  MFVideoTransFunc_10         = 1,
	  MFVideoTransFunc_18         = 2,
	  MFVideoTransFunc_20         = 3,
	  MFVideoTransFunc_22         = 4,
	  MFVideoTransFunc_709        = 5,
	  MFVideoTransFunc_240M       = 6,
	  MFVideoTransFunc_sRGB       = 7,
	  MFVideoTransFunc_28         = 8,
	  MFVideoTransFunc_Log_100    = 9,
	  MFVideoTransFunc_Log_316    = 10,
	  MFVideoTransFunc_709_sym    = 11,
    MFVideoTransFunc_2020_const	= 12,
    MFVideoTransFunc_2020	      = 13,
    MFVideoTransFunc_26	        = 14,
    MFVideoTransFunc_2084	      = 15, // SMPTE ST.2084
    MFVideoTransFunc_HLG	      = 16, // Hybrid Log-Gamma, ARIB STD-B67
// >= NTDDI_WIN10_RS4)
    MFVideoTransFunc_10_rel     = 17, // No gamma, display referred (relative)
// end
   	MFVideoTransFunc_Last       = (MFVideoTransFunc_HLG + 1),
	  MFVideoTransFunc_ForceDWORD = FORCEDWORD);
  {$EXTERNALSYM _MFVideoTransferFunction}
  MFVideoTransferFunction = _MFVideoTransferFunction;
  {$EXTERNALSYM MFVideoTransferFunction}


  PMFVideoPrimaries = ^MFVideoPrimaries;
  _MFVideoPrimaries              = (
	  MFVideoPrimaries_Unknown       = 0,
    MFVideoPrimaries_reserved      = 1,
	  MFVideoPrimaries_BT709         = 2,
   	MFVideoPrimaries_BT470_2_SysM  = 3,
   	MFVideoPrimaries_BT470_2_SysBG = 4,
   	MFVideoPrimaries_SMPTE170M     = 5,
   	MFVideoPrimaries_SMPTE240M     = 6,
   	MFVideoPrimaries_EBU3213       = 7,
   	MFVideoPrimaries_SMPTE_C       = 8,
    MFVideoPrimaries_BT2020	       = 9,
    MFVideoPrimaries_XYZ	         = 10,
    MFVideoPrimaries_DCI_P3	       = 11,
    MFVideoPrimaries_ACES	         = 12,
   	MFVideoPrimaries_Last          = (MFVideoPrimaries_ACES + 1),
   	MFVideoPrimaries_ForceDWORD    = FORCEDWORD);
  {$EXTERNALSYM _MFVideoPrimaries}
  MFVideoPrimaries = _MFVideoPrimaries;
  {$EXTERNALSYM MFVideoPrimaries}

  PMFVideoLighting = ^MFVideoLighting;
  _MFVideoLighting             = (
	  MFVideoLighting_Unknown    = 0,
	  MFVideoLighting_bright     = 1,
	  MFVideoLighting_office     = 2,
	  MFVideoLighting_dim        = 3,
	  MFVideoLighting_dark       = 4,
	  MFVideoLighting_Last       = (MFVideoLighting_dark + 1),
	  MFVideoLighting_ForceDWORD = FORCEDWORD);
  {$EXTERNALSYM _MFVideoLighting}
  MFVideoLighting = _MFVideoLighting;
  {$EXTERNALSYM MFVideoLighting}

  PMFVideoTransferMatrix = ^MFVideoTransferMatrix;
  _MFVideoTransferMatrix           = (
	  MFVideoTransferMatrix_Unknown    = 0,
	  MFVideoTransferMatrix_BT709      = 1,
   	MFVideoTransferMatrix_BT601      = 2,
   	MFVideoTransferMatrix_SMPTE240M  = 3,
    MFVideoTransferMatrix_BT2020_10	 = 4,
    MFVideoTransferMatrix_BT2020_12	 = 5,
   	MFVideoTransferMatrix_Last       = (MFVideoTransferMatrix_BT2020_12 + 1),
   	MFVideoTransferMatrix_ForceDWORD = FORCEDWORD);
  {$EXTERNALSYM _MFVideoTransferMatrix}
  MFVideoTransferMatrix = _MFVideoTransferMatrix;
  {$EXTERNALSYM MFVideoTransferMatrix}


  PMFVideoChromaSubsampling = ^MFVideoChromaSubsampling;
  _MFVideoChromaSubsampling                               = (
   	MFVideoChromaSubsampling_Unknown                        = 0,
	  MFVideoChromaSubsampling_ProgressiveChroma              = $8,
	  MFVideoChromaSubsampling_Horizontally_Cosited           = $4,
	  MFVideoChromaSubsampling_Vertically_Cosited             = $2,
	  MFVideoChromaSubsampling_Vertically_AlignedChromaPlanes = $1,
	  MFVideoChromaSubsampling_MPEG2                          = (Ord(MFVideoChromaSubsampling_Horizontally_Cosited) or
	                                                             Ord(MFVideoChromaSubsampling_Vertically_AlignedChromaPlanes)),
	  MFVideoChromaSubsampling_MPEG1                          = MFVideoChromaSubsampling_Vertically_AlignedChromaPlanes,
	  MFVideoChromaSubsampling_DV_PAL                         = (Ord(MFVideoChromaSubsampling_Horizontally_Cosited) or
	                                                             Ord(MFVideoChromaSubsampling_Vertically_Cosited)),
	  MFVideoChromaSubsampling_Cosited                        = ((Ord(MFVideoChromaSubsampling_Horizontally_Cosited) or
	                                                              Ord(MFVideoChromaSubsampling_Vertically_Cosited)) or
                                                                Ord(MFVideoChromaSubsampling_Vertically_AlignedChromaPlanes)),
	  MFVideoChromaSubsampling_Last                           = (MFVideoChromaSubsampling_Cosited + 1),
	  {$EXTERNALSYM MFVideoChromaSubsampling_Last}
	  MFVideoChromaSubsampling_ForceDWORD                     = FORCEDWORD);
  {$EXTERNALSYM _MFVideoChromaSubsampling}
  MFVideoChromaSubsampling = _MFVideoChromaSubsampling;
  {$EXTERNALSYM MFVideoChromaSubsampling}


  PMFNominalRange = ^MFNominalRange;
  _MFNominalRange           = (
	  MFNominalRange_Unknown    = 0,
	  MFNominalRange_Normal     = 1,
	  MFNominalRange_Wide       = 2,
	  MFNominalRange_0_255      = 1,
	  MFNominalRange_16_235     = 2,
	  MFNominalRange_48_208     = 3,
	  MFNominalRange_64_127     = 4,
	  MFNominalRange_Last       = (MFNominalRange_64_127 + 1),
	  MFNominalRange_ForceDWORD = FORCEDWORD);
  {$EXTERNALSYM _MFNominalRange}
  MFNominalRange = _MFNominalRange;
  {$EXTERNALSYM MFNominalRange}

  PMFVideoFlags = ^MFVideoFlags;
  _MFVideoFlags                     = (
	  MFVideoFlag_PAD_TO_Mask           = ($1 or $2),
	  MFVideoFlag_PAD_TO_None           = (0 * $1),
	  MFVideoFlag_PAD_TO_4x3            = (1 * $1),
	  MFVideoFlag_PAD_TO_16x9           = (2 * $1),
   	MFVideoFlag_SrcContentHintMask    = (($4 or $8) or $10),
   	MFVideoFlag_SrcContentHintNone    = (0 * $4),
   	MFVideoFlag_SrcContentHint16x9    = (1 * $4),
   	MFVideoFlag_SrcContentHint235_1   = (2 * $4),
   	MFVideoFlag_AnalogProtected       = $20,
   	MFVideoFlag_DigitallyProtected    = $40,
   	MFVideoFlag_ProgressiveContent    = $80,
   	MFVideoFlag_FieldRepeatCountMask  = (($100 or $200) or $400),
   	MFVideoFlag_FieldRepeatCountShift = 8,
   	MFVideoFlag_ProgressiveSeqReset   = $800,
   	MFVideoFlag_PanScanEnabled        = $20000,
   	MFVideoFlag_LowerFieldFirst       = $40000,
   	MFVideoFlag_BottomUpLinearRep     = $80000,
  	MFVideoFlags_DXVASurface          = $100000,
  	MFVideoFlags_RenderTargetSurface  = $400000,
  	MFVideoFlags_ForceQWORD           = $7FFFFFFF);
  {$EXTERNALSYM _MFVideoFlags}
  MFVideoFlags = _MFVideoFlags;
  {$EXTERNALSYM MFVideoFlags}

  PMFRatio = ^MFRatio;
  _MFRatio = record
	  Numerator: DWORD;
	  Denominator: DWORD;
  end;
  {$EXTERNALSYM _MFRatio}
  MFRatio = _MFRatio;
  {$EXTERNALSYM MFRatio}

  PMFOffset = ^MFOffset;
  _MFOffset = record
   	fract: WORD;
	  value: SHORT;
  end;
  {$EXTERNALSYM _MFOffset}
  MFOffset = _MFOffset;
  {$EXTERNALSYM MFOffset}

  PMFVideoArea = ^MFVideoArea;
  _MFVideoArea = record
	  OffsetX: MFOffset;  // Contains the x-coordinate of the upper-left corner of the rectangle.
                        // This coordinate might have a fractional value.
	  OffsetY: MFOffset;  // Contains the y-coordinate of the upper-left corner of the rectangle.
                        // This coordinate might have a fractional value.
   	Area: SIZE;         // A SIZE structure that contains the width and height of the rectangle.
  end;
  {$EXTERNALSYM _MFVideoArea}
  MFVideoArea = _MFVideoArea;
  {$EXTERNALSYM MFVideoArea}


  PMFVideoInfo = ^MFVideoInfo;
  _MFVideoInfo = record
    dwWidth: DWORD;
    dwHeight: DWORD;
    PixelAspectRatio: MFRatio;
    SourceChromaSubsampling: MFVideoChromaSubsampling;
    InterlaceMode: MFVideoInterlaceMode;
    TransferFunction: MFVideoTransferFunction;
    ColorPrimaries: MFVideoPrimaries;
    TransferMatrix: MFVideoTransferMatrix;
    SourceLighting: MFVideoLighting;
    FramesPerSecond: MFRatio;
    NominalRange: MFNominalRange;
    GeometricAperture: MFVideoArea;
    MinimumDisplayAperture: MFVideoArea;
    PanScanAperture: MFVideoArea;
    VideoFlags: UInt64;
  end;
  {$EXTERNALSYM _MFVideoInfo}
//####old  MFVideoInfo = _MFVideoInfo;
  MFVideoInfo = _MFVideoInfo;
  {$EXTERNALSYM MFVideoInfo}


  PMFAYUVSample = ^MFAYUVSample;
  __MFAYUVSample = record
	  bCrValue: Byte;
	  bCbValue: Byte;
	  bYValue: Byte;
	  bSampleAlpha8: Byte;
  end;
  {$EXTERNALSYM __MFAYUVSample}
  MFAYUVSample = __MFAYUVSample;
  {$EXTERNALSYM MFAYUVSample}


  PMFARGB = ^MFARGB;
  _MFARGB = record
	  rgbBlue: Byte;
	  rgbGreen: Byte;
	  rgbRed: Byte;
	  rgbAlpha: Byte;
  end;
  {$EXTERNALSYM _MFARGB}
  MFARGB = _MFARGB;
  {$EXTERNALSYM MFARGB}


  PMFPaletteEntry = ^MFPaletteEntry;
	_MFPaletteEntry = record
	  ARGB: MFARGB;
	  AYCbCr: MFAYUVSample;
  end;
	{$EXTERNALSYM _MFPaletteEntry}
  MFPaletteEntry = _MFPaletteEntry;
  {$EXTERNALSYM MFPaletteEntry}


  PMFVideoSurfaceInfo = ^MFVideoSurfaceInfo;
  _MFVideoSurfaceInfo = record
	  Format: DWORD;
	  PaletteEntries: DWORD;
	  Palette: array of MFPaletteEntry;
  end;
  {$EXTERNALSYM _MFVideoSurfaceInfo}
  MFVideoSurfaceInfo = _MFVideoSurfaceInfo;
  {$EXTERNALSYM MFVideoSurfaceInfo}


  PMFVideoCompressedInfo = ^MFVideoCompressedInfo;
  _MFVideoCompressedInfo = record
	  AvgBitrate: LONGLONG;
	  AvgBitErrorRate: LONGLONG;
	  MaxKeyFrameSpacing: DWORD;
  end;
  {$EXTERNALSYM _MFVideoCompressedInfo}
  MFVideoCompressedInfo = _MFVideoCompressedInfo;
  {$EXTERNALSYM MFVideoCompressedInfo}


	PMFVIDEOFORMAT = ^MFVIDEOFORMAT;
  _MFVIDEOFORMAT = record
	  dwSize: DWORD;
	  videoInfo: MFVideoInfo;
	  guidFormat: TGUID;
	  compressedInfo: MFVideoCompressedInfo;
	  surfaceInfo: MFVideoSurfaceInfo;
  end;
  {$EXTERNALSYM _MFVIDEOFORMAT}
	MFVIDEOFORMAT = _MFVIDEOFORMAT;
	{$EXTERNALSYM MFVIDEOFORMAT}


  PMFStandardVideoFormat = ^MFStandardVideoFormat;
	_MFStandardVideoFormat        = (
   	MFStdVideoFormat_reserved       = 0,
    MFStdVideoFormat_NTSC         = (MFStdVideoFormat_reserved + 1),
    MFStdVideoFormat_PAL          = (MFStdVideoFormat_NTSC + 1),
    MFStdVideoFormat_DVD_NTSC     = (MFStdVideoFormat_PAL + 1),
    MFStdVideoFormat_DVD_PAL      = (MFStdVideoFormat_DVD_NTSC + 1),
    MFStdVideoFormat_DV_PAL       = (MFStdVideoFormat_DVD_PAL + 1),
    MFStdVideoFormat_DV_NTSC      = (MFStdVideoFormat_DV_PAL + 1),
    MFStdVideoFormat_ATSC_SD480i  = (MFStdVideoFormat_DV_NTSC + 1),
    MFStdVideoFormat_ATSC_HD1080i = (MFStdVideoFormat_ATSC_SD480i + 1),
	 	MFStdVideoFormat_ATSC_HD720p  = (MFStdVideoFormat_ATSC_HD1080i + 1));
	{$EXTERNALSYM _MFStandardVideoFormat}
  MFStandardVideoFormat = _MFStandardVideoFormat;
  {$EXTERNALSYM MFStandardVideoFormat}


  PMFBYTESTREAM_SEEK_ORIGIN = ^MFBYTESTREAM_SEEK_ORIGIN;
  _MFBYTESTREAM_SEEK_ORIGIN = (
   	msoBegin                  = 0,
   	msoCurrent                = (msoBegin  + 1));
  {$EXTERNALSYM _MFBYTESTREAM_SEEK_ORIGIN}
  MFBYTESTREAM_SEEK_ORIGIN = _MFBYTESTREAM_SEEK_ORIGIN;
  {$EXTERNALSYM MFBYTESTREAM_SEEK_ORIGIN}


  PMF_FILE_ACCESSMODE = ^MF_FILE_ACCESSMODE;
  cwMF_FILE_ACCESSMODE    = (
	  MF_ACCESSMODE_READ      = 1,
   	MF_ACCESSMODE_WRITE     = 2,
   	MF_ACCESSMODE_READWRITE = 3);
  {$EXTERNALSYM cwMF_FILE_ACCESSMODE}
  MF_FILE_ACCESSMODE = cwMF_FILE_ACCESSMODE;
  {$EXTERNALSYM MF_FILE_ACCESSMODE}


  PMF_FILE_OPENMODE = ^MF_FILE_OPENMODE;
  cwMF_OPENMODE_FAIL_IF_NOT_EXIST = (
    MF_OPENMODE_FAIL_IF_NOT_EXIST   = 0,
	  MF_OPENMODE_FAIL_IF_EXIST       = 1,
	  MF_OPENMODE_RESET_IF_EXIST      = 2,
	  MF_OPENMODE_APPEND_IF_EXIST     = 3,
	  MF_OPENMODE_DELETE_IF_EXIST     = 4);
  {$EXTERNALSYM cwMF_OPENMODE_FAIL_IF_NOT_EXIST}
  MF_FILE_OPENMODE = cwMF_OPENMODE_FAIL_IF_NOT_EXIST;
  {$EXTERNALSYM MF_FILE_OPENMODE}


  PMF_FILE_FLAGS = ^MF_FILE_FLAGS;
  cwMF_FILE_FLAGS                     = (
    MF_FILEFLAGS_NONE                 = 0,
	  MF_FILEFLAGS_NOBUFFERING          = $1,
    MF_FILEFLAGS_ALLOW_WRITE_SHARING  = $2);
  {$EXTERNALSYM cwMF_FILE_FLAGS}
  MF_FILE_FLAGS = cwMF_FILE_FLAGS;
  {$EXTERNALSYM MF_FILE_FLAGS}


// >= Windows 7

  PMF_Plugin_Type = ^MF_Plugin_Type;
  _MF_Plugin_Type            = (
	  MF_Plugin_Type_MFT         = 0,
	  MF_Plugin_Type_MediaSource = 1);
  {$EXTERNALSYM _MF_Plugin_Type}
  MF_Plugin_Type = _MF_Plugin_Type;
  {$EXTERNALSYM MF_Plugin_Type}

//end  >= Windows 7

type


  //============================================================================
  // Interfaces
  //============================================================================


  // Interface IMFAttributes
  // =======================
  // Provides a generic way to store key/value pairs on an object.
  // The keys are GUIDs, and the values can be any of the following data types:
  // UINT32, UINT64, double, GUID, wide-character string, byte array, or IUnknown pointer.
  // The standard implementation of this interface holds a thread lock while values are added,
  // deleted, or retrieved.
  //
  PIMFAttributes = ^IMFAttributes;
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFAttributes);'}
  {$EXTERNALSYM IMFAttributes}
	IMFAttributes = interface(IUnknown)
	['{2cd2d921-c447-44a7-a13c-4adabfc247e3}']

    function GetItem(const guidKey: REFGUID;
                     var pValue: MfPROPVARIANT): HResult; stdcall;

    function GetItemType(const guidKey: REFGUID;
                         out pType: MF_ATTRIBUTE_TYPE): HResult; stdcall;

		function CompareItem(const guidKey: REFGUID;
                         const Value: REFPROPVARIANT;
                         out pbResult: BOOL): HResult; stdcall;

    function Compare(const pTheirs: IMFAttributes;
                     const MatchType: MF_ATTRIBUTES_MATCH_TYPE;
                     out pbResult: BOOL): HResult; stdcall;

    function GetUINT32(const guidKey: TGUID;
                       out punValue: UINT32): HResult; stdcall;

    function GetUINT64(const guidKey: TGUID;
                       out punValue: UINT64): HResult; stdcall;

    function GetDouble(const guidKey: TGUID;
                       out pfValue: Double): HResult; stdcall;

    function GetGUID(const guidKey: REFGUID;
                     out pguidValue: TGuid): HResult; stdcall;

    function GetStringLength(const guidKey: TGUID;
                             pcchLength: UINT32): HResult; stdcall;

    function GetString(const guidKey: REFGUID;
                       out pwszValue: LPWSTR;
                       out cchBufSize: UINT32;
                       pcchLength: UINT32): HResult; stdcall;

    function GetAllocatedString(const guidKey: REFGUID;
                                out ppwszValue: LPWSTR;
                                out pcchLength: UINT32): HResult; stdcall;

    function GetBlobSize(const guidKey: TGUID;
                         out pcbBlobSize: UINT32): HResult; stdcall;

    function GetBlob(const guidKey: TGUID;
                     out pBuf: PUINT8;
                     out cbBufSize: UINT32;
                     out pcbBlobSize: PUINT32): HResult; stdcall;
    // Usage example:
    // var
    //    pArea: MFVideoArea;
    //
    // Result := pType.GetBlob(MF_MT_PAN_SCAN_APERTURE,
    //                         @pArea,
    //                         sizeof(MFVideoArea),
    //                         Nil);

    function GetAllocatedBlob(const guidKey: TGUID;
                              out ppBuf: UINT8;
                              pcbSize: UINT32): HResult; stdcall;

    function GetUnknown(const guidKey: TGUID;
                        const riid: REFIID;
                        out ppv): HResult; stdcall;

    function SetItem(const guidKey: TGUID;
                     const Value: REFPROPVARIANT): HResult; stdcall;

    function DeleteItem(const guidKey: TGUID): HResult; stdcall;

    function DeleteAllItems(): HResult; stdcall;

    function SetUINT32(const guidKey: TGUID;
                       const unValue: UINT32): HResult; stdcall;

    function SetUINT64(const guidKey: TGUID;
                       const unValue: UINT64): HResult; stdcall;

    function SetDouble(const guidKey: TGUID;
                       const fValue: Double): HResult; stdcall;

    function SetGUID(const guidKey: TGUID;
                     const guidValue: REFGUID): HResult; stdcall;

    function SetString(const guidKey: TGUID;
                       const wszValue: LPCWSTR): HResult; stdcall;

    function SetBlob(const guidKey: TGUID;
                     pBuf: UINT8;
                     cbBufSize: UINT32): HResult; stdcall;

    function SetUnknown(const constguidKey: TGUID;
                        pUnk: IUnknown): HResult; stdcall;

    function LockStore(): HResult; stdcall;

    function UnlockStore(): HResult; stdcall;

    function GetCount(out pcItems: UINT32): HResult; stdcall;

    function GetItemByIndex(const unIndex: UINT32;
                            const guidKey: TGUID;
                            var pValue: MfPROPVARIANT): HResult; stdcall;

    function CopyAllItems(pDest: IMFAttributes): HResult; stdcall;

	end;
  IID_IMFAttributes = IMFAttributes;
  {$EXTERNALSYM IID_IMFAttributes}


  // Interface IMFMediaBuffer
  // ========================
  //     The IMFMediaBuffer interface represent a buffer of multimedia data
  //     for any possible multimedia type.
  //     It provides methods for accessing the buffer pointer, the current
  //     length, and the maximum length of the buffer
  //
  PIMFMediaBuffer = ^IMFMediaBuffer;
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFMediaBuffer);'}
  {$EXTERNALSYM IMFMediaBuffer}
  IMFMediaBuffer = interface(IUnknown)
	['{045FA593-8799-42b8-BC8D-8968C6453507}']

    function Lock(out ppbBuffer: PByte;     // Receives a pointer to the start of the buffer.
                  pcbMaxLength: PDWord;     // Receives the maximum amount of data that can be written to the buffer. This parameter can be Nil.
                  pcbCurrentLength: PDWord  // Receives the length of the valid data in the buffer, in bytes. This parameter can be Nil.
                  ): HResult; stdcall;

    function Unlock(): HResult; stdcall;

    function GetCurrentLength(out pcbCurrentLength: DWord): HResult; stdcall;

    function SetCurrentLength(const cbCurrentLength: DWord): HResult; stdcall;

    function GetMaxLength(out pcbMaxLength: DWord): HResult; stdcall;

  end;
  IID_IMFMediaBuffer = IMFMediaBuffer;
  {$EXTERNALSYM IID_IMFMediaBuffer}


  // Interface IMFSample
  // ===================
  // Represents a media sample, which is a container object for media data.
  // For video, a sample typically contains one video frame.
  // For audio data, a sample typically contains multiple audio samples, rather than a single sample of audio.
  // A media sample contains zero or more buffers.
  // Each buffer manages a block of memory, and is represented by the IMFMediaBuffer interface.
  // A sample can have multiple buffers.
  // The buffers are kept in an ordered list and accessed by index value.
  // It is also valid to have an empty sample with no buffers.
  //
  PIMFSample = ^IMFSample;
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFSample);'}
  {$EXTERNALSYM IMFSample}
  IMFSample = interface(IMFAttributes)
	['{c40a00f2-b93a-4d80-ae8c-5a1c634f58e4}']

    function GetSampleFlags(out pdwSampleFlags: DWord): HResult; stdcall;

    function SetSampleFlags(dwSampleFlags: DWord): HResult; stdcall;

    function GetSampleTime(out phnsSampleTime: LONGLONG): HResult; stdcall;

    function SetSampleTime(hnsSampleTime: LONGLONG): HResult; stdcall;

    function GetSampleDuration(out phnsSampleDuration: LONGLONG): HResult; stdcall;

    function SetSampleDuration(hnsSampleDuration: LONGLONG): HResult; stdcall;
    //
    // Methods to manage the sample's buffers
    //
    function GetBufferCount(out pdwBufferCount: DWord): HResult; stdcall;

    function GetBufferByIndex(dwIndex: DWord;
                              out ppBuffer: IMFMediaBuffer): HResult; stdcall;

    function ConvertToContiguousBuffer(out ppBuffer: IMFMediaBuffer): HResult; stdcall;

    function AddBuffer(pBuffer: IMFMediaBuffer): HResult; stdcall; // If sample does not support adding buffers, it returns MF_E_SAMPLE_UNSUPPORTED_OP.

    function RemoveBufferByIndex(dwIndex: DWord): HResult; stdcall;

    function RemoveAllBuffers(): HResult; stdcall;

    function GetTotalLength(out pcbTotalLength: DWord): HResult; stdcall;

    function CopyToBuffer(pBuffer: IMFMediaBuffer): HResult; stdcall;

  end;
  IID_IMFSample = IMFSample;
  {$EXTERNALSYM IID_IMFSample}


  // Interface IMF2DBuffer
  // ======================
  // Represents a buffer that contains a two-dimensional surface, such as a video frame.
  //
  // <summary>
  //   The IMF2DBuffer interface is supported by a media buffer whose multimedia data represents a 2D media type e.g. video.
  //   For these media types it is important to know the "pitch" of the buffer which indicates the number of
  //   bytes required to go from scanline to scanline.
  //   The IMF2DBuffer interface is obtained from a buffer by calling QueryInterface().
  //   For each 2D media type a contiguous standard representation is defined which is designed to be compatible with the
  //   standard layout of a DirectX surface when represented by system memory.
  //   In this representation the surface pitch is always positive and
  //   equal to the number of bytes taken up by a single row of pixels padded out to a 4-byte boundary.
  //
  //   Applications may still access 2D buffers using IMFBuffer.Lock().
  //   In that case the data in the buffer will always be in the standard contiguous format for the format of
  //   data represented by the buffer. An internal copy may be required on IMFBuffer.Lock()
  //   and IMFBuffer.Unlock() to achieve this.
  //   The copy will not occur if the buffer is already in the correct format.
  //   When a copy occurs an event will be generated to aid debugging performance issues resulting from the copy.
  //   Components that process 2D data should aim to use the IMF2DBuffer interface to access sample data.
  // </summary>
  //
  PIMF2DBuffer = ^IMF2DBuffer;
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMF2DBuffer);'}
  {$EXTERNALSYM IMF2DBuffer}
  IMF2DBuffer = interface(IUnknown)
	['{7DC9D5F9-9ED9-44ec-9BBF-0600BB589FBB}']

    function Lock2D(out pbScanline0: PByte; // Receives a pointer to the first byte of the top row of pixels in the image. The top row is defined as the top row when the image is presented to the viewer, and might not be the first row in memory.
                    out plPitch: LONG       // Receives the surface stride, in bytes.
                    ): HResult; stdcall;    // The stride might be negative, indicating that the image is oriented from the bottom up in memory.


    function Unlock2D(): HResult; stdcall;

    function GetScanline0AndPitch(out pbScanline0: PByte;
                                  out plPitch: LONG): HResult; stdcall;

    function IsContiguousFormat(out pfIsContiguous: BOOL): HResult; stdcall;

    function GetContiguousLength(out pcbLength: DWord): HResult; stdcall;

    function ContiguousCopyTo(out pbDestBuffer: PByte;
                              const cbDestBuffer: DWord): HResult; stdcall;

    function ContiguousCopyFrom(pbSrcBuffer: PByte;
                                cbSrcBuffer: DWord): HResult; stdcall;

  end;
  IID_IMF2DBuffer = IMF2DBuffer;
  {$EXTERNALSYM IID_IMF2DBuffer}


  // Interface IMFMediaType
  // ======================
  // Represents a description of a media format.
  //
  PIMFMediaType = ^IMFMediaType;
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFMediaType);'}
  {$EXTERNALSYM IMFMediaType}
  IMFMediaType = interface(IMFAttributes)
	['{44ae0fa8-ea31-4109-8d2e-4cae4997c555}']
    function GetMajorType(out pguidMajorType: TGuid): HResult; stdcall;

    function IsCompressedFormat(out pfCompressed: BOOL): HResult; stdcall;

    function IsEqual(pIMediaType: IMFMediaType;
                     out pdwFlags: DWord): HResult; stdcall;

    function GetRepresentation(const guidRepresentation: TGuid;
                               out ppvRepresentation): HResult; stdcall;

    function FreeRepresentation(const guidRepresentation: TGuid;
                                pvRepresentation: Pointer): HResult; stdcall;
  end;
  IID_IMFMediaType = IMFMediaType;
  {$EXTERNALSYM IID_IMFMediaType}


  // Interface IMFAudioMediaType
  // ===========================
  // Represents a description of an audio format.
  // (Vista only)
  // NOTE:  No longer available for use as of Windows 7
  //        Instead, use the media type attributes to get the properties of the audio format.
  //
  PIMFAudioMediaType = ^IMFAudioMediaType;
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFAudioMediaType);'}
  {$EXTERNALSYM IMFAudioMediaType}
  IMFAudioMediaType = interface(IMFMediaType)
	['{26a0adc3-ce26-4672-9304-69552edd3faf}']

		function GetAudioFormat(): PWAVEFORMATEX; stdcall;

  end;
  IID_IMFAudioMediaType = IMFAudioMediaType;
  {$EXTERNALSYM IID_IMFAudioMediaType}


  // Interface IMFVideoMediaType
  // ===========================
	// Represents a description of a video format.
  // NOTE: Applications should avoid using this interface except when a method or function requires an
  //       IMFVideoMediaType pointer as a parameter. You can get all of the format information from a
  //       video media type through the IMFAttributes interface, which IMFMediaType inherits.
  //
  PIMFVideoMediaType = ^IMFVideoMediaType;
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFVideoMediaType);'}
  {$EXTERNALSYM IMFVideoMediaType}
  IMFVideoMediaType = interface(IMFMediaType)
	['{b99f381f-a8f9-47a2-a5af-ca3a225a3890}']

    //Returns a pointer to an MFVIDEOFORMAT structure that describes the video format. (Deprecated)
		function GetVideoFormat(): PMFVIDEOFORMAT; stdcall;
    //Retrieves an alternative representation of the media type. (Deprecated)

		function GetVideoRepresentation(const guidRepresentation: TGuid;
                                    out ppvRepresentation;
                                    lStride: LONG): HResult; stdcall;
	end;
  IID_IMFVideoMediaType = IMFVideoMediaType;
  {$EXTERNALSYM IID_IMFVideoMediaType}


  // Interface IMFAsyncResult
  // ========================
  // Provides information about the result of an asynchronous operation.
  //
  PIMFAsyncResult = ^IMFAsyncResult;
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFAsyncResult);'}
  {$EXTERNALSYM IMFAsyncResult}
  IMFAsyncResult = interface(IUnknown)
	['{ac6b7889-0740-4d51-8619-905994a55cc6}']

    function GetState(out ppunkState: IUnknown): HResult; stdcall;

    function GetStatus(): HResult; stdcall;

    function SetStatus(hrStatus: HRESULT): HResult; stdcall;

    function GetObject(out ppObject: IUnknown): HResult; stdcall;

    function GetStateNoAddRef(): IUnknown; stdcall;

  end;
  IID_IMFAsyncResult = IMFAsyncResult;
  {$EXTERNALSYM IID_IMFAsyncResult}


  // Interface IMFAsyncCallback
  // ==========================
  // Callback interface to notify the application when an asynchronous method completes.
  //
  PIMFAsyncCallback = ^IMFAsyncCallback;
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFAsyncCallback);'}
  {$EXTERNALSYM IMFAsyncCallback}
  IMFAsyncCallback = interface(IUnknown)
	['{a27003cf-2354-4f2a-8d6a-ab7cff15437e}']

    function GetParameters(out pdwFlags: DWord;
                           out pdwQueue: DWord): HResult; stdcall;

    function Invoke(pAsyncResult: IMFAsyncResult): HResult; stdcall;

  end;
  IID_IMFAsyncCallback = IMFAsyncCallback;
  {$EXTERNALSYM IID_IMFAsyncCallback}


  // Interface IMFMediaEvent
  // =======================
  // Represents an event generated by a Media Foundation object.
  // Use this interface to get information about the event.
  // To get a pointer to this interface, call IMFMediaEventGenerator.BeginGetEvent or
  // IMFMediaEventGenerator.GetEvent on the event generator.
  //
  PIMFMediaEvent = ^IMFMediaEvent;
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFMediaEvent);'}
  {$EXTERNALSYM IMFMediaEvent}
  IMFMediaEvent = interface(IMFAttributes)
	['{DF598932-F10C-4E39-BBA2-C308F101DAA3}']

    function GetType(out pmet: MediaEventType): HResult; stdcall;

    function GetExtendedType(out pguidExtendedType: TGuid): HResult; stdcall;

    function GetStatus(out phrStatus: HRESULT): HResult; stdcall;

    function GetValue(out pvValue: mfPROPVARIANT): HResult; stdcall;

  end;
  IID_IMFMediaEvent = IMFMediaEvent;
  {$EXTERNALSYM IID_IMFMediaEvent}


  // Interface IMFMediaEventGenerator
  // ================================
  // Retrieves events from any Media Foundation object that generates events.
  // NOTE: An object that supports this interface maintains a queue of events.
  //       The client of the object can retrieve the events either synchronously or asynchronously.
  //       The synchronous method is GetEvent.
  //       The asynchronous methods are BeginGetEvent and EndGetEvent.
  //
  PIMFMediaEventGenerator = ^IMFMediaEventGenerator;
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFMediaEventGenerator);'}
  {$EXTERNALSYM IMFMediaEventGenerator}
  IMFMediaEventGenerator = interface(IUnknown)
	['{2CD0BD52-BCD5-4B89-B62C-EADC0C031E7D}']

    function GetEvent(dwFlags: DWord;
                      out ppEvent: IMFMediaEvent): HResult; stdcall;

    function BeginGetEvent(pCallback: IMFAsyncCallback;
                           punkState: IUnknown): HResult; stdcall;

    function EndGetEvent(pResult: IMFAsyncResult;
                         out ppEvent: IMFMediaEvent): HResult; stdcall;

    function QueueEvent(met: MediaEventType;
                        guidExtendedType: REFGUID;
                        hrStatus: HRESULT;
                        pvValue: MfPROPVARIANT): HResult; stdcall;
  end;
  IID_IMFMediaEventGenerator = IMFMediaEventGenerator;
  {$EXTERNALSYM IID_IMFMediaEventGenerator}


  // Interface IMFRemoteAsyncCallback
  // ================================
  // Used by the Microsoft Media Foundation proxy/stub DLL to marshal certain asynchronous method calls across process boundaries.
  // Applications do not use or implement this interface.
  // So, it's just here to complete the header translation, for your convenience...
  //
  PIMFRemoteAsyncCallback = ^IMFRemoteAsyncCallback;
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFRemoteAsyncCallback);'}
  {$EXTERNALSYM IMFRemoteAsyncCallback}
  IMFRemoteAsyncCallback = interface(IUnknown)
	['{a27003d0-2354-4f2a-8d6a-ab7cff15437e}']

    function Invoke(const hr: HRESULT;
                    pRemoteResult: IUnknown): HResult; stdcall;
  end;
  IID_IMFRemoteAsyncCallback = IMFRemoteAsyncCallback;
  {$EXTERNALSYM IID_IMFRemoteAsyncCallback}


  // Interface IMFByteStream
  // =======================
  // Represents a byte stream from some data source, which might be a local file, a network file,
  // or some other source.
  // The IMFByteStream interface supports the typical stream operations, such as reading, writing, and seeking.
  //
  PIMFByteStream = ^IMFByteStream;
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFByteStream);'}
  {$EXTERNALSYM IMFByteStream}
  IMFByteStream = interface(IUnknown)
	['{ad4c1b00-4bf7-422f-9175-756693d9130d}']

    function GetCapabilities(out pdwCapabilities: DWord): HResult; stdcall;

    function GetLength(out pqwLength: QWORD): HResult; stdcall;

    function SetLength(qwLength: QWORD): HResult; stdcall;

    function GetCurrentPosition(out pqwPosition: QWORD): HResult; stdcall;

    function SetCurrentPosition(const qwPosition: QWORD): HResult; stdcall;

    function IsEndOfStream(out pfEndOfStream: BOOL): HResult; stdcall;

    function Read(pb: PByte;
                  cb: ULONG;
                  out pcbRead: PByte): HResult; stdcall;

    function BeginRead(pb: PByte;
                       cb: ULONG;
                       pCallback: IMFAsyncCallback;
                       punkState: IUnknown): HResult; stdcall;

    function EndRead(pResult: IMFAsyncResult;
                     out pcbRead: ULONG): HResult; stdcall;

    function Write(pb: PByte;
                   cb: ULONG;
                   out pcbWritten: ULONG): HResult; stdcall;

    function BeginWrite(pb: PByte;
                        cb: ULONG;
                        pCallback: IMFAsyncCallback;
                        punkState: IUnknown): HResult; stdcall;

    function EndWrite(pResult: IMFAsyncResult;
                      out pcbWritten: ULONG): HResult; stdcall;

    function Seek(SeekOrigin: MFBYTESTREAM_SEEK_ORIGIN;
                  llSeekOffset: LONGLONG;
                  dwSeekFlags: DWord;
                  out pqwCurrentPosition: QWORD): HResult; stdcall;

    function Flush(): HResult; stdcall;

    function Close(): HResult; stdcall;

  end;
  IID_IMFByteStream = IMFByteStream;
  {$EXTERNALSYM IID_IMFByteStream}


  // Interface IMFCollection
  // =======================
  // Represents a generic collection of IUnknown pointers.
  // NOTE: To create an empty collection object, call MFCreateCollection.
  //
  PIMFCollection = ^IMFCollection;
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFCollection);'}
  {$EXTERNALSYM IMFCollection}
  IMFCollection = interface(IUnknown)
	['{5BC8A76B-869A-46a3-9B03-FA218A66AEBE}']

    function GetElementCount(out pcElements: DWord): HResult; stdcall;

    function GetElement(const dwElementIndex: DWord;
                        var ppUnkElement: IUnknown): HResult; stdcall;

    function AddElement(pUnkElement: IUnknown): HResult; stdcall;

    function RemoveElement(const dwElementIndex: DWord;
                           out ppUnkElement: IUnknown): HResult; stdcall;

    function InsertElementAt(const dwIndex: DWord;
                             out pUnknown_: IUnknown): HResult; stdcall;

    function RemoveAllElements(): HResult; stdcall;

  end;
  IID_IMFCollection = IMFCollection;
  {$EXTERNALSYM IID_IMFCollection}


  // Interface IMFMediaEventQueue
  // ============================
  // Provides an event queue for applications that need to implement the IMFMediaEventGenerator interface.
  // This interface is exposed by a helper object that implements an event queue.
  // If you are writing a component that implements the IMFMediaEventGenerator interface,
  // you can use this object in your implementation.
  // The event queue object is thread safe and provides methods to queue events and to pull them from
  // the queue either synchronously or asynchronously.
  // To create the event queue object, call MFCreateEventQueue.
  //
  PIMFMediaEventQueue = ^IMFMediaEventQueue;
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFMediaEventQueue);'}
  {$EXTERNALSYM IMFMediaEventQueue}
  IMFMediaEventQueue = interface(IUnknown)
	['{36f846fc-2256-48b6-b58e-e2b638316581}']

    function GetEvent(dwFlags: DWord;
                      out ppEvent: IMFMediaEvent): HResult; stdcall;

    function BeginGetEvent(pCallback: IMFAsyncCallback;
                           punkState: IUnknown): HResult; stdcall;

    function EndGetEvent(pResult: IMFAsyncResult;
                         out ppEvent: IMFMediaEvent): HResult; stdcall;

    function QueueEvent(pEvent: IMFMediaEvent): HResult; stdcall;

    function QueueEventParamVar(met: MediaEventType;
                                guidExtendedType: REFGUID;
                                hrStatus: HRESULT;
                                pvValue: MfPROPVARIANT): HResult; stdcall;

    function QueueEventParamUnk(met: MediaEventType;
                                guidExtendedType: REFGUID;
                                hrStatus: HRESULT;
                                pUnk: IUnknown): HResult; stdcall;

    function Shutdown(): HResult; stdcall;
  end;
  IID_IMFMediaEventQueue = IMFMediaEventQueue;
  {$EXTERNALSYM IID_IMFMediaEventQueue}


  // Interface IMFActivate
  // =====================
  // Enables the application to defer the creation of an object.
  // This interface is exposed by activation objects.
  // NOTE:  Typically, the application calls some function that returns an IMFActivate pointer and
  //        then passes that pointer to another component.
  //        The other component calls ActivateObject at a later time to create the object.
  //        In the protected media path (PMP), the IMFActivate pointer might be marshaled to the
  //        protected process, so that the object can be created in that process.
  //
  PIMFActivate = ^IMFActivate;
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFActivate);'}
  {$EXTERNALSYM IMFActivate}
  IMFActivate = interface(IMFAttributes)
	['{7FEE9E9A-4A89-47a6-899C-B6A53A70FB67}']

    function ActivateObject(const riid: REFIID;
                            out ppv): HResult; stdcall;
    // Creates the object associated with this activation object.

    function ShutdownObject(): HResult; stdcall;
    // Shuts down the created object.

    function DetachObject(): HResult; stdcall;
    // Detaches the created object from the activation object.

   end;
  IID_IMFActivate = IMFActivate;
  {$EXTERNALSYM IID_IMFActivate}


// >= Windows 8
  // Interface IMFPluginControl
  // ==========================
  // Controls how media sources and transforms are enumerated in Microsoft Media Foundation.
  // To get a pointer to this interface, call MFGetPluginControl.
  //
  PIMFPluginControl = ^IMFPluginControl;
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFPluginControl);'}
  {$EXTERNALSYM IMFPluginControl}
  IMFPluginControl = interface(IUnknown)
	['{5c6c44bf-1db6-435b-9249-e8cd10fdec96}']

    function GetPreferredClsid(pluginType: DWord;
                               selector: LPCWSTR;
                               out clsid: CLSID): HResult; stdcall;

    function GetPreferredClsidByIndex(pluginType: DWord;
                                      const index: Dword;
                                      out selector: LPWSTR;
                                      out clsid: CLSID): HResult; stdcall;

    function SetPreferredClsid(pluginType: DWord;
                               selector: LPCWSTR;
                               const clsid: CLSID): HResult; stdcall;

		function IsDisabled(pluginType: DWord;
                        clsid: REFCLSID): HResult; stdcall;

    function GetDisabledByIndex(pluginType: DWord;
                                const index: DWord;
                                out clsid: CLSID): HResult; stdcall;

    function SetDisabled(pluginType: DWord;
                         clsid: REFCLSID;
                         disabled: BOOL): HResult; stdcall;
	end;
  IID_IMFPluginControl = IMFPluginControl;
  {$EXTERNALSYM IID_IMFPluginControl}
  // end >= Windows 8


  // NEW since SDK 8
  // ===============
  //
  PMfPluginControlPolicy = ^MF_PLUGIN_CONTROL_POLICY;
  MF_PLUGIN_CONTROL_POLICY                            = (
    MF_PLUGIN_CONTROL_POLICY_USE_ALL_PLUGINS          = 0,
    MF_PLUGIN_CONTROL_POLICY_USE_APPROVED_PLUGINS     = 1,
    MF_PLUGIN_CONTROL_POLICY_USE_WEB_PLUGINS          = 2,
    MF_PLUGIN_CONTROL_POLICY_USE_WEB_PLUGINS_EDGEMODE = 3);
  {$EXTERNALSYM MF_PLUGIN_CONTROL_POLICY}



  // Interface IMFPluginControl2
  // ===========================
  PIMFPluginControl2 = ^IMFPluginControl2;
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFPluginControl2);'}
  {$EXTERNALSYM IMFPluginControl2}
  IMFPluginControl2 = interface(IMFPluginControl)
	['{C6982083-3DDC-45CB-AF5E-0F7A8CE4DE77}']

    function SetPolicy(policy: MF_PLUGIN_CONTROL_POLICY): HResult; stdcall;

  end;
  // IMFPluginControl2
  IID_IMFPluginControl2 = IMFPluginControl2;
  {$EXTERNALSYM IID_IMFPluginControl2}



  // Interface IMFDXGIDeviceManager
  // ==============================
  //
  PIMFDXGIDeviceManager = ^IMFDXGIDeviceManager;
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFDXGIDeviceManager);'}
  {$EXTERNALSYM IMFDXGIDeviceManager}
  IMFDXGIDeviceManager = interface(IUnknown)
	['{eb533d5d-2db6-40f8-97a9-494692014f07}']

    function CloseDeviceHandle(hDevice: THandle): HResult; stdcall;

    function GetVideoService(hDevice: THandle;
                             const riid: REFIID;
                             out ppService): HResult; stdcall;

    function LockDevice(hDevice: THandle;
                        const riid: REFIID;
                        out ppUnkDevice;
                        fBlock: BOOL): HResult; stdcall;

    function OpenDeviceHandle(phDevice: THandle): HResult; stdcall;

    function ResetDevice(pUnkDevice: IUnknown;
                         const resetToken: UINT): HResult; stdcall;

    function TestDevice(hDevice: THandle): HResult; stdcall;

    function UnlockDevice(hDevice: THandle;
                          fSaveState: BOOL): HResult; stdcall;


  end;
  // IMFDXGIDeviceManager
  IID_IMFDXGIDeviceManager = IMFDXGIDeviceManager;
  {$EXTERNALSYM IID_IMFDXGIDeviceManager}


  PMfStreamState = ^MF_STREAM_STATE;
  _MF_STREAM_STATE          = (
    MF_STREAM_STATE_STOPPED = 0,
    MF_STREAM_STATE_PAUSED  = (MF_STREAM_STATE_STOPPED + 1),
    MF_STREAM_STATE_RUNNING = (MF_STREAM_STATE_PAUSED + 1));
  {$EXTERNALSYM _MF_STREAM_STATE}
  MF_STREAM_STATE = _MF_STREAM_STATE;
  {$EXTERNALSYM MF_STREAM_STATE}


  // Interface IMFMuxStreamAttributesManager
  // =======================================
  //
  PIMFMuxStreamAttributesManager =^IMFMuxStreamAttributesManager;
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFMuxStreamAttributesManager);'}
  {$EXTERNALSYM IMFMuxStreamAttributesManager}
  IMFMuxStreamAttributesManager = interface(IUnknown)
	['{CE8BD576-E440-43B3-BE34-1E53F565F7E8}']

    function GetStreamCount(out pdwMuxStreamCount: DWORD): HResult; stdcall;

    function GetAttributes(dwMuxStreamIndex: DWORD;
                           out ppStreamAttributes: IMFAttributes): HResult; stdcall;

  end;
  IID_IMFMuxStreamAttributesManager = IMFMuxStreamAttributesManager;
  {$EXTERNALSYM IID_IMFMuxStreamAttributesManager}


  // Interface IMFMuxStreamMediaTypeManager
  // ======================================
  //
  PIMFMuxStreamMediaTypeManager =^IMFMuxStreamMediaTypeManager;
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFMuxStreamMediaTypeManager);'}
  {$EXTERNALSYM IMFMuxStreamMediaTypeManager}
  IMFMuxStreamMediaTypeManager = interface(IUnknown)
	['{505A2C72-42F7-4690-AEAB-8F513D0FFDB8}']

    function GetStreamCount(out pdwMuxStreamCount: DWORD): HResult; stdcall;

    function GetMediaType(dwMuxStreamIndex: DWORD;
                          out ppMediaType: IMFMediaType): HResult; stdcall;

    function GetStreamConfigurationCount(out pdwCount: DWORD): HResult; stdcall;

    function AddStreamConfiguration(ullStreamMask: ULONGLONG): HResult; stdcall;

    function RemoveStreamConfiguration(ullStreamMask: ULONGLONG): HResult; stdcall;

    function GetStreamConfiguration(ulIndex: DWORD;
                                    out pullStreamMask: ULONGLONG): HResult; stdcall;

  end;
  IID_IMFMuxStreamMediaTypeManager = IMFMuxStreamMediaTypeManager;
  {$EXTERNALSYM IID_IMFMuxStreamMediaTypeManager}


  // Interface IMFMuxStreamSampleManager
  // ===================================
  //
  PIMFMuxStreamSampleManager =^IMFMuxStreamSampleManager;
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFMuxStreamSampleManager);'}
  {$EXTERNALSYM IMFMuxStreamSampleManager}
  IMFMuxStreamSampleManager = interface(IUnknown)
	['{74ABBC19-B1CC-4E41-BB8B-9D9B86A8F6CA}']

    function GetStreamCount(out pdwMuxStreamCount: DWORD): HResult; stdcall;

    function GetSample(dwMuxStreamIndex: DWORD;
                       out ppSample: IMFSample): HResult; stdcall;

    function GetStreamConfiguration(): ULONGLONG; stdcall;

  end;
  IID_IMFMuxStreamSampleManager = IMFMuxStreamSampleManager;
  {$EXTERNALSYM IID_IMFMuxStreamSampleManager}


  // Interface IMFSecureBuffer
  // This interface encapsulates a secure buffer allocated by the frame server
  // trustlet.
  // =========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFSecureBuffer);'}
  {$EXTERNALSYM IMFSecureBuffer}
  IMFSecureBuffer = interface(IUnknown)
	['{C1209904-E584-4752-A2D6-7F21693F8B21}']
    // <summary>
    // Get the identifier for a secure buffer. This GUID can be used by
    // a device's trustlet to open a secure section containing the buffer.
    // </summary>
    function GetIdentifier(out pGuidIdentifier: TGUID): HResult; stdcall;

  end;
  IID_IMFSecureBuffer = IMFSecureBuffer;
  {$EXTERNALSYM IID_IMFSecureBuffer}



  // functions /////////////////////////////////////////////////////////////////

  function MFSerializeAttributesToStream(pAttr: IMFAttributes;
                                         dwOptions: DWord;
                                         pStm: IStream): HResult; stdcall;
  {$EXTERNALSYM MFSerializeAttributesToStream}

	function MFDeserializeAttributesFromStream(pAttr: IMFAttributes;
                                             dwOptions: DWord;
                                             pStm: IStream): HResult; stdcall;
	{$EXTERNALSYM MFDeserializeAttributesFromStream}



  // Additional Prototypes for ALL interfaces

type

  // The following array definitions can be used if you don't want to implement pointermath for
  // pointer to pointerarray definitions.
  // Otherwise use {$POINTERMATH ON/OFF} in your code.

  // Type identifiers for functions that needs the Array of IMFActivate or IMFMediaTypeArray.
  //
  // To declare parameters that must be dynamic arrays, you need to specify a type identifier.
  // Source: Delphi Help
  //
  // Advisable is to use static arrays with a max of WORD,
  // which should be a sufficient amount of elements to be returned.

  PIMFActivateArray = ^TIMFActivateArray;
  // dynamic
  // TIMFActivateArray = array of IMFActivate;
  // or static (preferred)
  TIMFActivateArray = array[0..65535] of IMFActivate;
  {$EXTERNALSYM TIMFActivateArray}


  // Used by function MfPack.MfIdl.MFCreateStreamDescriptor
  PIMFMediaTypeArray = ^TIMFMediaTypeArray;
  // dynamic
  // TIMFMediaTypeArray = array of IMFMediaType;
  // or static (preferred)
  TIMFMediaTypeArray = array[0..65535] of IMFMediaType;
  {$EXTERNALSYM TIMFMediaTypeArray}

  // End of Additional Prototypes


implementation

const
  MfObjectsLib = 'Mfplat.dll';

{$WARN SYMBOL_PLATFORM OFF}
  function MFSerializeAttributesToStream; external MfObjectsLib name 'MFSerializeAttributesToStream' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFDeserializeAttributesFromStream; external MfObjectsLib name 'MFDeserializeAttributesFromStream' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
{$WARN SYMBOL_PLATFORM ON}

  // Implement Additional Prototypes here.

end.
