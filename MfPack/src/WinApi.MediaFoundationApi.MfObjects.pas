// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - MediaFoundation
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.MediaFoundationApi.MfObjects.pas
// Kind: Pascal / Delphi unit
// Release date: 29-06-2012
// Language: ENU
//
// Revision Version: 3.0.1
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
// 13/08/2020 All                 Enigma release. New layout and namespaces
// 08/12/2020 Tony                Added updates from sdk 10.0.19041.0
// 26/01/2021 Tony                Fixed IMFCollection
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
// Related projects: MfPackX300
// Known Issues: -
//
// Compiler version: 23 up to 33
// SDK version: 10.0.19041.0
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
unit WinApi.MediaFoundationApi.MfObjects;

  {$HPPEMIT '#include "mfobjects.h"'}

interface

uses
  {WinApi}
  Winapi.Windows,
  WinApi.WinApiTypes,
  WinApi.Unknwn,
  WinApi.WinMM.MMReg,
  WinApi.MediaObj,
  {ActiveX}
  WinApi.ActiveX.PropSys,
  WinApi.ActiveX.PropIdl,
  WinApi.ActiveX.ObjIdlbase;


  {$WEAKPACKAGEUNIT ON}
  {$MINENUMSIZE 4}

  {$IFDEF WIN32}
    {$ALIGN 1}
  {$ELSE}
    {$ALIGN 8} // Win64
  {$ENDIF}

  {$WARN SYMBOL_PLATFORM OFF}

  {$I 'WinApiTypes.inc'}
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

  //
  // Async callback flags
  //

  MFASYNC_FAST_IO_PROCESSING_CALLBACK = $00000001;
  {$EXTERNALSYM MFASYNC_FAST_IO_PROCESSING_CALLBACK}
  MFASYNC_SIGNAL_CALLBACK             = $00000002;
  {$EXTERNALSYM MFASYNC_SIGNAL_CALLBACK}
  MFASYNC_BLOCKING_CALLBACK           = $00000004;
  {$EXTERNALSYM MFASYNC_BLOCKING_CALLBACK}
  MFASYNC_REPLY_CALLBACK              = $00000008; // Callback will reply via IMFAsyncCallback in GetObject()
  {$EXTERNALSYM MFASYNC_REPLY_CALLBACK}
  MFASYNC_LOCALIZE_REMOTE_CALLBACK    = $00000010; // The callback object is not apartment-agile and the callback pointer must be localized after an RPC
  {$EXTERNALSYM MFASYNC_LOCALIZE_REMOTE_CALLBACK}

  //
  // Async callback invocation queue IDs
  //

  MFASYNC_CALLBACK_QUEUE_UNDEFINED     = $00000000;
  {$EXTERNALSYM MFASYNC_CALLBACK_QUEUE_UNDEFINED}
  MFASYNC_CALLBACK_QUEUE_STANDARD      = $00000001;
  {$EXTERNALSYM MFASYNC_CALLBACK_QUEUE_STANDARD}
  MFASYNC_CALLBACK_QUEUE_RT            = $00000002;
  {$EXTERNALSYM MFASYNC_CALLBACK_QUEUE_RT}
  MFASYNC_CALLBACK_QUEUE_IO            = $00000003;
  {$EXTERNALSYM MFASYNC_CALLBACK_QUEUE_IO}
  MFASYNC_CALLBACK_QUEUE_TIMER         = $00000004;
  {$EXTERNALSYM MFASYNC_CALLBACK_QUEUE_TIMER}
  MFASYNC_CALLBACK_QUEUE_MULTITHREADED = $00000005;
  {$EXTERNALSYM MFASYNC_CALLBACK_QUEUE_MULTITHREADED}
  MFASYNC_CALLBACK_QUEUE_LONG_FUNCTION = $00000007;
  {$EXTERNALSYM MFASYNC_CALLBACK_QUEUE_LONG_FUNCTION}
  MFASYNC_CALLBACK_QUEUE_PRIVATE_MASK  = $FFFF0000;
  {$EXTERNALSYM MFASYNC_CALLBACK_QUEUE_PRIVATE_MASK}
  MFASYNC_CALLBACK_QUEUE_ALL           = MAXDW; // 0xFFFFFFFF
  {$EXTERNALSYM MFASYNC_CALLBACK_QUEUE_ALL}

type
  // MediaEventType
  PMediaEventType = ^MediaEventType;
  MediaEventType = DWord;
  {$EXTERNALSYM MediaEventType}

const
  //-------------------------------------------------------------------------
  // generic events
  //-------------------------------------------------------------------------

  MEUnknown                               = MediaEventType(0);
  {$EXTERNALSYM MEUnknown}
  MEError                                 = MediaEventType(1);
  {$EXTERNALSYM MEError}
  MEExtendedType                          = MediaEventType(2);
  {$EXTERNALSYM MEExtendedType}
  MENonFatalError                         = MediaEventType(3);
  {$EXTERNALSYM MENonFatalError}
  MEGenericV1Anchor                       = MENonFatalError;
  {$EXTERNALSYM MEGenericV1Anchor}

  //-------------------------------------------------------------------------
  // Media Session events:
  // Events of interest to applications using the Media Session
  //-------------------------------------------------------------------------

  MESessionUnknown                        = MediaEventType(100);
  {$EXTERNALSYM MESessionUnknown}
  MESessionTopologySet                    = MediaEventType(101);
  {$EXTERNALSYM MESessionTopologySet}
  MESessionTopologiesCleared              = MediaEventType(102);
  {$EXTERNALSYM MESessionTopologiesCleared}
  MESessionStarted                        = MediaEventType(103);
  {$EXTERNALSYM MESessionStarted}
  MESessionPaused                         = MediaEventType(104);
  {$EXTERNALSYM MESessionPaused}
  MESessionStopped                        = MediaEventType(105);
  {$EXTERNALSYM MESessionStopped}
  MESessionClosed                         = MediaEventType(106);
  {$EXTERNALSYM MESessionClosed}
  MESessionEnded                          = MediaEventType(107);
  {$EXTERNALSYM MESessionEnded}
  MESessionRateChanged                    = MediaEventType(108);
  {$EXTERNALSYM MESessionRateChanged}
  MESessionScrubSampleComplete            = MediaEventType(109);
  {$EXTERNALSYM MESessionScrubSampleComplete}
  MESessionCapabilitiesChanged            = MediaEventType(110);
  {$EXTERNALSYM MESessionCapabilitiesChanged}
  MESessionTopologyStatus                 = MediaEventType(111);
  {$EXTERNALSYM MESessionTopologyStatus}
  MESessionNotifyPresentationTime         = MediaEventType(112);
  {$EXTERNALSYM MESessionNotifyPresentationTime}
  MENewPresentation                       = MediaEventType(113);
  {$EXTERNALSYM MENewPresentation}
  MELicenseAcquisitionStart               = MediaEventType(114);
  {$EXTERNALSYM MELicenseAcquisitionStart}
  MELicenseAcquisitionCompleted           = MediaEventType(115);
  {$EXTERNALSYM MELicenseAcquisitionCompleted}
  MEIndividualizationStart                = MediaEventType(116);
  {$EXTERNALSYM MEIndividualizationStart}
  MEIndividualizationCompleted            = MediaEventType(117);
  {$EXTERNALSYM MEIndividualizationCompleted}
  MEEnablerProgress                       = MediaEventType(118);
  {$EXTERNALSYM MEEnablerProgress}
  MEEnablerCompleted                      = MediaEventType(119);
  {$EXTERNALSYM MEEnablerCompleted}
  MEPolicyError                           = MediaEventType(120);
  {$EXTERNALSYM MEPolicyError}
  MEPolicyReport                          = MediaEventType(121);
  {$EXTERNALSYM MEPolicyReport}
  MEBufferingStarted                      = MediaEventType(122);
  {$EXTERNALSYM MEBufferingStarted}
  MEBufferingStopped                      = MediaEventType(123);
  {$EXTERNALSYM MEBufferingStopped}
  MEConnectStart                          = MediaEventType(124);
  {$EXTERNALSYM MEConnectStart}
  MEConnectEnd                            = MediaEventType(125);
  {$EXTERNALSYM MEConnectEnd}
  MEReconnectStart                        = MediaEventType(126);
  {$EXTERNALSYM MEReconnectStart}
  MEReconnectEnd                          = MediaEventType(127);
  {$EXTERNALSYM MEReconnectEnd}
  MERendererEvent                         = MediaEventType(128);
  {$EXTERNALSYM MERendererEvent}
  MESessionStreamSinkFormatChanged        = MediaEventType(129);
  {$EXTERNALSYM MESessionStreamSinkFormatChanged}
  MESessionV1Anchor                       = MESessionStreamSinkFormatChanged;
  {$EXTERNALSYM MESessionV1Anchor}
  MESourceUnknown                         = MediaEventType(200);
  {$EXTERNALSYM MESourceUnknown}
  MESourceStarted                         = MediaEventType(201);
  {$EXTERNALSYM MESourceStarted}
  MEStreamStarted                         = MediaEventType(202);
  {$EXTERNALSYM MEStreamStarted}
  MESourceSeeked                          = MediaEventType(203);
  {$EXTERNALSYM MESourceSeeked}
  MEStreamSeeked                          = MediaEventType(204);
  {$EXTERNALSYM MEStreamSeeked}
  MENewStream                             = MediaEventType(205);
  {$EXTERNALSYM MENewStream}
  MEUpdatedStream                         = MediaEventType(206);
  {$EXTERNALSYM MEUpdatedStream}
  MESourceStopped                         = MediaEventType(207);
  {$EXTERNALSYM MESourceStopped}
  MEStreamStopped                         = MediaEventType(208);
  {$EXTERNALSYM MEStreamStopped}
  MESourcePaused                          = MediaEventType(209);
  {$EXTERNALSYM MESourcePaused}
  MEStreamPaused                          = MediaEventType(210);
  {$EXTERNALSYM MEStreamPaused}
  MEEndOfPresentation                     = MediaEventType(211);
  {$EXTERNALSYM MEEndOfPresentation}
  MEEndOfStream                           = MediaEventType(212);
  {$EXTERNALSYM MEEndOfStream}
  MEMediaSample                           = MediaEventType(213);  // Sent when a media stream delivers a new sample in response to a call to IMFMediaStream.RequestSample.
  {$EXTERNALSYM MEMediaSample}
  MEStreamTick                            = MediaEventType(214);  // Signals that a media stream does not have data available at a specified time.
  {$EXTERNALSYM MEStreamTick}
  MEStreamThinMode                        = MediaEventType(215);
  {$EXTERNALSYM MEStreamThinMode}
  MEStreamFormatChanged                   = MediaEventType(216);
  {$EXTERNALSYM MEStreamFormatChanged}
  MESourceRateChanged                     = MediaEventType(217);
  {$EXTERNALSYM MESourceRateChanged}
  MEEndOfPresentationSegment              = MediaEventType(218);
  {$EXTERNALSYM MEEndOfPresentationSegment}
  MESourceCharacteristicsChanged          = MediaEventType(219);
  {$EXTERNALSYM MESourceCharacteristicsChanged}
  MESourceRateChangeRequested             = MediaEventType(220);
  {$EXTERNALSYM MESourceRateChangeRequested}
  MESourceMetadataChanged                 = MediaEventType(221);
  {$EXTERNALSYM MESourceMetadataChanged}
  MESequencerSourceTopologyUpdated        = MediaEventType(222);
  {$EXTERNALSYM MESequencerSourceTopologyUpdated}
  MESourceV1Anchor                        = MESequencerSourceTopologyUpdated;
  {$EXTERNALSYM MESourceV1Anchor}
  MESinkUnknown                           = MediaEventType(300);
  {$EXTERNALSYM MESinkUnknown}
  MEStreamSinkStarted                     = MediaEventType(301);
  {$EXTERNALSYM MEStreamSinkStarted}
  MEStreamSinkStopped                     = MediaEventType(302);
  {$EXTERNALSYM MEStreamSinkStopped}
  MEStreamSinkPaused                      = MediaEventType(303);
  {$EXTERNALSYM MEStreamSinkPaused}
  MEStreamSinkRateChanged                 = MediaEventType(304);
  {$EXTERNALSYM MEStreamSinkRateChanged}
  MEStreamSinkRequestSample               = MediaEventType(305);
  {$EXTERNALSYM MEStreamSinkRequestSample}
  MEStreamSinkMarker                      = MediaEventType(306);
  {$EXTERNALSYM MEStreamSinkMarker}
  MEStreamSinkPrerolled                   = MediaEventType(307);
  {$EXTERNALSYM MEStreamSinkPrerolled}
  MEStreamSinkScrubSampleComplete         = MediaEventType(308);
  {$EXTERNALSYM MEStreamSinkScrubSampleComplete}
  MEStreamSinkFormatChanged               = MediaEventType(309);
  {$EXTERNALSYM MEStreamSinkFormatChanged}
  MEStreamSinkDeviceChanged               = MediaEventType(310);
  {$EXTERNALSYM MEStreamSinkDeviceChanged}
  MEQualityNotify                         = MediaEventType(311);
  {$EXTERNALSYM MEQualityNotify}
  MESinkInvalidated                       = MediaEventType(312);
  {$EXTERNALSYM MESinkInvalidated}
  MEAudioSessionNameChanged               = MediaEventType(313);
  {$EXTERNALSYM MEAudioSessionNameChanged}
  MEAudioSessionVolumeChanged             = MediaEventType(314);
  {$EXTERNALSYM MEAudioSessionVolumeChanged}
  MEAudioSessionDeviceRemoved             = MediaEventType(315);
  {$EXTERNALSYM MEAudioSessionDeviceRemoved}
  MEAudioSessionServerShutdown            = MediaEventType(316);
  {$EXTERNALSYM MEAudioSessionServerShutdown}
  MEAudioSessionGroupingParamChanged      = MediaEventType(317);
  {$EXTERNALSYM MEAudioSessionGroupingParamChanged}
  MEAudioSessionIconChanged               = MediaEventType(318);
  {$EXTERNALSYM MEAudioSessionIconChanged}
  MEAudioSessionFormatChanged             = MediaEventType(319);
  {$EXTERNALSYM MEAudioSessionFormatChanged}
  MEAudioSessionDisconnected              = MediaEventType(320);
  {$EXTERNALSYM MEAudioSessionDisconnected}
  MEAudioSessionExclusiveModeOverride     = MediaEventType(321);
  {$EXTERNALSYM MEAudioSessionExclusiveModeOverride}
  MESinkV1Anchor                          = MEAudioSessionExclusiveModeOverride;
  {$EXTERNALSYM MESinkV1Anchor}
  MECaptureAudioSessionVolumeChanged      = MediaEventType(322);
  {$EXTERNALSYM MECaptureAudioSessionVolumeChanged}
  MECaptureAudioSessionDeviceRemoved      = MediaEventType(323);
  {$EXTERNALSYM MECaptureAudioSessionDeviceRemoved}
  MECaptureAudioSessionFormatChanged      = MediaEventType(324);
  {$EXTERNALSYM MECaptureAudioSessionFormatChanged}
  MECaptureAudioSessionDisconnected       = MediaEventType(325);
  {$EXTERNALSYM MECaptureAudioSessionDisconnected}
  MECaptureAudioSessionExclusiveModeOverride  = MediaEventType(326);
  {$EXTERNALSYM MECaptureAudioSessionExclusiveModeOverride}
  MECaptureAudioSessionServerShutdown     = MediaEventType(327);
  {$EXTERNALSYM MECaptureAudioSessionServerShutdown}
  MESinkV2Anchor                          = MECaptureAudioSessionServerShutdown;
  {$EXTERNALSYM MESinkV2Anchor}
  METrustUnknown                          = MediaEventType(400);
  {$EXTERNALSYM METrustUnknown}
  MEPolicyChanged                         = MediaEventType(401);
  {$EXTERNALSYM MEPolicyChanged}
  MEContentProtectionMessage              = MediaEventType(402);
  {$EXTERNALSYM MEContentProtectionMessage}
  MEPolicySet                             = MediaEventType(403);
  {$EXTERNALSYM MEPolicySet}
  METrustV1Anchor                         = MEPolicySet;
  {$EXTERNALSYM METrustV1Anchor}
  MEWMDRMLicenseBackupCompleted           = MediaEventType(500);
  {$EXTERNALSYM MEWMDRMLicenseBackupCompleted}
  MEWMDRMLicenseBackupProgress            = MediaEventType(501);
  {$EXTERNALSYM MEWMDRMLicenseBackupProgress}
  MEWMDRMLicenseRestoreCompleted          = MediaEventType(502);
  {$EXTERNALSYM MEWMDRMLicenseRestoreCompleted}
  MEWMDRMLicenseRestoreProgress           = MediaEventType(503);
  {$EXTERNALSYM MEWMDRMLicenseRestoreProgress}
  MEWMDRMLicenseAcquisitionCompleted      = MediaEventType(506);
  {$EXTERNALSYM MEWMDRMLicenseAcquisitionCompleted}
  MEWMDRMIndividualizationCompleted       = MediaEventType(508);
  {$EXTERNALSYM MEWMDRMIndividualizationCompleted}
  MEWMDRMIndividualizationProgress        = MediaEventType(513);
  {$EXTERNALSYM MEWMDRMIndividualizationProgress}
  MEWMDRMProximityCompleted               = MediaEventType(514);
  {$EXTERNALSYM MEWMDRMProximityCompleted}
  MEWMDRMLicenseStoreCleaned              = MediaEventType(515);
  {$EXTERNALSYM MEWMDRMLicenseStoreCleaned}
  MEWMDRMRevocationDownloadCompleted      = MediaEventType(516);
  {$EXTERNALSYM MEWMDRMRevocationDownloadCompleted}
  MEWMDRMV1Anchor                         = MEWMDRMRevocationDownloadCompleted;
  {$EXTERNALSYM MEWMDRMV1Anchor}
  METransformUnknown                      = MediaEventType(600);
  {$EXTERNALSYM METransformUnknown}
  METransformNeedInput                    = METransformUnknown + 1;
  {$EXTERNALSYM METransformNeedInput}
  METransformHaveOutput                   = METransformNeedInput + 1;
  {$EXTERNALSYM METransformHaveOutput}
  METransformDrainComplete                = METransformHaveOutput + 1;
  {$EXTERNALSYM METransformDrainComplete}
  METransformMarker                       = METransformDrainComplete + 1;
  {$EXTERNALSYM METransformMarker}
  METransformInputStreamStateChanged      = METransformMarker + 1;
  {$EXTERNALSYM METransformInputStreamStateChanged}
  MEByteStreamCharacteristicsChanged      = MediaEventType(700);
  {$EXTERNALSYM MEByteStreamCharacteristicsChanged}
  MEVideoCaptureDeviceRemoved             = MediaEventType(800);
  {$EXTERNALSYM MEVideoCaptureDeviceRemoved}
  MEVideoCaptureDevicePreempted           = MediaEventType(801);
  {$EXTERNALSYM MEVideoCaptureDevicePreempted}
  MEStreamSinkFormatInvalidated           = MediaEventType(802);
  {$EXTERNALSYM MEStreamSinkFormatInvalidated}
  MEEncodingParameters                    = MediaEventType(803);
  {$EXTERNALSYM MEEncodingParameters}
  MEContentProtectionMetadata             = MediaEventType(900);
  {$EXTERNALSYM MEContentProtectionMetadata}
  MEDeviceThermalStateChanged             = MediaEventType(950);
  {$EXTERNALSYM MEDeviceThermalStateChanged}

  //-------------------------------------------------------------------------
  // MF reserves a range of events for internal and future use
  //-------------------------------------------------------------------------

  // <member name="MEReservedMax">
  //     All event type codes up to and including this value are
  //     reserved.
  // </member>
  MEReservedMax                           = MediaEventType(10000);
  {$EXTERNALSYM MEReservedMax}



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

  MFBYTESTREAM_SHARE_WRITE                 = $00000400;    // (WINVER >= _WIN32_WINNT_WIN7)
  {$EXTERNALSYM MFBYTESTREAM_SHARE_WRITE}

  MFBYTESTREAM_DOES_NOT_USE_NETWORK        = $00000800;    // (WINVER >= _WIN32_WINNT_WIN8)
  {$EXTERNALSYM MFBYTESTREAM_DOES_NOT_USE_NETWORK}

  MFBYTESTREAM_SEEK_FLAG_CANCEL_PENDING_IO = $00000001;
  {$EXTERNALSYM MFBYTESTREAM_SEEK_FLAG_CANCEL_PENDING_IO}

  // Interface IMFByteStream

  //
  // Byte Stream attributes (query the Byte Stream for IMFAttributes)
  //

  MF_BYTESTREAM_ORIGIN_NAME                        : TGUID = '{fc358288-3cb6-460c-a424-b6681260375a}';
  {$EXTERNALSYM MF_BYTESTREAM_ORIGIN_NAME}
  MF_BYTESTREAM_CONTENT_TYPE                       : TGUID = '{fc358289-3cb6-460c-a424-b6681260375a}';
  {$EXTERNALSYM MF_BYTESTREAM_CONTENT_TYPE}
  MF_BYTESTREAM_DURATION                           : TGUID = '{fc35828a-3cb6-460c-a424-b6681260375a}';
  {$EXTERNALSYM MF_BYTESTREAM_DURATION}
  MF_BYTESTREAM_LAST_MODIFIED_TIME                 : TGUID = '{fc35828b-3cb6-460c-a424-b6681260375a}';
  {$EXTERNALSYM MF_BYTESTREAM_LAST_MODIFIED_TIME}

  // >= Windows 7
  //#if (WINVER >= _WIN32_WINNT_WIN7)
  MF_BYTESTREAM_IFO_FILE_URI                       : TGUID = '{fc35828c-3cb6-460c-a424-b6681260375a}';
  {$EXTERNALSYM MF_BYTESTREAM_IFO_FILE_URI}
  MF_BYTESTREAM_DLNA_PROFILE_ID                    : TGUID = '{fc35828d-3cb6-460c-a424-b6681260375a}';
  {$EXTERNALSYM MF_BYTESTREAM_DLNA_PROFILE_ID}
  MF_BYTESTREAM_EFFECTIVE_URL                      : TGUID = '{9AFA0209-89D1-42AF-8456-1DE6B562D691}';
  {$EXTERNALSYM MF_BYTESTREAM_EFFECTIVE_URL}
  MF_BYTESTREAM_TRANSCODED                         : TGUID = '{b6c5c282-4dc9-4db9-ab48-cf3b6d8bc5e0}';
  {$EXTERNALSYM MF_BYTESTREAM_TRANSCODED}
  //#endif // (WINVER >= _WIN32_WINNT_WIN7)
  //end >= Windows 7


  CLSID_MFByteStreamProxyClassFactory   : TGUID = '{770e8e77-4916-441c-a9a7-b342d0eebc71}';



  // Interface IMFByteStream
type
  QWORD = ULONGLONG;
  {$EXTERNALSYM QWORD}

  // PRGBQUAD = ^RGBQUAD;
  // RGBQUAD = DWORD;
  // RGBQUAD is defined ins Winapi.Windows (Wingdi.h) and WinApi.WinApiTypes.pas
  // To store a RGBQUAD in a DWORD, use procedure CopyRGBQuadToClrRef in WinApi.MediaFoundationApi.MfUtils.pas

  PMF_ATTRIBUTE_TYPE = ^_MF_ATTRIBUTE_TYPE;
  _MF_ATTRIBUTE_TYPE = (
    MF_ATTRIBUTE_UINT32   = VT_UI4,
    MF_ATTRIBUTE_UINT64   = VT_UI8,
    MF_ATTRIBUTE_DOUBLE   = VT_R8,
    MF_ATTRIBUTE_GUID     = VT_CLSID,
    MF_ATTRIBUTE_STRING   = VT_LPWSTR,
    MF_ATTRIBUTE_BLOB     = (VT_VECTOR or VT_UI1),
    MF_ATTRIBUTE_IUNKNOWN = VT_UNKNOWN
  );
  {$EXTERNALSYM _MF_ATTRIBUTE_TYPE}
  MF_ATTRIBUTE_TYPE = _MF_ATTRIBUTE_TYPE;
  {$EXTERNALSYM MF_ATTRIBUTE_TYPE}


  PMF_ATTRIBUTES_MATCH_TYPE = ^_MF_ATTRIBUTES_MATCH_TYPE;
  _MF_ATTRIBUTES_MATCH_TYPE = (
    MF_ATTRIBUTES_MATCH_OUR_ITEMS    = 0,    // do all of our items exist in their store and have identical data?
    MF_ATTRIBUTES_MATCH_THEIR_ITEMS  = 1,    // do all of their items exist in our store and have identical data?
    MF_ATTRIBUTES_MATCH_ALL_ITEMS    = 2,    // do both stores have the same set of identical items?
    MF_ATTRIBUTES_MATCH_INTERSECTION = 3,    // do the attributes that intersect match?
    MF_ATTRIBUTES_MATCH_SMALLER      = 4     // do all the attributes in the type that has fewer attributes match?
    );
  {$EXTERNALSYM _MF_ATTRIBUTES_MATCH_TYPE}
  MF_ATTRIBUTES_MATCH_TYPE = _MF_ATTRIBUTES_MATCH_TYPE;
  {$EXTERNALSYM MF_ATTRIBUTES_MATCH_TYPE}

type
  PMF_ATTRIBUTE_SERIALIZE_OPTIONS = ^cwMF_ATTRIBUTE_SERIALIZE_OPTIONS;
  cwMF_ATTRIBUTE_SERIALIZE_OPTIONS = DWord;
  {$EXTERNALSYM cwMF_ATTRIBUTE_SERIALIZE_OPTIONS}
  MF_ATTRIBUTE_SERIALIZE_OPTIONS = cwMF_ATTRIBUTE_SERIALIZE_OPTIONS;
  {$EXTERNALSYM MF_ATTRIBUTE_SERIALIZE_OPTIONS}
const
    MF_ATTRIBUTE_SERIALIZE_UNKNOWN_BYREF = MF_ATTRIBUTE_SERIALIZE_OPTIONS($1);

type
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

//
// New version of MFT_REGISTER_TYPE_INFO for MF
//
  PMFT_REGISTER_TYPE_INFO = ^MFT_REGISTER_TYPE_INFO;
  cwMFT_REGISTER_TYPE_INFO = record
    guidMajorType: TGUID;
    guidSubtype: TGUID;
  end;
  {$EXTERNALSYM cwMFT_REGISTER_TYPE_INFO}
  MFT_REGISTER_TYPE_INFO = cwMFT_REGISTER_TYPE_INFO;
  {$EXTERNALSYM MFT_REGISTER_TYPE_INFO}


type
  PMFVideoInterlaceMode = ^MFVideoInterlaceMode;
  _MFVideoInterlaceMode = UINT32;
  {$EXTERNALSYM _MFVideoInterlaceMode}
  MFVideoInterlaceMode = _MFVideoInterlaceMode;
  {$EXTERNALSYM MFVideoInterlaceMode}
const
  MFVideoInterlace_Unknown                     = MFVideoInterlaceMode(0);
  {$EXTERNALSYM MFVideoInterlace_Unknown}
  MFVideoInterlace_Progressive                 = MFVideoInterlaceMode(2);
  {$EXTERNALSYM MFVideoInterlace_Progressive}
  MFVideoInterlace_FieldInterleavedUpperFirst  = MFVideoInterlaceMode(3);
  {$EXTERNALSYM MFVideoInterlace_FieldInterleavedUpperFirst}
  MFVideoInterlace_FieldInterleavedLowerFirst  = MFVideoInterlaceMode(4);
  {$EXTERNALSYM MFVideoInterlace_FieldInterleavedLowerFirst}
  MFVideoInterlace_FieldSingleUpper            = MFVideoInterlaceMode(5);
  {$EXTERNALSYM MFVideoInterlace_FieldSingleUpper}
  MFVideoInterlace_FieldSingleLower            = MFVideoInterlaceMode(6);
  {$EXTERNALSYM MFVideoInterlace_FieldSingleLower}
  MFVideoInterlace_MixedInterlaceOrProgressive = MFVideoInterlaceMode(7);
  {$EXTERNALSYM MFVideoInterlace_MixedInterlaceOrProgressive}
  MFVideoInterlace_Last                        = MFVideoInterlace_MixedInterlaceOrProgressive + 1;
  {$EXTERNALSYM MFVideoInterlace_Last}
  //MFVideoInterlace_ForceDWORD                  = FORCEDWORD);

const
  MFVideoInterlace_FieldSingleUpperFirst = MFVideoInterlace_FieldSingleUpper;
  {$EXTERNALSYM MFVideoInterlace_FieldSingleUpperFirst}
  MFVideoInterlace_FieldSingleLowerFirst = MFVideoInterlace_FieldSingleLower;
  {$EXTERNALSYM MFVideoInterlace_FieldSingleLowerFirst}


type
  PMFVideoTransferFunction = ^MFVideoTransferFunction;
  _MFVideoTransferFunction = UINT32;
  {$EXTERNALSYM _MFVideoTransferFunction}
  MFVideoTransferFunction = _MFVideoTransferFunction;
  {$EXTERNALSYM MFVideoTransferFunction}
const
  MFVideoTransFunc_Unknown     = MFVideoTransferFunction(0);
  {$EXTERNALSYM MFVideoTransFunc_Unknown}
  MFVideoTransFunc_10          = MFVideoTransferFunction(1);
  {$EXTERNALSYM MFVideoTransFunc_10}
  MFVideoTransFunc_18          = MFVideoTransferFunction(2);
  {$EXTERNALSYM MFVideoTransFunc_18}
  MFVideoTransFunc_20          = MFVideoTransferFunction(3);
  {$EXTERNALSYM MFVideoTransFunc_20}
  MFVideoTransFunc_22          = MFVideoTransferFunction(4);
  {$EXTERNALSYM MFVideoTransFunc_22}
  MFVideoTransFunc_709         = MFVideoTransferFunction(5);
  {$EXTERNALSYM MFVideoTransFunc_709}
  MFVideoTransFunc_240M        = MFVideoTransferFunction(6);
  {$EXTERNALSYM MFVideoTransFunc_240M}
  MFVideoTransFunc_sRGB        = MFVideoTransferFunction(7);
  {$EXTERNALSYM MFVideoTransFunc_sRGB}
  MFVideoTransFunc_28          = MFVideoTransferFunction(8);
  {$EXTERNALSYM MFVideoTransFunc_28}
  MFVideoTransFunc_Log_100     = MFVideoTransferFunction(9);
  {$EXTERNALSYM MFVideoTransFunc_Log_100}
  MFVideoTransFunc_Log_316     = MFVideoTransferFunction(10);
  {$EXTERNALSYM MFVideoTransFunc_Log_316}
  MFVideoTransFunc_709_sym     = MFVideoTransferFunction(11);
  {$EXTERNALSYM MFVideoTransFunc_709_sym}
  MFVideoTransFunc_2020_const  = MFVideoTransferFunction(12);
  {$EXTERNALSYM MFVideoTransFunc_2020_const}
  MFVideoTransFunc_2020        = MFVideoTransferFunction(13);
  {$EXTERNALSYM MFVideoTransFunc_2020}
  MFVideoTransFunc_26          = MFVideoTransferFunction(14);
  {$EXTERNALSYM MFVideoTransFunc_26}
  MFVideoTransFunc_2084        = MFVideoTransferFunction(15); // SMPTE ST.2084
  {$EXTERNALSYM MFVideoTransFunc_2084}
  MFVideoTransFunc_HLG         = MFVideoTransferFunction(16); // Hybrid Log-Gamma, ARIB STD-B67
  {$EXTERNALSYM MFVideoTransFunc_HLG}
// >= NTDDI_WIN10_RS4)
  MFVideoTransFunc_10_rel      = MFVideoTransferFunction(17); // No gamma, display referred (relative)
  {$EXTERNALSYM MFVideoTransFunc_10_rel}
// end
  FVideoTransFunc_Last         = MFVideoTransFunc_HLG + 1;
  {$EXTERNALSYM FVideoTransFunc_Last}
  // MFVideoTransFunc_ForceMFVideoTransferFunction = FORCEMFVideoTransferFunction;

type
  PMFVideoPrimaries = ^MFVideoPrimaries;
  _MFVideoPrimaries = UINT32;
  {$EXTERNALSYM _MFVideoPrimaries}
  MFVideoPrimaries = _MFVideoPrimaries;
  {$EXTERNALSYM MFVideoPrimaries}
const
  MFVideoPrimaries_Unknown       = MFVideoPrimaries(0);
  {$EXTERNALSYM MFVideoPrimaries_Unknown}
  MFVideoPrimaries_reserved      = MFVideoPrimaries(1);
  {$EXTERNALSYM MFVideoPrimaries_reserved}
  MFVideoPrimaries_BT709         = MFVideoPrimaries(2);
  {$EXTERNALSYM MFVideoPrimaries_BT709}
  MFVideoPrimaries_BT470_2_SysM  = MFVideoPrimaries(3);
  {$EXTERNALSYM MFVideoPrimaries_BT470_2_SysM}
  MFVideoPrimaries_BT470_2_SysBG = MFVideoPrimaries(4);
  {$EXTERNALSYM MFVideoPrimaries_BT470_2_SysBG}
  MFVideoPrimaries_SMPTE170M     = MFVideoPrimaries(5);
  {$EXTERNALSYM MFVideoPrimaries_SMPTE170M}
  MFVideoPrimaries_SMPTE240M     = MFVideoPrimaries(6);
  {$EXTERNALSYM MFVideoPrimaries_SMPTE240M}
  MFVideoPrimaries_EBU3213       = MFVideoPrimaries(7);
  {$EXTERNALSYM MFVideoPrimaries_EBU3213}
  MFVideoPrimaries_SMPTE_C       = MFVideoPrimaries(8);
  {$EXTERNALSYM MFVideoPrimaries_SMPTE_C}
  MFVideoPrimaries_BT2020        = MFVideoPrimaries(9);
  {$EXTERNALSYM MFVideoPrimaries_BT2020}
  MFVideoPrimaries_XYZ           = MFVideoPrimaries(10);
  {$EXTERNALSYM MFVideoPrimaries_XYZ}
  MFVideoPrimaries_DCI_P3        = MFVideoPrimaries(11);
  {$EXTERNALSYM MFVideoPrimaries_DCI_P3}
  MFVideoPrimaries_ACES          = MFVideoPrimaries(12);
  {$EXTERNALSYM MFVideoPrimaries_ACES}
  MFVideoPrimaries_Last          = MFVideoPrimaries_ACES + 1;
  {$EXTERNALSYM MFVideoPrimaries_Last}
  //MFVideoPrimaries_ForceMFVideoPrimaries    = FORCEMFVideoPrimaries);

type
  PMFVideoLighting = ^MFVideoLighting;
  _MFVideoLighting = UINT32;
  {$EXTERNALSYM _MFVideoLighting}
  MFVideoLighting = _MFVideoLighting;
  {$EXTERNALSYM MFVideoLighting}
const
  MFVideoLighting_Unknown    = MFVideoLighting(0);
  {$EXTERNALSYM MFVideoLighting_Unknown}
  MFVideoLighting_bright     = MFVideoLighting(1);
  {$EXTERNALSYM MFVideoLighting_bright}
  MFVideoLighting_office     = MFVideoLighting(2);
  {$EXTERNALSYM MFVideoLighting_office}
  MFVideoLighting_dim        = MFVideoLighting(3);
  {$EXTERNALSYM MFVideoLighting_dim}
  MFVideoLighting_dark       = MFVideoLighting(4);
  {$EXTERNALSYM MFVideoLighting_dark}
  MFVideoLighting_Last       = MFVideoLighting_dark + 1;
  {$EXTERNALSYM MFVideoLighting_Last}
  //MFVideoLighting_ForceDWORD = FORCEDWORD);

type
  PMFVideoTransferMatrix = ^MFVideoTransferMatrix;
  _MFVideoTransferMatrix = UINT32;
  {$EXTERNALSYM _MFVideoTransferMatrix}
  MFVideoTransferMatrix = _MFVideoTransferMatrix;
  {$EXTERNALSYM MFVideoTransferMatrix}
const
  MFVideoTransferMatrix_Unknown    = MFVideoTransferMatrix(0);
  {$EXTERNALSYM MFVideoTransferMatrix_Unknown}
  MFVideoTransferMatrix_BT709      = MFVideoTransferMatrix(1);
  {$EXTERNALSYM MFVideoTransferMatrix_BT709}
  MFVideoTransferMatrix_BT601      = MFVideoTransferMatrix(2);
  {$EXTERNALSYM MFVideoTransferMatrix_BT601}
  MFVideoTransferMatrix_SMPTE240M  = MFVideoTransferMatrix(3);
  {$EXTERNALSYM MFVideoTransferMatrix_SMPTE240M}
  MFVideoTransferMatrix_BT2020_10  = MFVideoTransferMatrix(4);
  {$EXTERNALSYM MFVideoTransferMatrix_BT2020_10}
  MFVideoTransferMatrix_BT2020_12  = MFVideoTransferMatrix(5);
  {$EXTERNALSYM MFVideoTransferMatrix_BT2020_12}
  MFVideoTransferMatrix_Last       = MFVideoTransferMatrix_BT2020_12 + 1;
  {$EXTERNALSYM MFVideoTransferMatrix_Last}
  //MFVideoTransferMatrix_ForceDWORD = FORCEDWORD);

type
  PMFVideoChromaSubsampling = ^MFVideoChromaSubsampling;
  _MFVideoChromaSubsampling = UINT32;
  {$EXTERNALSYM _MFVideoChromaSubsampling}
  MFVideoChromaSubsampling = _MFVideoChromaSubsampling;
  {$EXTERNALSYM MFVideoChromaSubsampling}
const
  MFVideoChromaSubsampling_Unknown                        = MFVideoChromaSubsampling(0);
  {$EXTERNALSYM MFVideoChromaSubsampling_Unknown}
  MFVideoChromaSubsampling_ProgressiveChroma              = MFVideoChromaSubsampling($8);
  {$EXTERNALSYM MFVideoChromaSubsampling_ProgressiveChroma}
  MFVideoChromaSubsampling_Horizontally_Cosited           = MFVideoChromaSubsampling($4);
  {$EXTERNALSYM MFVideoChromaSubsampling_Horizontally_Cosited}
  MFVideoChromaSubsampling_Vertically_Cosited             = MFVideoChromaSubsampling($2);
  {$EXTERNALSYM MFVideoChromaSubsampling_Vertically_Cosited}
  MFVideoChromaSubsampling_Vertically_AlignedChromaPlanes = MFVideoChromaSubsampling($1);
  {$EXTERNALSYM MFVideoChromaSubsampling_Vertically_AlignedChromaPlanes}
  {$EXTERNALSYM MFVideoChromaSubsampling_MPEG2}
  MFVideoChromaSubsampling_MPEG2                          = MFVideoChromaSubsampling_Horizontally_Cosited or
                                                            MFVideoChromaSubsampling_Vertically_AlignedChromaPlanes;
  {$EXTERNALSYM MFVideoChromaSubsampling_MPEG2}
  MFVideoChromaSubsampling_MPEG1                          = MFVideoChromaSubsampling_Vertically_AlignedChromaPlanes;
  {$EXTERNALSYM MFVideoChromaSubsampling_MPEG1}
  MFVideoChromaSubsampling_DV_PAL                         = MFVideoChromaSubsampling_Horizontally_Cosited or
                                                            MFVideoChromaSubsampling_Vertically_Cosited;
  {$EXTERNALSYM MFVideoChromaSubsampling_DV_PAL}
  MFVideoChromaSubsampling_Cosited                        = MFVideoChromaSubsampling_Horizontally_Cosited or
                                                            MFVideoChromaSubsampling_Vertically_Cosited or
                                                            MFVideoChromaSubsampling_Vertically_AlignedChromaPlanes;
  {$EXTERNALSYM MFVideoChromaSubsampling_Cosited}
  MFVideoChromaSubsampling_Last                           = MFVideoChromaSubsampling_Cosited + 1;
  {$EXTERNALSYM MFVideoChromaSubsampling_Last}
  //MFVideoChromaSubsampling_ForceDWORD                     = FORCEDWORD);

type
  PMFNominalRange = ^MFNominalRange;
  _MFNominalRange = UINT32;
  {$EXTERNALSYM _MFNominalRange}
  MFNominalRange = _MFNominalRange;
  {$EXTERNALSYM MFNominalRange}
const
  MFNominalRange_Unknown    = MFNominalRange(0);
  {$EXTERNALSYM MFNominalRange_Unknown}
  MFNominalRange_Normal     = MFNominalRange(1);
  {$EXTERNALSYM MFNominalRange_Normal}
  MFNominalRange_Wide       = MFNominalRange(2);
  {$EXTERNALSYM MFNominalRange_Wide}
  MFNominalRange_0_255      = MFNominalRange(1);
  {$EXTERNALSYM MFNominalRange_0_255}
  MFNominalRange_16_235     = MFNominalRange(2);
  {$EXTERNALSYM MFNominalRange_16_235}
  MFNominalRange_48_208     = MFNominalRange(3);
  {$EXTERNALSYM MFNominalRange_48_208}
  MFNominalRange_64_127     = MFNominalRange(4);
  {$EXTERNALSYM MFNominalRange_64_127}
  MFNominalRange_Last       = MFNominalRange_64_127 + 1;
  {$EXTERNALSYM MFNominalRange_Last}
  // MFNominalRange_ForceMFNominalRange = FORCEMFNominalRange);

type
  PMFVideoFlags = ^MFVideoFlags;
  _MFVideoFlags = QWORD;
  {$EXTERNALSYM _MFVideoFlags}
  MFVideoFlags = _MFVideoFlags;
  {$EXTERNALSYM MFVideoFlags}
const
  // static flags
  MFVideoFlag_PAD_TO_Mask           = MFVideoFlags($1 or $2);
  {$EXTERNALSYM MFVideoFlag_PAD_TO_Mask}
  MFVideoFlag_PAD_TO_None           = MFVideoFlags(0 * $1);
  {$EXTERNALSYM MFVideoFlag_PAD_TO_None}
  MFVideoFlag_PAD_TO_4x3            = MFVideoFlags(1 * $1);
  {$EXTERNALSYM MFVideoFlag_PAD_TO_4x3}
  MFVideoFlag_PAD_TO_16x9           = MFVideoFlags(2 * $1);
  {$EXTERNALSYM MFVideoFlag_PAD_TO_16x9}

  MFVideoFlag_SrcContentHintMask    = MFVideoFlags(($4 or $8) or $10);
  {$EXTERNALSYM MFVideoFlag_SrcContentHintMask}
  MFVideoFlag_SrcContentHintNone    = MFVideoFlags(0 * $4);
  {$EXTERNALSYM MFVideoFlag_SrcContentHintNone}
  MFVideoFlag_SrcContentHint16x9    = MFVideoFlags(1 * $4);
  {$EXTERNALSYM MFVideoFlag_SrcContentHint16x9}
  MFVideoFlag_SrcContentHint235_1   = MFVideoFlags(2 * $4);
  {$EXTERNALSYM MFVideoFlag_SrcContentHint235_1}

  // static/dynamic flags
  MFVideoFlag_AnalogProtected       = MFVideoFlags($20);
  {$EXTERNALSYM MFVideoFlag_AnalogProtected}
  MFVideoFlag_DigitallyProtected    = MFVideoFlags($40);
  {$EXTERNALSYM MFVideoFlag_DigitallyProtected}

  // dynamic flags
  MFVideoFlag_ProgressiveContent    = MFVideoFlags($80);
  {$EXTERNALSYM MFVideoFlag_ProgressiveContent}
  MFVideoFlag_FieldRepeatCountMask  = MFVideoFlags(($100 or $200) or $400);
  {$EXTERNALSYM MFVideoFlag_FieldRepeatCountMask}
  MFVideoFlag_FieldRepeatCountShift = MFVideoFlags(8);
  {$EXTERNALSYM MFVideoFlag_FieldRepeatCountShift}
  MFVideoFlag_ProgressiveSeqReset   = MFVideoFlags($800);
  {$EXTERNALSYM MFVideoFlag_ProgressiveSeqReset}
  MFVideoFlag_PanScanEnabled        = MFVideoFlags($20000);
  {$EXTERNALSYM MFVideoFlag_PanScanEnabled}
  MFVideoFlag_LowerFieldFirst       = MFVideoFlags($40000);
  {$EXTERNALSYM MFVideoFlag_LowerFieldFirst}
  MFVideoFlag_BottomUpLinearRep     = MFVideoFlags($80000);
  {$EXTERNALSYM MFVideoFlag_BottomUpLinearRep}

  // ******************************************
  // static surface creation flags - UNAPPROVED
  // --------------- DO NOT USE ---------------
  // ******************************************
  MFVideoFlags_DXVASurface          = MFVideoFlags($100000);
  {$EXTERNALSYM MFVideoFlags_DXVASurface}
  MFVideoFlags_RenderTargetSurface  = MFVideoFlags($400000);
  {$EXTERNALSYM MFVideoFlags_RenderTargetSurface}
  //  MFVideoFlags_ForceQWORD           = $7FFFFFFF);

type
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
     Area: SIZE;        // A SIZE structure that contains the width and height of the rectangle.
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
    Palette: array [0..0] of MFPaletteEntry;
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
    MFStdVideoFormat_reserved     = 0,
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

  //
  // File access modes for the file creation functions (MFCreateFile and related
  // functions in mfapi.h).
  // Regardless of the access mode with which the file is opened, the sharing
  // permissions will allow shared reading and deleting.
  //

  PMF_FILE_ACCESSMODE = ^MF_FILE_ACCESSMODE;
  __MIDL___MIDL_itf_mfobjects_0000_0017_0001 = (
    MF_ACCESSMODE_READ      = 1,
    MF_ACCESSMODE_WRITE     = 2,
    MF_ACCESSMODE_READWRITE = 3);
  {$EXTERNALSYM __MIDL___MIDL_itf_mfobjects_0000_0017_0001}
  MF_FILE_ACCESSMODE = __MIDL___MIDL_itf_mfobjects_0000_0017_0001;
  {$EXTERNALSYM MF_FILE_ACCESSMODE}


  PMF_FILE_OPENMODE = ^MF_FILE_OPENMODE;
  __MIDL___MIDL_itf_mfobjects_0000_0017_0002 = (
    MF_OPENMODE_FAIL_IF_NOT_EXIST   = 0,
    MF_OPENMODE_FAIL_IF_EXIST       = 1,
    MF_OPENMODE_RESET_IF_EXIST      = 2,
    MF_OPENMODE_APPEND_IF_EXIST     = 3,
    MF_OPENMODE_DELETE_IF_EXIST     = 4);
  {$EXTERNALSYM __MIDL___MIDL_itf_mfobjects_0000_0017_0002}
  MF_FILE_OPENMODE = __MIDL___MIDL_itf_mfobjects_0000_0017_0002;
  {$EXTERNALSYM MF_FILE_OPENMODE}

type
  PMF_FILE_FLAGS = ^MF_FILE_FLAGS;
  __MIDL___MIDL_itf_mfobjects_0000_0017_0003 = DWord;
  {$EXTERNALSYM __MIDL___MIDL_itf_mfobjects_0000_0017_0003}
  MF_FILE_FLAGS = __MIDL___MIDL_itf_mfobjects_0000_0017_0003 ;
  {$EXTERNALSYM MF_FILE_FLAGS}
const
  MF_FILEFLAGS_NONE                 = MF_FILE_FLAGS(0);
  {$EXTERNALSYM MF_FILEFLAGS_NONE}
  MF_FILEFLAGS_NOBUFFERING          = MF_FILE_FLAGS($1);
  {$EXTERNALSYM MF_FILEFLAGS_NOBUFFERING}
  MF_FILEFLAGS_ALLOW_WRITE_SHARING  = MF_FILE_FLAGS($2);
  {$EXTERNALSYM MF_FILEFLAGS_ALLOW_WRITE_SHARING}


// >= Windows 7

type
  //
  //  List IDs for IMFPluginControl
  //
  PMF_Plugin_Type = ^MF_Plugin_Type;
  _MF_Plugin_Type            = (
    MF_Plugin_Type_MFT                 = 0,
    MF_Plugin_Type_MediaSource         = 1,
    MF_Plugin_Type_MFT_MatchOutputType = 2,
    MF_Plugin_Type_Other               = DWORD(-1)
  );
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
                     var pValue: PROPVARIANT): HResult; stdcall;

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
                              out ppBuf: PUINT8;
                              pcbSize: UINT32): HResult; stdcall;

    function GetUnknown(const guidKey: TGUID;
                        const riid: REFIID;
                        out ppv): HResult; stdcall;

    function SetItem(const guidKey: TGUID;
                     const Value: PROPVARIANT): HResult; stdcall;

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
                            var pValue: PROPVARIANT): HResult; stdcall;

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
  // <summary>
  //     This interface is used to represent the result from an asynchronous
  //     operation.
  //     For most Media Foundation components and applications that need to
  //     create an IMFAsyncResult implementation, MFCreateAsyncResult, which
  //     instantiates the MF implementation of this interface, will suffice.
  //     Any implementation of IMFAsyncResult must inherit from the
  //     MFASYNCRESULT structure defined in mfapi.h
  // </summary>
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

    function GetValue(out pvValue: PROPVARIANT): HResult; stdcall;

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
                        pvValue: PROPVARIANT): HResult; stdcall;
  end;
  IID_IMFMediaEventGenerator = IMFMediaEventGenerator;
  {$EXTERNALSYM IID_IMFMediaEventGenerator}


  // Interface IMFRemoteAsyncCallback
  // ================================
  // <summary>
  //     Remote async callback function for use with
  //     IMFMediaEventGenerator.RemoteBeginGetEvent.
  //     Media Foundation applications need not implement this interface.
  // </summary>
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

    function GetElement(dwElementIndex: DWord;
                        out ppUnkElement: PIUnknown): HResult; stdcall;

    function AddElement(pUnkElement: IUnknown): HResult; stdcall;

    function RemoveElement(dwElementIndex: DWord;
                           out ppUnkElement: PIUnknown): HResult; stdcall;

    function InsertElementAt(dwIndex: DWord;
                             pUnknown_: IUnknown): HResult; stdcall;

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
    // <summary>
    //     Media Event Generator components that use the Media Event Queue
    //     object should just forward IMFMediaEventGenerator::GetEvent
    //     calls to this method on the Media Event Queue.
    //     See IMFMediaEventGenerator::GetEvent parameter descriptions.
    // </summary>
    function GetEvent(dwFlags: DWord;
                      out ppEvent: IMFMediaEvent): HResult; stdcall;

    // <summary>
    //     Media Event Generator components that use the Media Event Queue
    //     object should just forward IMFMediaEventGenerator::BeginGetEvent
    //     calls to this method on the Media Event Queue.
    //     See IMFMediaEventGenerator::BeginGetEvent parameter descriptions.
    // </summary>
    function BeginGetEvent(pCallback: IMFAsyncCallback;
                           punkState: IUnknown): HResult; stdcall;

    // <summary>
    //     Media Event Generator components that use the Media Event Queue
    //     object should just forward IMFMediaEventGenerator::EndGetEvent
    //     calls to this method on the Media Event Queue.
    //     See IMFMediaEventGenerator::EndGetEvent parameter descriptions.
    // </summary>
    function EndGetEvent(pResult: IMFAsyncResult;
                         out ppEvent: IMFMediaEvent): HResult; stdcall;

    // <summary>
    //     Queues the specified event on the Media Event Queue object.
    //     This event will be retrievable using Begin/EndGetEvent or
    //     GetEvent
    // </summary>
    // <param name="pEvent">
    //     Pointer to the Media Event object to queue
    // </param>
    function QueueEvent(pEvent: IMFMediaEvent): HResult; stdcall;

    // <summary>
    //     Queues an event with the specified event data on the Media Event
    //     Queue object.
    //     This event will be retrievable using Begin/EndGetEvent or
    //     GetEvent
    // </summary>
    // <param name="met">
    //     Event type
    // </param>
    // <param name="guidExtendedType">
    //     Extended event type
    // </param>
    // <param name="hrStatus">
    //     Event status
    // </param>
    // <param name="pvValue">
    //     Event value
    // </param>
    function QueueEventParamVar(met: MediaEventType;
                                const guidExtendedType: REFGUID;
                                hrStatus: HRESULT;
                                pvValue: PROPVARIANT): HResult; stdcall;

    // <summary>
    //     Queues an event with the specified event data on the Media Event
    //     Queue object.
    //     This event will be retrievable using Begin/EndGetEvent or
    //     GetEvent
    // </summary>
    // <param name="met">
    //     Event type
    // </param>
    // <param name="guidExtendedType">
    //     Extended event type
    // </param>
    // <param name="hrStatus">
    //     Event status
    // </param>
    // <param name="pUnk">
    //     Event value, specified as an IUnknown *
    // </param>
    function QueueEventParamUnk(met: MediaEventType;
                                const guidExtendedType: REFGUID;
                                hrStatus: HRESULT;
                                pUnk: IUnknown): HResult; stdcall;

    // <summary>
    //     Shutdown must be called when the component is done using the
    //     Media Event Queue object to break circular references and
    //     prevent memory leaks.
    // </summary>
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

    // <summary>
    //     Creates the object that this activate represents.
    //     The ActivateObject(...) method should always return the same instance of the object
    //     until either ShutdownObject() or DetachObject() is called.
    // </summary>
    // <param name="riid">
    //     The interface ID that the requested object will be QI'ed for
    // </param>
    // <param name="ppv">
    //     Will contain the requested interface
    // </param>
    function ActivateObject(const riid: REFIID;
                            out ppv): HResult; stdcall;

    // <summary>
    //     Shuts down the internal represented object
    //     (that is returned on ActivateObject(...))
    //     and then releases all references to it.
    // </summary>
    function ShutdownObject(): HResult; stdcall;

    // <summary>
    //     Releases all references to the internal represented
    //     object (without shutting it down.)
    //     If this action is not supported, E_NOTIMPL should be returned.
    // </summary>
    function DetachObject(): HResult; stdcall;

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
                        const clsid: REFCLSID): HResult; stdcall;

    function GetDisabledByIndex(pluginType: DWord;
                                const index: DWord;
                                out clsid: CLSID): HResult; stdcall;

    function SetDisabled(pluginType: DWord;
                         const clsid: REFCLSID;
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
    MF_PLUGIN_CONTROL_POLICY_USE_WEB_PLUGINS_EDGEMODE = 3
  );
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
    MF_STREAM_STATE_PAUSED  = 1,
    MF_STREAM_STATE_RUNNING = 2
  );
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
                       out ppSample: PIMFSample): HResult; stdcall;

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


  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFByteStreamProxyClassFactory);'}
  {$EXTERNALSYM IMFByteStreamProxyClassFactory}
  IMFByteStreamProxyClassFactory = interface(IUnknown)
  ['{a6b43f84-5c0a-42e8-a44d-b1857a76992f}']
    function CreateByteStreamProxy(pByteStream: IMFByteStream;
                                   pAttributes: IMFAttributes;
                                   const riid: REFIID;
                                   {out} ppvObject: Pointer): HResult; stdcall;

  end;
  IID_IMFByteStreamProxyClassFactory = IMFByteStreamProxyClassFactory;
  {$EXTERNALSYM IID_IMFByteStreamProxyClassFactory}


  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFSampleOutputStream);'}
  {$EXTERNALSYM IMFSampleOutputStream}
  IMFSampleOutputStream = interface(IUnknown)
  ['{8feed468-6f7e-440d-869a-49bdd283ad0d}']
    /// <summary>
    ///     Begin async write operation.
    /// </summary>
    function BeginWriteSample(pSample: IMFSample;
                              pCallback: IMFAsyncCallback;
                              punkState: IUnknown): HResult; stdcall;

    /// <summary>
    ///     Complete async write operation.
    /// </summary>
    function EndWriteSample(pResult: IMFAsyncResult): HResult; stdcall;

    function Close(): HResult; stdcall;

  end;
  IID_IMFSampleOutputStream = IMFSampleOutputStream;
  {$EXTERNALSYM IID_IMFSampleOutputStream}



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
  {$NODEFINE PIMFActivateArray}
  PIMFActivateArray = ^TIMFActivateArray;
  // dynamic
  // TIMFActivateArray = array of IMFActivate;
  // or static (preferred)
  {$NODEFINE TIMFActivateArray}
  TIMFActivateArray = array[0..65535] of IMFActivate;


  // Used by function MfPack.MfIdl.MFCreateStreamDescriptor
  {$NODEFINE PIMFMediaTypeArray}
  PIMFMediaTypeArray = ^TIMFMediaTypeArray;
  // dynamic
  // TIMFMediaTypeArray = array of IMFMediaType;
  // or static (preferred)
  {$NODEFINE TIMFMediaTypeArray}
  TIMFMediaTypeArray = array[0..65535] of IMFMediaType;


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
