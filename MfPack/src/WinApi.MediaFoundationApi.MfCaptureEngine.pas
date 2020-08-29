// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - MediaFoundation
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.MediaFoundationApi.MfCaptureEngine.pas
// Kind: Pascal / Delphi unit
// Release date: 09-10-2015
// Language: ENU
//
// Revision Version: 3.0.0
// Description: -
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX), Peter Larson (ozships), (TopPlay)
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
// Source: mfcaptureengine.h
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
unit WinApi.MediaFoundationApi.MfCaptureEngine;

  {$HPPEMIT '#include "mfcaptureengine.h"'}

interface

uses

  {WinApi}
  WinApi.Windows,
  WinApi.WinApiTypes,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfObjects,
  WinApi.MediaFoundationApi.MfIdl;


  {$WEAKPACKAGEUNIT ON}
  {$MINENUMSIZE 4}

  {$IFDEF WIN32}
    {$ALIGN 1}
  {$ELSE}
    {$ALIGN 8} // Win64
  {$ENDIF}

  {$I 'WinApiTypes.inc'}


type

  PMFVideoNormalizedRect = ^MFVideoNormalizedRect;
  MFVideoNormalizedRect = record
    left: Single;
    top: Single;
    right: Single;
    bottom: Single;
  end;
  {$EXTERNALSYM MFVideoNormalizedRect}


  //////////////////////////////////////////////////////////////////////////////
  //
  // MF_CAPTURE_ENGINE_DEVICE_TYPE Enumeration
  //
  /////////////////////////////////////////////////////////////////////////////

  // Defines a capture device type

  PMfCaptureEngineDeviceType = ^MF_CAPTURE_ENGINE_DEVICE_TYPE;
  MF_CAPTURE_ENGINE_DEVICE_TYPE         = (
    MF_CAPTURE_ENGINE_DEVICE_TYPE_AUDIO = $00000000,  // Audio device which is a microphone
    MF_CAPTURE_ENGINE_DEVICE_TYPE_VIDEO = $00000001   // Video device which is a webcam
  );
  {$EXTERNALSYM MF_CAPTURE_ENGINE_DEVICE_TYPE}


  //////////////////////////////////////////////////////////////////////////////
  //
  // MF_CAPTURE_ENGINE_SINK_TYPE Enumeration
  //
  /////////////////////////////////////////////////////////////////////////////

  // Defines the capture sink type
  PMfCaptureEngineSinkType = ^MF_CAPTURE_ENGINE_SINK_TYPE;
  MF_CAPTURE_ENGINE_SINK_TYPE           = (
    MF_CAPTURE_ENGINE_SINK_TYPE_RECORD  = 0,  // Record sink, used for outputting compressed data
    MF_CAPTURE_ENGINE_SINK_TYPE_PREVIEW = $1, // Record sink, used for outputting uncompressed data or rendering video
    MF_CAPTURE_ENGINE_SINK_TYPE_PHOTO   = $2  // Photo sink, used for retrieving a single photograph
  );
  {$EXTERNALSYM MF_CAPTURE_ENGINE_SINK_TYPE}


const
    //
    MF_CAPTURE_ENGINE_PREFERRED_SOURCE_STREAM_FOR_VIDEO_PREVIEW  = UINT($fffffffa); // The preferred stream for previewing video
    {$EXTERNALSYM MF_CAPTURE_ENGINE_PREFERRED_SOURCE_STREAM_FOR_VIDEO_PREVIEW}
    MF_CAPTURE_ENGINE_PREFERRED_SOURCE_STREAM_FOR_VIDEO_RECORD   = UINT($fffffff9); // The preferred stream for recording video
    {$EXTERNALSYM MF_CAPTURE_ENGINE_PREFERRED_SOURCE_STREAM_FOR_VIDEO_RECORD}
    MF_CAPTURE_ENGINE_PREFERRED_SOURCE_STREAM_FOR_PHOTO          = UINT($fffffff8); // The first independent photo stream if present, or else the same stream chosen by MF_CAPTURE_ENGINE_PREFERRED_SOURCE_STREAM_FOR_VIDEO_PREVIEW
    {$EXTERNALSYM MF_CAPTURE_ENGINE_PREFERRED_SOURCE_STREAM_FOR_PHOTO}
    MF_CAPTURE_ENGINE_PREFERRED_SOURCE_STREAM_FOR_AUDIO          = UINT($fffffff7); // The first available audio stream
    {$EXTERNALSYM MF_CAPTURE_ENGINE_PREFERRED_SOURCE_STREAM_FOR_AUDIO}
    MF_CAPTURE_ENGINE_MEDIASOURCE                                = UINT($ffffffff);
    {$EXTERNALSYM MF_CAPTURE_ENGINE_MEDIASOURCE}


type
  //////////////////////////////////////////////////////////////////////////////
  //
  // MF_CAPTURE_ENGINE_STREAM_CATEGORY Enumeration
  //
  /////////////////////////////////////////////////////////////////////////////

  // Defines the source stream category
  PMF_CAPTURE_ENGINE_STREAM_CATEGORY = ^MF_CAPTURE_ENGINE_STREAM_CATEGORY;
  MF_CAPTURE_ENGINE_STREAM_CATEGORY                     = (
    MF_CAPTURE_ENGINE_STREAM_CATEGORY_VIDEO_PREVIEW     = $00000000, // Video Preview stream
    MF_CAPTURE_ENGINE_STREAM_CATEGORY_VIDEO_CAPTURE     = $00000001, // Video Capture Stream
    MF_CAPTURE_ENGINE_STREAM_CATEGORY_PHOTO_INDEPENDENT = $00000002, // Independent photo stream
    MF_CAPTURE_ENGINE_STREAM_CATEGORY_PHOTO_DEPENDENT   = $00000003, // Dependent photo stream
    MF_CAPTURE_ENGINE_STREAM_CATEGORY_AUDIO             = $00000004, // Audio stream
    MF_CAPTURE_ENGINE_STREAM_CATEGORY_UNSUPPORTED       = $00000005  // Unsupported stream
  );
  {$EXTERNALSYM MF_CAPTURE_ENGINE_STREAM_CATEGORY}



  //////////////////////////////////////////////////////////////////////////////
  //
  // MF_CAPTURE_ENGINE_MEDIA_CATEGORY_TYPE Enumeration
  //
  /////////////////////////////////////////////////////////////////////////////

  // Defines the media capture categories.
  PMF_CAPTURE_ENGINE_MEDIA_CATEGORY_TYPE = ^MF_CAPTURE_ENGINE_MEDIA_CATEGORY_TYPE;
  MF_CAPTURE_ENGINE_MEDIA_CATEGORY_TYPE = (
    MF_CAPTURE_ENGINE_MEDIA_CATEGORY_TYPE_OTHER          = 0,  // Default capture category
    MF_CAPTURE_ENGINE_MEDIA_CATEGORY_TYPE_COMMUNICATIONS = 1,  // Communications capture category
    MF_CAPTURE_ENGINE_MEDIA_CATEGORY_TYPE_MEDIA          = 2,  // General media capture category
    MF_CAPTURE_ENGINE_MEDIA_CATEGORY_TYPE_GAMECHAT       = 3,  // Game Chat capture category
    MF_CAPTURE_ENGINE_MEDIA_CATEGORY_TYPE_SPEECH         = 4   // Speech capture category
  );
  {$EXTERNALSYM MF_CAPTURE_ENGINE_MEDIA_CATEGORY_TYPE}


  //////////////////////////////////////////////////////////////////////////////
  //
  // MF_CAPTURE_ENGINE_AUDIO_PROCESSING_MODE Enumeration
  //
  /////////////////////////////////////////////////////////////////////////////

  // Defines the capture audio processing mode.
  PMF_CAPTURE_ENGINE_AUDIO_PROCESSING_MODE = ^MF_CAPTURE_ENGINE_AUDIO_PROCESSING_MODE;
  MF_CAPTURE_ENGINE_AUDIO_PROCESSING_MODE = (
    MF_CAPTURE_ENGINE_AUDIO_PROCESSING_DEFAULT = 0, // Normal audio signal processing.
    MF_CAPTURE_ENGINE_AUDIO_PROCESSING_RAW = 1  // Minimal audio signal processing.
  );
  {$EXTERNALSYM MF_CAPTURE_ENGINE_AUDIO_PROCESSING_MODE}


const
  ///////////////////////////////////////////////////////////////////////////////
  //
  // GUIDs to go with IMFCaptureEngineOnEventCallback. These are extended types on IMFMediaEvent
  //
  ///////////////////////////////////////////////////////////////////////////////
  // 219992bc-cf92-4531-a1ae-96e1e886c8f1
  // MF_CAPTURE_ENGINE_INITIALIZED
  // Signals capture engine was initialized in response to IMFCaptureEngine.Initialize
  MF_CAPTURE_ENGINE_INITIALIZED       : TGUID = '{219992bc-cf92-4531-a1ae-96e1e886c8f1}';
  {$EXTERNALSYM MF_CAPTURE_ENGINE_INITIALIZED}


  // a416df21-f9d3-4a74-991b-b817298952c4
  // MF_CAPTURE_ENGINE_PREVIEW_STARTED
  // Signals capture engine preview started in response to IMFCaptureEngine.StartPreview
  MF_CAPTURE_ENGINE_PREVIEW_STARTED   : TGUID = '{a416df21-f9d3-4a74-991b-b817298952c4}';
  {$EXTERNALSYM MF_CAPTURE_ENGINE_PREVIEW_STARTED}

  // 13d5143c-1edd-4e50-a2ef-350a47678060
  // MF_CAPTURE_ENGINE_PREVIEW_STOPPED
  // Signals capture engine preview stopped in response to IMFCaptureEngine.StopPreview
  MF_CAPTURE_ENGINE_PREVIEW_STOPPED   : TGUID = '{13d5143c-1edd-4e50-a2ef-350a47678060}';
  {$EXTERNALSYM MF_CAPTURE_ENGINE_PREVIEW_STOPPED}

  // ac2b027b-ddf9-48a0-89be-38ab35ef45c0
  // MF_CAPTURE_ENGINE_RECORD_STARTED
  // Signals capture engine record started in response to IMFCaptureEngine.StartRecord
  MF_CAPTURE_ENGINE_RECORD_STARTED    : TGUID = '{ac2b027b-ddf9-48a0-89be-38ab35ef45c0}';
  {$EXTERNALSYM MF_CAPTURE_ENGINE_RECORD_STARTED}

  // 55e5200a-f98f-4c0d-a9ec-9eb25ed3d773
  // MF_CAPTURE_ENGINE_RECORD_STOPPED
  // Signals capture engine record stopped in response to IMFCaptureEngine.StopRecord
  MF_CAPTURE_ENGINE_RECORD_STOPPED    : TGUID = '{55e5200a-f98f-4c0d-a9ec-9eb25ed3d773}';
  {$EXTERNALSYM MF_CAPTURE_ENGINE_RECORD_STOPPED}

  // 3c50c445-7304-48eb-865d-bba19ba3af5c
  // MF_CAPTURE_ENGINE_PHOTO_TAKEN
  // Signals a photo was taken by capture engine in response to IMFCaptureEngine.TakePhoto
  MF_CAPTURE_ENGINE_PHOTO_TAKEN       : TGUID = '{3c50c445-7304-48eb-865d-bba19ba3af5c}';
  {$EXTERNALSYM MF_CAPTURE_ENGINE_PHOTO_TAKEN}

  // e7e75e4c-039c-4410-815b-8741307b63aa
  // MF_CAPTURE_SOURCE_CURRENT_DEVICE_MEDIA_TYPE_SET
  // Signals capture engine succeeded in configuring the device set by the user in IMFCaptureSource.SetCurrentDeviceMediaType
  MF_CAPTURE_SOURCE_CURRENT_DEVICE_MEDIA_TYPE_SET : TGUID = '{e7e75e4c-039c-4410-815b-8741307b63aa}';
  {$EXTERNALSYM MF_CAPTURE_SOURCE_CURRENT_DEVICE_MEDIA_TYPE_SET}

  // 46b89fc6-33cc-4399-9dad-784de77d587c
  // MF_CAPTURE_ENGINE_ERROR
  // Signals an error in capture engine
  MF_CAPTURE_ENGINE_ERROR            : TGUID = '{46b89fc6-33cc-4399-9dad-784de77d587c}';
  {$EXTERNALSYM MF_CAPTURE_ENGINE_ERROR}

  // AA8DC7B5-A048-4e13-8EBE-F23C46C830C1
  // MF_CAPTURE_ENGINE_EFFECT_ADDED
  // Signals an error in capture engine
  MF_CAPTURE_ENGINE_EFFECT_ADDED     : TGUID = '{AA8DC7B5-A048-4e13-8EBE-F23C46C830C1}';
  {$EXTERNALSYM MF_CAPTURE_ENGINE_EFFECT_ADDED}

  // C6E8DB07-FB09-4a48-89C6-BF92A04222C9
  // MF_CAPTURE_ENGINE_EFFECT_REMOVED
  // Signals an error in capture engine
  MF_CAPTURE_ENGINE_EFFECT_REMOVED   : TGUID = '{C6E8DB07-FB09-4a48-89C6-BF92A04222C9}';
  {$EXTERNALSYM MF_CAPTURE_ENGINE_EFFECT_REMOVED}

  // FDED7521-8ED8-431a-A96B-F3E2565E981C
  // MF_CAPTURE_ENGINE_ALL_EFFECTS_REMOVED
  // Signals an error in capture engine
  MF_CAPTURE_ENGINE_ALL_EFFECTS_REMOVED : TGUID = '{fded7521-8ed8-431a-a96b-f3e2565e981c}';
  {$EXTERNALSYM MF_CAPTURE_ENGINE_ALL_EFFECTS_REMOVED}

  // 7BFCE257-12B1-4409-8C34-D445DAAB7578
  // MF_CAPTURE_SINK_PREPARED
  // Signals that the capture sink is prepared to accept GetService calls
  MF_CAPTURE_SINK_PREPARED : TGUID = '{7BFCE257-12B1-4409-8C34-D445DAAB7578}';
  {$EXTERNALSYM MF_CAPTURE_SINK_PREPARED}

  // caaad994-83ec-45e9-a30a-1f20aadb9831
  // MF_CAPTURE_ENGINE_OUTPUT_MEDIA_TYPE_SET
  // Signals that the output type has been set on captureengine in response to IMFCaptureSink2.SetOutputType
  MF_CAPTURE_ENGINE_OUTPUT_MEDIA_TYPE_SET : TGUID = '{caaad994-83ec-45e9-a30a-1f20aadb9831}';
  {$EXTERNALSYM MF_CAPTURE_ENGINE_OUTPUT_MEDIA_TYPE_SET}


  ///////////////////////////////////////////////////////////////////////////////
  //
  // Capture Engine Attributes
  //
  ///////////////////////////////////////////////////////////////////////////////

  // MF_CAPTURE_ENGINE_CAMERA_STREAM_BLOCKED
  // Signals that video capture is being blocked by the driver.
  MF_CAPTURE_ENGINE_CAMERA_STREAM_BLOCKED :	TGUID = '{A4209417-8D39-46F3-B759-5912528F4207}';
  {$EXTERNALSYM MF_CAPTURE_ENGINE_CAMERA_STREAM_BLOCKED}

  // MF_CAPTURE_ENGINE_CAMERA_STREAM_UNBLOCKED
  // Signals that video capture is restored after being blocked.
  MF_CAPTURE_ENGINE_CAMERA_STREAM_UNBLOCKED :	TGUID = '{9BE9EEF0-CDAF-4717-8564-834AAE66415C}';
  {$EXTERNALSYM MF_CAPTURE_ENGINE_CAMERA_STREAM_UNBLOCKED}

  // MF_CAPTURE_ENGINE_D3D_MANAGER
  // Data type: IUnknown
  // This attribute enables video sample allocation using DX11 surfaces,
  // and enables hardware acceleration within the Source Reader.
  // This should be set to the IUnknown interface of an
  // object that implements the IDirect3DDeviceManager11 interface.
  MF_CAPTURE_ENGINE_D3D_MANAGER : TGUID = '{76e25e7b-d595-4283-962c-c594afd78ddf}';
  {$EXTERNALSYM MF_CAPTURE_ENGINE_D3D_MANAGER}

  // MF_CAPTURE_ENGINE_RECORD_SINK_VIDEO_MAX_UNPROCESSED_SAMPLES
  // Data type: UINT32
  // This attribute dictates the maximum number of unprocessed samples that can be buffered for processing in the record sink video path.
  // Once the record has buffered MF_CAPTURE_ENGINE_RECORD_SINK_VIDEO_MAX_UNPROCESSED_SAMPLES number of unprocessed samples, it drops the frame rate by dropping samples
  MF_CAPTURE_ENGINE_RECORD_SINK_VIDEO_MAX_UNPROCESSED_SAMPLES : TGUID = '{76e25e7b-d595-4283-962c-c594afd78ddf}';
  {$EXTERNALSYM MF_CAPTURE_ENGINE_RECORD_SINK_VIDEO_MAX_UNPROCESSED_SAMPLES}

  // MF_CAPTURE_ENGINE_RECORD_SINK_AUDIO_MAX_UNPROCESSED_SAMPLES
  // Data type: UINT32
  // This attribute dictates the maximum number of unprocessed samples that can be buffered for processing in the record sink audio path.
  // Once the record has buffered MF_CAPTURE_ENGINE_RECORD_SINK_AUDIO_MAX_UNPROCESSED_SAMPLES number of unprocessed samples, it drops the frame rate by dropping samples
  MF_CAPTURE_ENGINE_RECORD_SINK_AUDIO_MAX_UNPROCESSED_SAMPLES : TGUID = '{1cddb141-a7f4-4d58-9896-4d15a53c4efe}';
  {$EXTERNALSYM MF_CAPTURE_ENGINE_RECORD_SINK_AUDIO_MAX_UNPROCESSED_SAMPLES}

  // MF_CAPTURE_ENGINE_RECORD_SINK_VIDEO_MAX_PROCESSED_SAMPLES
  // Data type: UINT32
  // This attribute dictates the maximum number of processed samples that can be buffered in the record sink video path.
  MF_CAPTURE_ENGINE_RECORD_SINK_VIDEO_MAX_PROCESSED_SAMPLES : TGUID = '{e7b4a49e-382c-4aef-a946-aed5490b7111}';
  {$EXTERNALSYM MF_CAPTURE_ENGINE_RECORD_SINK_VIDEO_MAX_PROCESSED_SAMPLES}

  // MF_CAPTURE_ENGINE_RECORD_SINK_AUDIO_MAX_PROCESSED_SAMPLES
  // Data type: UINT32
  // This attribute dictates the maximum number of processed samples that can be buffered in the record sink audio path.
  // {9896E12A-F707-4500-B6BD-DB8EB810B50F}
  MF_CAPTURE_ENGINE_RECORD_SINK_AUDIO_MAX_PROCESSED_SAMPLES : TGUID = '{9896E12A-F707-4500-B6BD-DB8EB810B50F}';
  {$EXTERNALSYM MF_CAPTURE_ENGINE_RECORD_SINK_AUDIO_MAX_PROCESSED_SAMPLES}

  // {1C8077DA-8466-4dc4-8B8E-276B3F85923B}
  // MF_CAPTURE_ENGINE_USE_AUDIO_DEVICE_ONLY
  // Data type: UINT32
  // Set this attribute for Capture Engine to use the devices accordingly
  MF_CAPTURE_ENGINE_USE_AUDIO_DEVICE_ONLY : TGUID = '{1C8077DA-8466-4dc4-8B8E-276B3F85923B}';
  {$EXTERNALSYM MF_CAPTURE_ENGINE_USE_AUDIO_DEVICE_ONLY}

  // {7E025171-CF32-4f2e-8F19-410577B73A66}
  // MF_CAPTURE_ENGINE_USE_VIDEO_DEVICE_ONLY
  // Data type: UINT32
  // Set this attribute for Capture Engine to use the devices accordingly
  MF_CAPTURE_ENGINE_USE_VIDEO_DEVICE_ONLY : TGUID = '{7E025171-CF32-4f2e-8F19-410577B73A66}';
  {$EXTERNALSYM MF_CAPTURE_ENGINE_USE_VIDEO_DEVICE_ONLY}

  // {b7c42a6b-3207-4495-b4e7-81f9c35d5991}
  // MF_CAPTURE_ENGINE_DISABLE_HARDWARE_TRANSFORMS
  // Data type: UINT32
  // Set this attribute for capture engine to enable source reader and sink writer hardware transforms
  MF_CAPTURE_ENGINE_DISABLE_HARDWARE_TRANSFORMS : TGUID = '{b7c42a6b-3207-4495-b4e7-81f9c35d5991}';
  {$EXTERNALSYM MF_CAPTURE_ENGINE_DISABLE_HARDWARE_TRANSFORMS}

  // {f9818862-179d-433f-a32f-74cbcf74466d}
  // MF_CAPTURE_ENGINE_DISABLE_DXVA
  // Data type: UINT32
  // Set this attribute for capture engine to disable DXVA
  MF_CAPTURE_ENGINE_DISABLE_DXVA : TGUID = '{f9818862-179d-433f-a32f-74cbcf74466d}';
  {$EXTERNALSYM MF_CAPTURE_ENGINE_DISABLE_DXVA}

  // {bc6989d2-0fc1-46e1-a74f-efd36bc788de}
  // MF_CAPTURE_ENGINE_MEDIASOURCE_CONFIG
  // Data type: IPropertyStore* stored as IUnknown*
  // Set this attribute for capture engine to configure the media source
  MF_CAPTURE_ENGINE_MEDIASOURCE_CONFIG : TGUID = '{bc6989d2-0fc1-46e1-a74f-efd36bc788de}';
  {$EXTERNALSYM MF_CAPTURE_ENGINE_MEDIASOURCE_CONFIG}

  // {2b8ad2e8-7acb-4321-a606-325c4249f4fc}
  // MF_CAPTURE_ENGINE_DECODER_MFT_FIELDOFUSE_UNLOCK_Attribute
  // Data type:  IMFFieldOfUseMFTUnlock* stored as IUnknown*
  // This attribute is used to unlock decoder MFTs with field of use restrictions
  MF_CAPTURE_ENGINE_DECODER_MFT_FIELDOFUSE_UNLOCK_Attribute : TGUID = '{2b8ad2e8-7acb-4321-a606-325c4249f4fc}';
  {$EXTERNALSYM MF_CAPTURE_ENGINE_DECODER_MFT_FIELDOFUSE_UNLOCK_Attribute}

  // {54c63a00-78d5-422f-aa3e-5e99ac649269}
  // MF_CAPTURE_ENGINE_ENCODER_MFT_FIELDOFUSE_UNLOCK_Attribute
  // Data type:  IMFFieldOfUseMFTUnlock* stored as IUnknown*
  // This attribute is used to unlock encoder MFTs with field of use restrictions
  MF_CAPTURE_ENGINE_ENCODER_MFT_FIELDOFUSE_UNLOCK_Attribute : TGUID = '{54c63a00-78d5-422f-aa3e-5e99ac649269}';
  {$EXTERNALSYM MF_CAPTURE_ENGINE_ENCODER_MFT_FIELDOFUSE_UNLOCK_Attribute}

  // {4C808E9D-AAED-4713-90FB-CB24064AB8DA}
  // MF_CAPTURE_ENGINE_ENABLE_CAMERA_STREAMSTATE_NOTIFICATION
  // Data type:  UINT32 - 0/not present - do not eanble notification.  Non-zero, enable notification.
  // Indicates whether stream state notification should be enabled or not.
  MF_CAPTURE_ENGINE_ENABLE_CAMERA_STREAMSTATE_NOTIFICATION :	TGUID = '{4C808E9D-AAED-4713-90FB-CB24064AB8DA}';
  {$EXTERNALSYM MF_CAPTURE_ENGINE_ENABLE_CAMERA_STREAMSTATE_NOTIFICATION}

  // {8e3f5bd5-dbbf-42f0-8542-d07a3971762a}
  // MF_CAPTURE_ENGINE_MEDIA_CATEGORY
  // Data type:  UINT32 - values come from the MF_CAPTURE_ENGINE_MEDIA_CATEGORY_TYPE enumeration.
  // Attribute that dictates the media capture category used by the capture engine.
  MF_CAPTURE_ENGINE_MEDIA_CATEGORY :	TGUID = '{8E3F5BD5-DBBF-42F0-8542-D07A3971762A}';
  {$EXTERNALSYM MF_CAPTURE_ENGINE_MEDIA_CATEGORY}

  // {10f1be5e-7e11-410b-973d-f4b6109000fe}
  // MF_CAPTURE_ENGINE_AUDIO_PROCESSING
  // Data type:  UINT32 - values come from the MF_CAPTURE_ENGINE_AUDIO_PROCESSING_MODE enumeration.
  // Attribute that defines the audio processing mode to be used during capture.
  MF_CAPTURE_ENGINE_AUDIO_PROCESSING :	TGUID = '{10F1BE5E-7E11-410B-973D-F4B6109000FE}';
  {$EXTERNALSYM MF_CAPTURE_ENGINE_AUDIO_PROCESSING}



  ///////////////////////////////////////////////////////////////////////////////
  //
  // Capture Engine Event Attributes
  //
  ///////////////////////////////////////////////////////////////////////////////

  // {abfa8ad5-fc6d-4911-87e0-961945f8f7ce}
  // MF_CAPTURE_ENGINE_EVENT_GENERATOR_GUID
  // Data type: GUID
  // This attribute provids information about which capture engine component generated the event
  MF_CAPTURE_ENGINE_EVENT_GENERATOR_GUID : TGUID = '{abfa8ad5-fc6d-4911-87e0-961945f8f7ce}';
  {$EXTERNALSYM MF_CAPTURE_ENGINE_EVENT_GENERATOR_GUID}

  // {82697f44-b1cf-42eb-9753-f86d649c8865}
  // MF_CAPTURE_ENGINE_EVENT_STREAM_INDEX
  // Data type: DWORD
  // This attribute provids information about the component stream index with which the event is associated
  MF_CAPTURE_ENGINE_EVENT_STREAM_INDEX : TGUID = '{82697f44-b1cf-42eb-9753-f86d649c8865}';
  {$EXTERNALSYM MF_CAPTURE_ENGINE_EVENT_STREAM_INDEX}

  // {03160B7E-1C6F-4DB2-AD56-A7C430F82392}
  // MF_CAPTURE_ENGINE_SELECTEDCAMERAPROFILE
  // Data Type:  GUID.
  // The GUID represents the GUID of the selected profile ID. If this attribute is not set,
  // CaptureEngine will internally use the KSCAMERAPROFILE_Legacy. If this attribute is set to
  // GUID_NULL, it will inform the camera driver that the application is profile aware and should
  // provide the full range of media types.
  MF_CAPTURE_ENGINE_SELECTEDCAMERAPROFILE :	TGUID = '{03160B7E-1C6F-4DB2-AD56-A7C430F82392}';
  {$EXTERNALSYM MF_CAPTURE_ENGINE_SELECTEDCAMERAPROFILE}

  // {3CE88613-2214-46C3-B417-82F8A313C9C3}
  // MF_CAPTURE_ENGINE_SELECTEDCAMERAPROFILE_INDEX
  // Data Type:  UINT32.
  // The UINT32 representing the index of the profile.  If this index is not present, the selected
  // profile index is assumed to be 0.
  MF_CAPTURE_ENGINE_SELECTEDCAMERAPROFILE_INDEX :	TGUID = '{3CE88613-2214-46C3-B417-82F8A313C9C3}';
  {$EXTERNALSYM MF_CAPTURE_ENGINE_SELECTEDCAMERAPROFILE_INDEX}



  // CLSID_MFCaptureEngine
  // Data type: GUID
  // CLSID for creating CaptureEngine
  //
  // {efce38d3-8914-4674-a7df-ae1b3d654b8a}
  CLSID_MFCaptureEngine       : TGUID = '{efce38d3-8914-4674-a7df-ae1b3d654b8a}';
  {$EXTERNALSYM CLSID_MFCaptureEngine}


  //////////////////////////////////////////////
  // MFSampleExtension_DeviceReferenceSystemTime
  // Data type: GUID
  // Attribute that indicates the original device timestamp on the sample, of type MFTTIME.
  // {6523775a-ba2d-405f-b2c5-01ff88e2e8f6}
  MFSampleExtension_DeviceReferenceSystemTime    : TGUID = '{6523775a-ba2d-405f-b2c5-01ff88e2e8f6}';
  {$EXTERNALSYM MFSampleExtension_DeviceReferenceSystemTime}


  ////////////////////////////////////////////////////
  // CLSID_MFCaptureEngineClassFactory
  // Data type: GUID
  // CLSID for creating the MFCaptureEngine Class Factory.
  // This object can be used to create initialized Capture Engine Objects
  // {efce38d3-8914-4674-a7df-ae1b3d654b8a}
  CLSID_MFCaptureEngineClassFactory       : TGUID = '{efce38d3-8914-4674-a7df-ae1b3d654b8a}';
  {$EXTERNALSYM CLSID_MFCaptureEngineClassFactory}


type

  // Interfaces Type Library
  //////////////////////////////////////////////////////////////////////////////


  // Interface IMFCaptureEngineOnEventCallback
  // =========================================
  // This interface is used as the callback mechanism for the
  // Capture Engine. The application
  // passes in an instance of an object that implements this
  // callback interface as a param to IMFCaptureEngine.Intialize
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFCaptureEngineOnEventCallback);'}
  {$EXTERNALSYM IMFCaptureEngineOnEventCallback}
  IMFCaptureEngineOnEventCallback = interface(IUnknown)
  ['{aeda51c0-9025-4983-9012-de597b88b089}']

    function OnEvent(pEvent: IMFMediaEvent): HResult; stdcall;
    // Callback function that completes an asynchronous call on the capture engine.
    //
    // <param name = "IMFMediaEvent">
    // Pointer to IMFMediaEvent interface

  end;
  IID_IMFCaptureEngineOnEventCallback = IMFCaptureEngineOnEventCallback;
  {$EXTERNALSYM IID_IMFCaptureEngineOnEventCallback}


  // IMFCaptureEngineOnSampleCallback Interface
  // ==========================================
  // This interface is used as the callback mechanism for returning
  // samples from the Capture Engine. The application
  // passes in an instance of an object that implements this
  // callback interface as an attribute by configuring one
  // of the Capture Engine's sinks. This callback is used to
  // return samples from a single stream.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFCaptureEngineOnSampleCallback);'}
  {$EXTERNALSYM IMFCaptureEngineOnSampleCallback}
  IMFCaptureEngineOnSampleCallback = interface(IUnknown)
  ['{52150b82-ab39-4467-980f-e48bf0822ecd}']

    function OnSample(pSample: IMFSample): HResult; stdcall;
    // Callback function that completes an asynchronous request for the
    // next available sample.
    // <param name = "pSample">
    // Contains the next sample for the stream. It is possible for
    // this parameter to be Nil, so the application should
    // explicitly check for Nil before dereferencing the sample.

  end;
  IID_IMFCaptureEngineOnSampleCallback = IMFCaptureEngineOnSampleCallback;
  {$EXTERNALSYM IID_IMFCaptureEngineOnSampleCallback}



  // IMFCaptureSink Interface
  // ========================
  // This interface is used to configure a sink
  // within the Capture Engine. The app obtains a
  // pointer to this interface using IMFCaptureEngine.GetSink.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFCaptureSink);'}
  {$EXTERNALSYM IMFCaptureSink}
  IMFCaptureSink = interface(IUnknown)
  ['{72d6135b-35e9-412c-b926-fd5265f2a885}']

    function GetOutputMediaType(dwSinkStreamIndex: DWORD;
                                out ppMediaType: IMFMediaType): HResult; stdcall;
    // Method to get the current output media type on the sink
    // <param name = "ppMediaType">
    // Receives a pointer to the current output media type of the sink.

    function GetService(dwSinkStreamIndex: DWORD;
                        const rguidService: REFGUID;
                        const riid: REFIID;
                        out ppUnknown_: IUnknown): HResult; stdcall;
    // Allows access to an interface from IMFCaptureSink
    // <param name = "dwSinkStreamIndex">
    // The zero based stream index of sink
    // <param name = "rguid">
    // Specifies the service guid. Can be GUID_NULL.
    // <param name = "riid">
    // Specifies the interface identifier
    // <param name = "ppUnknown">
    // Receives a pointer to the interface requested

    function AddStream(dwSourceStreamIndex: DWORD;
                       pMediaType: IMFMediaType;
                       pAttributes: IMFAttributes;
                       out pdwSinkStreamIndex: DWORD): HResult; stdcall;
    // Allows application to choose which source read stream is used as input into this sink
    // <param name = "dwSourceStreamIndex">
    // The zero based source stream index
    // <param name = "pMediaType">
    // Desired output media type
    // <param name = "pAttributes">
    // A pointer to the IMFAttributes interface of an attribute store
    // <param name = "pdwSinkStreamIndex">
    // Sink stream index.

    function Prepare(): HResult; stdcall;
    // Prepares the sink. At this time the topology of the sink is built,
    // after which GetService can be used to configure individual components.
    // <remarks>
    // This method is optional and only required if the user wants to configure the encoder,
    // XVP etc before actually calling StartPreview/StartRecord/TakePhoto.

    function RemoveAllStreams(): HResult; stdcall;
    // Allows application to remove all streams.

   end;
  IID_IMFCaptureSink = IMFCaptureSink;
  {$EXTERNALSYM IID_IMFCaptureSink}



  // IMFCaptureRecordSink Interface
  // ==============================
  // This interface is used to set the output of the record sink
  // The output can be set to a file, a bytestream or a callback.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFCaptureRecordSink);'}
  {$EXTERNALSYM IMFCaptureRecordSink}
  IMFCaptureRecordSink = interface(IMFCaptureSink)
  ['{3323b55a-f92a-4fe2-8edc-e9bfc0634d77}']

    function SetOutputByteStream(ByteStream: IMFByteStream;
                                 const guidContainerType: REFGUID): HResult; stdcall;
    // Allows an app to the set output of the record sink to an IMFBytestream
    // <param name = "pByteStream">
    // Pointer to an IMFByestream
    // <param name = "guidContainerType">
    // Container type GUID.

    function SetOutputFileName(fileName: PWideChar): HResult; stdcall;
    // Allows an app to the set output file name to store a muxed stream
    // <param name = "fileName">
    // File name for storing output.

    function SetSampleCallback(const dwStreamSinkIndex: DWORD;
                               pCallback: IMFCaptureEngineOnSampleCallback): HResult; stdcall;
    // Allows an app to retrieve samples via a callback
    // <param name = "dwSinkStreamIndex">
    // The zero based stream index for sink
    // <param name = "pCallback">
    // Pointer to IMFCaptureEngineOnSamplecallback interface.

    function SetCustomSink(pMediaSink: IMFMediaSink): HResult; stdcall;
    // Allows an app to set a custom sink for the record path
    // <param name = "pMediaSink">
    // Pointer to IUnknown(IMFMediaSink) interface.

    function GetRotation(const dwStreamIndex: DWORD;
                         out pdwRotationValue: DWORD): HResult; stdcall;
    // Allows the application to get the current rotation value on the video stream.
    // <param name = "dwStreamIndex">
    // Index of the stream to query.  Must be a video stream.
    // <param name = "pdwRotationValue">
    // The degree by which the video is rotated.  Valid values are 0, 90, 180, or 270.

    function SetRotation(const dwStreamIndex: DWORD;
                         dwRotationValue: DWORD): HResult; stdcall;
    // Allows the application to rotate the preview video stream 0, 90, 180 or 270 degrees.
    // 0 degrees restores the video to non-rotated state.
    // <param name = "dwStreamIndex">
    // Index of the stream to rotate.  Must be a video stream.
    // <param name = "dwRotationValue">
    // The degree by which the video is rotated.  Valid values are 0, 90, 180, or 270 degrees.

  end;
  IID_IMFCaptureRecordSink = IMFCaptureRecordSink;
  {$EXTERNALSYM IID_IMFCaptureRecordSink}



  // IMFCapturePreviewSink Interface
  // ===============================
  // This interface is used to set the output of the preview sink
  // The output can be set a handle for rendering or a callback.
  // Remarks:
  // To start preview, call IMFCaptureEngine.StartPreview.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFCapturePreviewSink);'}
  {$EXTERNALSYM IMFCapturePreviewSink}
  IMFCapturePreviewSink = interface(IMFCaptureSink)
  ['{77346cfd-5b49-4d73-ace0-5b52a859f2e0}']

    function SetRenderHandle(handle: THandle): HResult; stdcall;
    // Allows an app to render samples
    // <param name = "handle">
    // HWND (= THandle)

    function SetRenderSurface(pSurface: IUnknown): HResult; stdcall;
    // Allows an app to render samples.
    // <param name = "pSurface">
    // IUnknown(IDCompositionVisual)

    function UpdateVideo(pSrc: PMFVideoNormalizedRect;
                         pDst: PRECT;
                         pBorderClr: PCOLORREF): HResult; stdcall;
    // Updates the video frame.
    // Call this method when the preview window receives a WM_PAINT or WM_SIZE mes
    // Parameters
    //  pSrc [in]
    //    A pointer to an MFVideoNormalizedRect structure that specifies the source rectangle.
    //    The source rectangle defines the area of the video frame that is displayed.
    //    If this parameter is Nil, the entire video frame is displayed.
    //  pDst [in]
    //    A pointer to a TRECT structure that specifies the destination rectangle.
    //    The destination rectangle defines the area of the window or DirectComposition
    //    visual where the video is drawn.
    //  pBorderClr [in]
    //    The border color. Use the RGB macro to create this value.

    function SetSampleCallback(dwStreamSinkIndex: DWORD;
                               pCallback: IMFCaptureEngineOnSampleCallback): HResult; stdcall;
    // Allows an app to retrieve samples via a callback
    // <param name = "dwSinkStreamIndex">
    // The zero based stream index for sink
    // <param name = "pCallback">
    // Pointer to IMFCaptureEngineOnSamplecallback interface.

    function GetMirrorState(out pfMirrorState: BOOL): HResult; stdcall;
    // Method to query for the current preview output's mirroring state
    // <param name = "pfMirrorState">
    // Receives the boolean flag indicating the current mirror state (true=on, false=off).

    function SetMirrorState(fMirrorState: BOOL): HResult; stdcall;
    // Method to set the current preview output's mirroring state
    // <param name = "fMirrorState">
    // Boolean flag indicating the current mirror state (true=on, false=off).

    function GetRotation(const dwStreamIndex: DWORD;
                         out pdwRotationValue: DWORD): HResult; stdcall;
    // Allows the application to get the current rotation value on the video stream.
    // <param name = "dwStreamIndex">
    // Index of the stream to query.  Must be a video stream.
    // <param name = "pdwRotationValue">
    // The degree by which the video is rotated.  Valid values are 0, 90, 180, or 270 degrees.

    function SetRotation(const dwStreamIndex: DWORD;
                         dwRotationValue: DWORD): HResult; stdcall;
    // Allows the application to rotate the preview video stream 0, 90, 180 or 270 degrees.
    // 0 degrees restores the video to non-rotated state.
    // <param name = "dwStreamIndex">
    // Index of the stream to rotate.  Must be a video stream.
    // <param name = "dwRotationValue">
    // The degree by which the video is rotated.  Valid values are 0, 90, 180, or 270 degrees.

    function SetCustomSink(var pMediaSink: IMFMediaSink): HResult; stdcall;
    // Allows an app to set a custom sink for the preview path
    // <param name = "pMediaSink">
    // Pointer to IUnknown(IMFMediaSink) interface.

  end;
  // IMFCapturePreviewSink
  IID_IMFCapturePreviewSink = IMFCapturePreviewSink;
  {$EXTERNALSYM IID_IMFCapturePreviewSink}



  // IMFCapturePhotoSink Interface
  // =============================
  // This interface is used to set the output of the photo sink.
  // The output can be set to a file or a callback.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFCapturePhotoSink);'}
  {$EXTERNALSYM IMFCapturePhotoSink}
  IMFCapturePhotoSink = interface(IMFCaptureSink)
  ['{d2d43cc8-48bb-4aa7-95db-10c06977e777}']
    function SetOutputFileName(fileName: PWideChar): HResult; stdcall;
    // Allows an app to the set output file name to store an image.
    // <param name = "fileName">
    // File name for storing output.

    function SetSampleCallback(pCallback: IMFCaptureEngineOnSampleCallback): HResult; stdcall;
    // Allows an app to retrieve a sample via a callback.
    // <param name = "pCallback">
    // Pointer to IMFCaptureEngineOnSamplecallback interface.

    function SetOutputByteStream(pByteStream: IMFByteStream): HResult; stdcall;
    // Allows an app to the set output of the photo sink to an IMFBytestream.
    // <param name = "pByteStream">
    // Pointer to an IMFByestream.

  end;
  // IMFCapturePhotoSink
  IID_IMFCapturePhotoSink = IMFCapturePhotoSink;
  {$EXTERNALSYM IID_IMFCapturePhotoSink}



  // IMFCaptureSource Interface
  // ==========================
  // This interface is used to configure the capture source which is a wrapper over the source reader.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFCaptureSource);'}
  {$EXTERNALSYM IMFCaptureSource}
  IMFCaptureSource = interface(IUnknown)
  ['{439a42a8-0d2c-4505-be83-f79b2a05d5c4}']

    function GetCaptureDeviceSource(mfCaptureEngineDeviceType: MF_CAPTURE_ENGINE_DEVICE_TYPE;
                                    out ppMediaSource: IMFMediaSource): HResult; stdcall;
    // Allows an app to get the current capture device's IMFMediaSource object pointer.
    // <param name = "mfCaptureEngineDeviceType">
    // Device type from MF_CAPTURE_ENGINE_DEVICE_TYPE enumeration.
    // <param name = "ppMediaSource">
    // Receives a pointer to IMFMediaSource that represents a device.

    function GetCaptureDeviceActivate(mfCaptureEngineDeviceType: MF_CAPTURE_ENGINE_DEVICE_TYPE;
                                      out ppActivate: IMFActivate): HResult; stdcall;
    // Allows an app to get the current capture device's IMFActivate object pointer.
    // <param name = "mfCaptureEngineDeviceType">
    // Device type from MF_CAPTURE_ENGINE_DEVICE_TYPE enumeration.
    // <param name = "ppActivate">
    // Receives a pointer to IMFActivate that represents a device.

    function GetService(const rguidService: REFIID;
                        const riid: REFIID;
                        out ppUnknown_: IUnknown): HResult; stdcall;
    // Allows access to an interface from IMFCaptureSource.
    // <param name = "rguid">
    // Specifies the service guid. Can be GUID_NULL.
    // <param name = "riid">
    // Specifies the interface identifier.
    // <param name = "ppUnknown">
    // Receives a pointer to the interface requested.

    function AddEffect(dwSourceStreamIndex: DWORD;
                       pUnknown_: IUnknown): HResult; stdcall;
    // Allows an app to add an effect to the device stream.
    // <param name = "dwSourceStreamIndex">
    // Zero based stream index of source.
    // <param name = "pUnknown">
    // Pointer to IUnknown, can be an IMFTransform or IMFActivate.

    function RemoveEffect(dwSourceStreamIndex: DWORD;
                          pUnknown_: IUnknown): HResult; stdcall;
    // Allows an app to remove an effect to the device stream.
    // <param name = "dwSourceStreamIndex">
    // Zero based stream index of source.
    // <param name = "pUnknown">
    // Pointer to IUnknown, can be an IMFTransform or IMFActivate.

    function RemoveAllEffects(dwSourceStreamIndex: DWORD): HResult; stdcall;
    // Allows an app to remove all effects from to the device stream.
    // <param name = "dwSourceStreamIndex">
    // Zero based stream index of source.

    function GetAvailableDeviceMediaType(dwSourceStreamIndex: DWORD;
                                         dwMediaTypeIndex: DWORD;
                                         ppMediaType: PIMFMediaType = Nil): HResult; stdcall;
    // Gets an available media type for a stream from the device.
    // <param name = "dwSourceStreamIndex">
    // Zero based stream index of source.
    // <param name = "dwMediaTypeIndex">
    // Index of the media type to retrieve. Media types are indexed from zero.
    // <param name = "ppMediaType">
    // Receives a pointer to IMFMediaType. App should release this.

    function SetCurrentDeviceMediaType(dwSourceStreamIndex: DWORD;
                                       pMediaType: IMFMediaType): HResult; stdcall;
    // Sets a media type for an stream from the device.
    // <param name = "dwSourceStreamIndex">
    // Zero based stream index of source.
    // <param name = "pMediaType">
    // Pointer to IMFMediaType.

    function GetCurrentDeviceMediaType(dwSourceStreamIndex: DWORD;
                                       out ppMediaType: IMFMediaType): HResult; stdcall;
    // Gets the currently set media type for a stream from the device
    // <param name = "dwSourceStreamIndex">
    // Zero based stream index of source
    // <param name = "ppMediaType">
    // Receives a pointer to IMFMediaType. App should release this.

    function GetDeviceStreamCount(out pdwStreamCount: DWORD): HResult; stdcall;
    // Gets the number of streams that is available on the source (includes audio and video streams)
    // <param name = "pdwStreamCount">
    // Receives the number of streams available.

    function GetDeviceStreamCategory(dwSourceStreamIndex: DWORD;
                                     out pStreamCategory: MF_CAPTURE_ENGINE_STREAM_CATEGORY): HResult; stdcall;
    // Gets the stream category for a given source stream index
    // <param name = "dwSourceStreamIndex">
    // Source stream index
    // <param name = "pStreamCategory">
    // Receives a value from the MF_CAPTURE_ENGINE_STREAM_CATEGORY enumeration.

    function GetMirrorState(const dwStreamIndex: DWORD;
                            out pfMirrorState: BOOL): HResult; stdcall;
    // Method to query for the current preview output's mirroring state
    // <param name = "pfMirrorState">
    // Receives the boolean flag indicating the current mirror state (true=on, false=off).

    function SetMirrorState(const dwStreamIndex: DWORD;
                            fMirrorState: BOOL): HResult; stdcall;
    // Method to set the current preview output's mirroring state
    // <param name = "fMirrorState">
    // Boolean flag indicating the current mirror state (true=on, false=off).

    function GetStreamIndexFromFriendlyName(uifriendlyName: UINT32;
                                            out pdwActualStreamIndex: DWORD): HResult; stdcall;
    // Translates a friendly stream name to an actual device stream index.
    // <param name = "uifriendlyName">
    // can be one of:
    //    MF_CAPTURE_ENGINE_FIRST_SOURCE_AUDIO_STREAM,
    //    MF_CAPTURE_ENGINE_FIRST_SOURCE_VIDEO_STREAM,
    //    MF_CAPTURE_ENGINE_FIRST_SOURCE_PHOTO_STREAM,
    //    MF_CAPTURE_ENGINE_PREFERRED_SOURCE_VIDEO_STREAM_FOR_RECORD,
    //    MF_CAPTURE_ENGINE_PREFERRED_SOURCE_VIDEO_STREAM_FOR_PREVIEW,
    //    MF_CAPTURE_ENGINE_FIRST_SOURCE_INDEPENDENT_PHOTO_STREAM
    // <param name = "pdwActualStreamIndex>
    // Receives the value of the stream index that corresponds to the friendly name

  end;
  // IMFCaptureSource
  IID_IMFCaptureSource = IMFCaptureSource;
  {$EXTERNALSYM IID_IMFCaptureSource}


  // IMFCaptureEngine Interface
  // ==========================
  // This interface is used to control the Capture Engine.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFCaptureEngine);'}
  {$EXTERNALSYM IMFCaptureEngine}
  IMFCaptureEngine = interface(IUnknown)
  ['{a6bba433-176b-48b2-b375-53aa03473207}']
    function Initialize(pEventCallback: IMFCaptureEngineOnEventCallback;
                        pAttributes: IMFAttributes;
                        pAudioSource: IUnknown;
                        pVideoSource: IUnknown): HResult; stdcall;
    // Initializes the the capture engine.
    // App should listen for MF_CAPTURE_ENGINE_INITIALIZED via IMFCaptureEngineOnEventCallback.
    // <param name ="pEventCallback">
    // A pointer to IMFCaptureEngineOnEventCallback interface
    // <param name ="pAttributes">
    // A pointer to IMFAttributes interface
    // <param name ="pAudioSource">
    // A pointer to IMFMediaSource or IMFActivate interface that represents an audio device.
    // <param name ="pVideoSource">
    // A pointer to IMFAttributes or IMFActivate interface that represetns a video device.

    function StartPreview(): HResult; stdcall;
    // Asynchronous method to start preview.
    // App should listen for MF_CAPTURE_ENGINE_PREVIEW_STARTED via IMFCaptureEngineOnEventCallback.

    function StopPreview(): HResult; stdcall;
    // Asynchronous method to stop preview.
    // App should listen for MF_CAPTURE_ENGINE_PREVIEW_STOPPED via IMFCaptureEngineOnEventCallback.

    function StartRecord(): HResult; stdcall;
    // Asynchronous method to start recording.
    // App should listen for MF_CAPTURE_ENGINE_RECORD_STARTED via IMFCaptureEngineOnEventCallback.

    function StopRecord(bFinalize: BOOL;
                        bFlushUnprocessedSamples: BOOL): HResult; stdcall;
    // Asynchronous method to stop recording.
    // App should listen for MF_CAPTURE_ENGINE_RECORD_STOPPED via IMFCaptureEngineOnEventCallback.
    // <param name = "bFinalize">
    // Specifies if the output file should be finalized.
    // If this is false then the output file wont be able to play back.
    // <param name = "bFlushUnprocessedSamples">
    // Specifies if the unprocessed samples waiting to be encoded should be flushed.

    function TakePhoto(): HResult; stdcall;
    // Asynchronous method to take a photo.
    // App should listen for MF_CAPTURE_ENGINE_PHOTO_TAKEN via IMFCaptureEngineOnEventCallback.

    function GetSink(mfCaptureEngineSinkType: MF_CAPTURE_ENGINE_SINK_TYPE;
                     out ppSink: IMFCaptureSink): HResult; stdcall;
    // Method to obtain access to an IMFCaptureSink.
    // <param name = "mfCaptureEngineSinkType">
    // Specifies the capture sink type from the MF_CAPTURE_ENGINE_SINK_TYPE enumeration.
    // <param name = "ppSink">
    // Receives a pointer to IMFCaptureSink interface.

    function GetSource(out ppSource: IMFCaptureSource): HResult; stdcall;
    // Method to obtain access to IMFCaptureSource.
    // <param name = "ppSource">
    // Receives a pointer to IMFCaptureSource interface.

  end;
  // IMFCaptureEngine
  IID_IMFCaptureEngine = IMFCaptureEngine;
  {$EXTERNALSYM IID_IMFCaptureEngine}



  ////////////////////////////////////////////////////
  //
  // Class Factory for MF Capture Engine
  //
  ////////////////////////////////////////////////////


  // IMFCaptureEngineClassFactory Interface
  // ======================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFCaptureEngineClassFactory);'}
  {$EXTERNALSYM IMFCaptureEngineClassFactory}
  IMFCaptureEngineClassFactory = interface(IUnknown)
  ['{8f02d140-56fc-4302-a705-3a97c78be779}']

    function CreateInstance(const clsid: REFCLSID;
                            const riid: REFIID;
                            out ppvObject): HResult; stdcall;

  end;
  IID_IMFCaptureEngineClassFactory = IMFCaptureEngineClassFactory;
  {$EXTERNALSYM IID_IMFCaptureEngineClassFactory}



  // IMFCaptureEngineOnSampleCallback2 Interface
  // ===========================================
  //
  // This interface is used to communicate format change events inband with sample callback.
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFCaptureEngineOnSampleCallback2);'}
  {$EXTERNALSYM IMFCaptureEngineOnSampleCallback2}
  IMFCaptureEngineOnSampleCallback2 = interface(IMFCaptureEngineOnSampleCallback)
  ['{e37ceed7-340f-4514-9f4d-9c2ae026100b}']

    function OnSynchronizedEvent(var pEvent: IMFMediaEvent): HResult; stdcall;

  end; // IMFCaptureEngineOnSampleCallback2Interface
  IID_IMFCaptureEngineOnSampleCallback2 = IMFCaptureEngineOnSampleCallback2;
  {$EXTERNALSYM IID_IMFCaptureEngineOnSampleCallback2}



  // IMFCaptureSink2 Interface
  // =========================
  // This interface is used to set the output of the record sink or preview sink after
  // record or preview has started.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFCaptureSink2);'}
  {$EXTERNALSYM IMFCaptureSink2}
  IMFCaptureSink2 = interface(IMFCaptureSink)
  ['{f9e4219e-6197-4b5e-b888-bee310ab2c59}']

    function SetOutputMediaType(const dwStreamIndex: DWORD;
                                pMediaType: IMFMediaType;
                                pEncodingAttributes: IMFAttributes): HResult; stdcall;

  end;
  // IMFCaptureSink2
  IID_IMFCaptureSink2 = IMFCaptureSink2;
  {$EXTERNALSYM IID_IMFCaptureSink2}



  // Additional Prototypes for ALL interfaces

  // End of Additional Prototypes

implementation

  // Implement Additional functions here.

end.
