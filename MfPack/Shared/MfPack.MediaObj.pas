// FactoryX
//
// Copyright: � FactoryX. All rights reserved.
//
// Project: MfPack - Shared
// Project location: https://sourceforge.net/projects/MFPack
// Module: MfPack.MediaObj.pas
// Kind: Pascal / Delphi unit
// Release date: 08-07-2012
// Language: ENU
//
// Revision Version: 2.6.4
// Description: Define the interfaces for DirectX Media Objects.
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
// Remarks: Requires Windows Vista or later.
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
// Source: mediaobj.h
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
unit MfPack.MediaObj;

  {$HPPEMIT ''}
  {$HPPEMIT '#include "mediaobj.h"'}
  {$HPPEMIT ''}

interface

uses

  {WinApi}
  WinApi.Windows,
  System.Win.ComObj,
  {MfPack}
  MfPack.MfpTypes,
  MfPack.Unknwn;

  {$WEAKPACKAGEUNIT ON}
  {$MINENUMSIZE 4}

  {$IFDEF WIN32}
    {$ALIGN 1}
  {$ELSE}
    {$ALIGN 8} // Win64
  {$ENDIF}

  {$I 'MfPack.inc'}


type
  //  PDmoMediaType = ^DMO_MEDIA_TYPE;
  //  DMO_MEDIA_TYPE = AM_MEDIA_TYPE;

  PDmoMediaType = ^_DMOMediaType;
  PDMO_MEDIA_TYPE = ^_DMOMediaType;
 _DMOMediaType = record
    majortype: TGUID;
    subtype: TGUID;
    bFixedSizeSamples: BOOL;
    bTemporalCompression: BOOL;
    lSampleSize: ULONG;
    formattype: TGUID;
    pUnk: PIUnknown;
    cbFormat: ULONG;
    pbFormat: PByte;
  end;
   {$EXTERNALSYM PDmoMediaType}
  DMO_MEDIA_TYPE = _DMOMediaType;
  {$EXTERNALSYM DMO_MEDIA_TYPE}


  PReferenceTime = ^REFERENCE_TIME;
  REFERENCE_TIME = LONGLONG;
  {$EXTERNALSYM REFERENCE_TIME}


  PDMO_INPUT_DATA_BUFFER_FLAGS = ^_DMO_INPUT_DATA_BUFFER_FLAGS;
  _DMO_INPUT_DATA_BUFFER_FLAGS        = (
    DMO_INPUT_DATA_BUFFERF_SYNCPOINT  = $1,
    DMO_INPUT_DATA_BUFFERF_TIME        = $2,
    DMO_INPUT_DATA_BUFFERF_TIMELENGTH  = $4);
  {$EXTERNALSYM _DMO_INPUT_DATA_BUFFER_FLAGS}
  DMO_INPUT_DATA_BUFFER_FLAGS = _DMO_INPUT_DATA_BUFFER_FLAGS;
  {$EXTERNALSYM DMO_INPUT_DATA_BUFFER_FLAGS}


  PDMO_OUTPUT_DATA_BUFFER_FLAGS = ^_DMO_OUTPUT_DATA_BUFFER_FLAGS;
  _DMO_OUTPUT_DATA_BUFFER_FLAGS         = (
    DMO_OUTPUT_DATA_BUFFERF_SYNCPOINT   = $1,
    DMO_OUTPUT_DATA_BUFFERF_TIME        = $2,
    DMO_OUTPUT_DATA_BUFFERF_TIMELENGTH  = $4,
    DMO_OUTPUT_DATA_BUFFERF_INCOMPLETE  = $1000000);
  {$EXTERNALSYM _DMO_OUTPUT_DATA_BUFFER_FLAGS}
  DMO_OUTPUT_DATA_BUFFER_FLAGS = _DMO_OUTPUT_DATA_BUFFER_FLAGS;
  {$EXTERNALSYM DMO_OUTPUT_DATA_BUFFER_FLAGS}

  PDMO_INPUT_STATUS_FLAGS = ^DMO_INPUT_STATUS_FLAGS;
  _DMO_INPUT_STATUS_FLAGS               = (
    DMO_INPUT_STATUSF_ACCEPT_DATA = $1);
  {$EXTERNALSYM _DMO_INPUT_STATUS_FLAGS}
  DMO_INPUT_STATUS_FLAGS = _DMO_INPUT_STATUS_FLAGS;
  {$EXTERNALSYM DMO_INPUT_STATUS_FLAGS}

  PDMO_INPUT_STREAM_INFO_FLAGS = ^DMO_INPUT_STREAM_INFO_FLAGS;
  _DMO_INPUT_STREAM_INFO_FLAGS                  = (
    DMO_INPUT_STREAMF_WHOLE_SAMPLES             = $1,
    DMO_INPUT_STREAMF_SINGLE_SAMPLE_PER_BUFFER  = $2,
    DMO_INPUT_STREAMF_FIXED_SAMPLE_SIZE         = $4,
    DMO_INPUT_STREAMF_HOLDS_BUFFERS             = $8);
  {$EXTERNALSYM _DMO_INPUT_STREAM_INFO_FLAGS}
  DMO_INPUT_STREAM_INFO_FLAGS  = _DMO_INPUT_STREAM_INFO_FLAGS;
  {$EXTERNALSYM DMO_INPUT_STREAM_INFO_FLAGS}

  PDMO_OUTPUT_STREAM_INFO_FLAGS = ^_DMO_OUTPUT_STREAM_INFO_FLAGS;
  _DMO_OUTPUT_STREAM_INFO_FLAGS                  = (
    DMO_OUTPUT_STREAMF_WHOLE_SAMPLES             = $1,
    DMO_OUTPUT_STREAMF_SINGLE_SAMPLE_PER_BUFFER  = $2,
    DMO_OUTPUT_STREAMF_FIXED_SAMPLE_SIZE         = $4,
    DMO_OUTPUT_STREAMF_DISCARDABLE               = $8,
    DMO_OUTPUT_STREAMF_OPTIONAL                  = $10);
  {$EXTERNALSYM _DMO_OUTPUT_STREAM_INFO_FLAGS}
  DMO_OUTPUT_STREAM_INFO_FLAGS = _DMO_OUTPUT_STREAM_INFO_FLAGS;
  {$EXTERNALSYM DMO_OUTPUT_STREAM_INFO_FLAGS}

  PDMO_SET_TYPE_FLAGS = ^_DMO_SET_TYPE_FLAGS;
  _DMO_SET_TYPE_FLAGS                           = (
    DMO_SET_TYPEF_TEST_ONLY                     = $1,
    DMO_SET_TYPEF_CLEAR                         = $2);
  {$EXTERNALSYM _DMO_SET_TYPE_FLAGS}
  DMO_SET_TYPE_FLAGS = _DMO_SET_TYPE_FLAGS;
  {$EXTERNALSYM DMO_SET_TYPE_FLAGS}


  PDMO_PROCESS_OUTPUT_FLAGS = ^_DMO_PROCESS_OUTPUT_FLAGS;
  _DMO_PROCESS_OUTPUT_FLAGS                     = (
    DMO_PROCESS_OUTPUT_DISCARD_WHEN_NO_BUFFER     = $1);
  {$EXTERNALSYM _DMO_PROCESS_OUTPUT_FLAGS}
  DMO_PROCESS_OUTPUT_FLAGS = _DMO_PROCESS_OUTPUT_FLAGS;
  {$EXTERNALSYM DMO_PROCESS_OUTPUT_FLAGS}


  PDMO_INPLACE_PROCESS_FLAGS = ^_DMO_INPLACE_PROCESS_FLAGS;
  _DMO_INPLACE_PROCESS_FLAGS          = (
    DMO_INPLACE_NORMAL = 0,
    DMO_INPLACE_ZERO  = $1);
  {$EXTERNALSYM _DMO_INPLACE_PROCESS_FLAGS}
  DMO_INPLACE_PROCESS_FLAGS = _DMO_INPLACE_PROCESS_FLAGS;
  {$EXTERNALSYM DMO_INPLACE_PROCESS_FLAGS}

  PDMO_VIDEO_OUTPUT_STREAM_FLAGS = ^_DMO_VIDEO_OUTPUT_STREAM_FLAGS;
  _DMO_VIDEO_OUTPUT_STREAM_FLAGS      = (
    DMO_VOSF_NEEDS_PREVIOUS_SAMPLE    = $1);
  {$EXTERNALSYM _DMO_VIDEO_OUTPUT_STREAM_FLAGS}
  DMO_VIDEO_OUTPUT_STREAM_FLAGS = _DMO_VIDEO_OUTPUT_STREAM_FLAGS;
  {$EXTERNALSYM DMO_VIDEO_OUTPUT_STREAM_FLAGS}


type

  // Forward Interfaces Declarations

  PIMediaBuffer = ^IMediaBuffer;
  IMediaBuffer = interface;

  // INTERFACES ////////////////////////////////////////////////////////////////


  PDmoOutputDataBuffer = ^DMO_OUTPUT_DATA_BUFFER;
  _DMO_OUTPUT_DATA_BUFFER = record
    pBuffer: IMediaBuffer;
    dwStatus: DWORD;
    rtTimestamp: REFERENCE_TIME;
    rtTimelength: REFERENCE_TIME;
  end;
  {$EXTERNALSYM _DMO_OUTPUT_DATA_BUFFER}
  DMO_OUTPUT_DATA_BUFFER = _DMO_OUTPUT_DATA_BUFFER;
  {$EXTERNALSYM DMO_OUTPUT_DATA_BUFFER}


  // Interface IMediaBuffer
  // ======================
  // The IMediaBuffer interface provides methods for manipulating a data buffer.
  // Buffers passed to the IMediaObject.ProcessInput and ProcessOutput methods must implement this interface.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMediaBuffer);'}
  {$EXTERNALSYM IMediaBuffer}
  IMediaBuffer = interface(IUnknown)
  ['{59eff8b9-938c-4a26-82f2-95cb84cdc837}']
    function SetLength(cbLength: DWORD): HResult; stdcall;

    function GetMaxLength(out pcbMaxLength: DWORD): HResult; stdcall;

    function GetBufferAndLength(out ppBuffer: PByte;
                                out pcbLength: DWORD): HResult; stdcall;

  end;
  IID_IMediaBuffer = IMediaBuffer;
  {$EXTERNALSYM IID_IMediaBuffer}


  // Interface IMediaObject
  // ======================
  // The IMediaObject interface provides methods for manipulating a Microsoft DirectX Media Object (DMO).
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMediaObject);'}
  {$EXTERNALSYM IMediaObject}
  IMediaObject = interface(IUnknown)
  ['{d8ad0f58-5494-4102-97c5-ec798e59bcf4}']
    function GetStreamCount(out pcInputStreams: DWORD;
                            out pcOutputStreams: DWORD): HResult; stdcall;

    function GetInputStreamInfo(const dwInputStreamIndex: DWORD;
                                out pdwFlags: DWORD): HResult; stdcall;

    function GetOutputStreamInfo(const dwOutputStreamIndex: DWORD;
                                 out pdwFlags: DWORD): HResult; stdcall;

    function GetInputType(const dwInputStreamIndex: DWORD;
                          const dwTypeIndex: DWORD;
                          out pmt: DMO_MEDIA_TYPE): HResult; stdcall;

    function GetOutputType(const dwOutputStreamIndex: DWORD;
                           const dwTypeIndex: DWORD;
                           out pmt: DMO_MEDIA_TYPE): HResult; stdcall;

    function SetInputType(dwInputStreamIndex: DWORD;
                          pmt: DMO_MEDIA_TYPE;
                          dwFlags: DWORD): HResult; stdcall;

    function SetOutputType(dwOutputStreamIndex: DWORD;
                           pmt: DMO_MEDIA_TYPE;
                           dwFlags: DWORD): HResult; stdcall;

    function GetInputCurrentType(dwInputStreamIndex: DWORD;
                                 out pmt: DMO_MEDIA_TYPE): HResult; stdcall;

    function GetOutputCurrentType(dwOutputStreamIndex: DWORD;
                                  out pmt: DMO_MEDIA_TYPE): HResult; stdcall;

    function GetInputSizeInfo(dwInputStreamIndex: DWORD;
                              out pcbSize: DWORD;
                              out pcbMaxLookahead: DWORD;
                              out pcbAlignment: DWORD): HResult; stdcall;

    function GetOutputSizeInfo(dwOutputStreamIndex: DWORD;
                               out pcbSize: DWORD;
                               out pcbAlignment: DWORD): HResult; stdcall;

    function GetInputMaxLatency(dwInputStreamIndex: DWORD;
                                out prtMaxLatency: REFERENCE_TIME): HResult; stdcall;

    function SetInputMaxLatency(dwInputStreamIndex: DWORD;
                                const rtMaxLatency: REFERENCE_TIME): HResult; stdcall;

    function Flush(): HResult; stdcall;

    function Discontinuity(dwInputStreamIndex: DWORD): HResult; stdcall;

    function AllocateStreamingResources(): HResult; stdcall;

    function FreeStreamingResources(): HResult; stdcall;

    function GetInputStatus(dwInputStreamIndex: DWORD;
                            out dwFlags: DWORD): HResult; stdcall;

    function ProcessInput(dwInputStreamIndex: DWORD;
                          pBuffer: IMediaBuffer;
                          dwFlags: DWORD;
                          rtTimestamp: REFERENCE_TIME;
                          rtTimelength: REFERENCE_TIME): HResult; stdcall;

    function ProcessOutput(dwFlags: DWORD;
                           cOutputBufferCount: DWORD;
                           var pOutputBuffers: DMO_OUTPUT_DATA_BUFFER;
                           out pdwStatus: DWORD): HResult; stdcall;

    function Lock(const bLock: LONG): HResult; stdcall;

  end;
  IID_IMediaObject = IMediaObject;
  {$EXTERNALSYM IID_IMediaObject}


  // Interface IEnumDMO
  // ==================
  // The IEnumDMO interface provides methods for enumerating Microsoft DirectX Media Objects (DMOs).
  // It is based on the OLE enumeration interfaces.
  // For more information, see the IEnumXXXX topic in the Platform SDK.
  // To enumerate registered DMOs, call the DMOEnum function.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IEnumDMO);'}
  {$EXTERNALSYM IEnumDMO}
  IEnumDMO = interface(IUnknown)
  ['{2c3cd98a-2bfa-4a53-9c27-5249ba64ba0f}']
    function Next(cItemsToFetch: DWORD;
                  out pCLSID: CLSID;
                  out Names: PWideChar;
                  out pcItemsFetched: DWORD): HResult; stdcall;

    function Skip(cItemsToSkip: DWORD): HResult; stdcall;

    function Reset(): HResult; stdcall;

    function Clone(out ppEnum: IEnumDMO): HResult; stdcall;

  end;
  IID_IEnumDMO = IEnumDMO;
  {$EXTERNALSYM IID_IEnumDMO}



  // Interface IMediaObjectInPlace
  // =============================
  // The IMediaObjectInPlace interface provides methods for processing data in place.
  // A Microsoft DirectX Media Object (DMO) can expose this interface if it meets the following conditions:
  //   - It has one input stream and one output stream.
  //   - Both streams use the same media type.
  //   - The output is produced in place on the buffer; that is, without copying data.
  // This interface provides an optimized way to process data.
  // The application calls a single IMediaObjectInPlace.Process method instead of the IMediaObject::ProcessInput and
  // IMediaObject.ProcessOutput methods.
  // However, any DMO that implements this interface must also implement the IMediaObject interface.
  // Therefore, an application is never obligated to use this interface, and a DMO is never guaranteed to implement it.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMediaObjectInPlace);'}
  {$EXTERNALSYM IMediaObjectInPlace}
  IMediaObjectInPlace = interface(IUnknown)
  ['{651b9ad0-0fc7-4aa9-9538-d89931010741}']
    function Process(ulSize: ULONG;
                     var pData: PByte;
                     refTimeStart: REFERENCE_TIME;
                     dwFlags: DWORD): HResult; stdcall;

    function Clone(out ppMediaObject: IMediaObjectInPlace): HResult; stdcall;

    function GetLatency(out pLatencyTime: REFERENCE_TIME): HResult; stdcall;

  end;
  IID_IMediaObjectInPlace = IMediaObjectInPlace;
  {$EXTERNALSYM IID_IMediaObjectInPlace}


  // Interface IDMOQualityControl
  // ============================
  // The IDMOQualityControl interface supports quality control on a Microsoft DirectX Media Object (DMO).
  // A DMO exposes this interface if it can respond to late samples.
  // When quality control is enabled, the DMO attempts to process samples on time,
  // discarding late samples if necessary.
  // When quality control is disabled, the DMO processes every sample. By default, quality control is disabled.
  // Applications use this interface to enable or disable quality control.
  // Using quality control is appropriate when you are viewing media data in real time.
  // If you are capturing data to a file, do not enable quality control, because the DMO might discard samples.
  // It does not matter in file capture whether samples arrive late, and you do not want to lose the data.
  // COMMENT:
  //  To use quality control, perform the following steps:
  //   Call the IDMOQualityControl.SetNow method with the reference time of the earliest sample to be processed.
  //   Call the IDMOQualityControl.SetStatus method with the DMO_QUALITY_STATUS_ENABLED flag.
  //   To disable quality control, call SetStatus with no flag.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDMOQualityControl);'}
  {$EXTERNALSYM IDMOQualityControl}
  IDMOQualityControl = interface(IUnknown)
  ['{65abea96-cf36-453f-af8a-705e98f16260}']
    function SetNow(rtNow: REFERENCE_TIME): HResult; stdcall;

    function SetStatus(dwFlags: DWORD): HResult; stdcall;

    function GetStatus(out pdwFlags: DWORD): HResult; stdcall;

  end;
  IID_IDMOQualityControl = IDMOQualityControl;
  {$EXTERNALSYM IID_IDMOQualityControl}


  // Interface IDMOVideoOutputOptimizations
  // ======================================
  // The IDMOVideoOutputOptimizations interface supports video optimizations on a Microsoft DirectX Media Object (DMO).
  // NOTE: This interface enables an application to negotiate with a DMO about video output optimizations.
  //       A DMO exposes this interface when it can perform optimizations that require support from the application.
  //       The application can query the DMO for its preferred features, and then agree (or not agree) to provide them.
  //       The DMO must process output even if the application rejects the optimizations.
  //
  // For example, a video decoder might generate an output frame by applying deltas to the previous output frame.
  // When queried, it requests that the application supply the previous frame in the output buffer.
  // The application can agree to this request or not.
  //
  // Video optimizations are negotiated separately for each output stream.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDMOVideoOutputOptimizations);'}
  {$EXTERNALSYM IDMOVideoOutputOptimizations}
  IDMOVideoOutputOptimizations = interface(IUnknown)
  ['{be8f4f4e-5b16-4d29-b350-7f6b5d9298ac}']
    function QueryOperationModePreferences(ulOutputStreamIndex: ULONG;
                                           pdwRequestedCapabilities: DWORD): HResult; stdcall;

    function SetOperationMode(ulOutputStreamIndex: ULONG;
                              dwEnabledFeatures: DWORD): HResult; stdcall;

    function GetCurrentOperationMode(ulOutputStreamIndex: ULONG;
                                     var pdwEnabledFeatures: DWORD): HResult; stdcall;

    function GetCurrentSampleRequirements(ulOutputStreamIndex: ULONG;
                                          var pdwRequestedFeatures: DWORD): HResult; stdcall;
  end;
  IID_IDMOVideoOutputOptimizations = IDMOVideoOutputOptimizations;
  {$EXTERNALSYM IID_IDMOVideoOutputOptimizations}



  // Additional Prototypes for ALL interfaces

  // End of Additional Prototypes

implementation

  // Implement Additional Prototypes here.

end.
