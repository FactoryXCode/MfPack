// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - MediaFoundation
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.MediaFoundationApi.MfReadWrite.pas
// Kind: Pascal Unit
// Release date: 27-06-2012
// Language: ENU
//
// Revision Version: 3.0.1
// Description: -
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX), Peter Larson (ozships), (Ciaran), (topPlay)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 13/08/2020 All                 Enigma release. New layout and namespaces
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 7 or later (See: Remarks).
//
//                   Delphi : The IUnknown entries of functions should be casted like this:
//                   IUnknown(Pointer), IUnknown(Object), IUnknown(Nil) etc.
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
// Source: mfreadwrite.h
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
unit WinApi.MediaFoundationApi.MfReadWrite;

  {$HPPEMIT '#include "mfreadwrite.h"'}

interface

uses

  {WinApi}
  WinApi.WinApiTypes,
  {ActiveX}
  WinApi.ActiveX.PropIdl,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfObjects,
  WinApi.MediaFoundationApi.MfIdl,
  WinApi.MediaFoundationApi.MfTransform;


  {$WEAKPACKAGEUNIT ON}
  {$MINENUMSIZE 4}

  {$IFDEF WIN32}
    {$ALIGN 1}
  {$ELSE}
    {$ALIGN 8} // Win64
  {$ENDIF}

  {$I 'WinApiTypes.inc'}


const

  // Interface IMFSourceReader
  MF_SOURCE_READER_ASYNC_CALLBACK                       : TGUID = '{1e3dbeac-bb43-4c35-b507-cd644464c965}';
  {$EXTERNALSYM MF_SOURCE_READER_ASYNC_CALLBACK}
  MF_SOURCE_READER_D3D_MANAGER                          : TGUID = '{ec822da2-e1e9-4b29-a0d8-563c719f5269}';
  {$EXTERNALSYM MF_SOURCE_READER_D3D_MANAGER}
  MF_SOURCE_READER_DISABLE_DXVA                         : TGUID = '{aa456cfd-3943-4a1e-a77d-1838c0ea2e35}';
  {$EXTERNALSYM MF_SOURCE_READER_DISABLE_DXVA}
  MF_SOURCE_READER_MEDIASOURCE_CONFIG                   : TGUID = '{9085abeb-0354-48f9-abb5-200df838c68e}';
  {$EXTERNALSYM MF_SOURCE_READER_MEDIASOURCE_CONFIG}
  MF_SOURCE_READER_MEDIASOURCE_CHARACTERISTICS          : TGUID = '{6d23f5c8-c5d7-4a9b-9971-5d11f8bca880}';
  {$EXTERNALSYM MF_SOURCE_READER_MEDIASOURCE_CHARACTERISTICS}
  MF_SOURCE_READER_ENABLE_VIDEO_PROCESSING              : TGUID = '{fb394f3d-ccf1-42ee-bbb3-f9b845d5681d}';
  {$EXTERNALSYM MF_SOURCE_READER_ENABLE_VIDEO_PROCESSING}
//#if (WINVER >= _WIN32_WINNT_WIN8)
  MF_SOURCE_READER_ENABLE_ADVANCED_VIDEO_PROCESSING     : TGUID = '{0f81da2c-b537-4672-a8b2-a681b17307a3}';
  {$EXTERNALSYM MF_SOURCE_READER_ENABLE_ADVANCED_VIDEO_PROCESSING}
  MF_SOURCE_READER_DISABLE_CAMERA_PLUGINS               : TGUID = '{9d3365dd-058f-4cfb-9f97-b314cc99c8ad}';
  {$EXTERNALSYM MF_SOURCE_READER_DISABLE_CAMERA_PLUGINS}
//#endif (WINVER >= _WIN32_WINNT_WIN8)
  MF_SOURCE_READER_DISCONNECT_MEDIASOURCE_ON_SHUTDOWN   : TGUID = '{56b67165-219e-456d-a22e-2d3004c7fe56}';
  {$EXTERNALSYM MF_SOURCE_READER_DISCONNECT_MEDIASOURCE_ON_SHUTDOWN}
  MF_SOURCE_READER_ENABLE_TRANSCODE_ONLY_TRANSFORMS     : TGUID = '{dfd4f008-b5fd-4e78-ae44-62a1e67bbe27}';
  {$EXTERNALSYM MF_SOURCE_READER_ENABLE_TRANSCODE_ONLY_TRANSFORMS}
  MF_SOURCE_READER_D3D11_BIND_FLAGS                     : TGUID = '{33f3197b-f73a-4e14-8d85-0e4c4368788d}';
  {$EXTERNALSYM MF_SOURCE_READER_D3D11_BIND_FLAGS}


{$Z4}    // Interface IMFSourceReader

  MF_SOURCE_READER_INVALID_STREAM_INDEX  = MAXDW; // $ffffffff;
  {$EXTERNALSYM MF_SOURCE_READER_INVALID_STREAM_INDEX}
  MF_SOURCE_READER_ALL_STREAMS           = DWord($fffffffe);
  {$EXTERNALSYM MF_SOURCE_READER_ALL_STREAMS}
  MF_SOURCE_READER_ANY_STREAM            = DWord($fffffffe);
  {$EXTERNALSYM MF_SOURCE_READER_ANY_STREAM}
  MF_SOURCE_READER_FIRST_AUDIO_STREAM    = DWord($fffffffd);
  {$EXTERNALSYM MF_SOURCE_READER_FIRST_AUDIO_STREAM}
  MF_SOURCE_READER_FIRST_VIDEO_STREAM    = DWord($fffffffc);
  {$EXTERNALSYM MF_SOURCE_READER_FIRST_VIDEO_STREAM}
  MF_SOURCE_READER_MEDIASOURCE           = MAXDW; // $ffffffff;
  {$EXTERNALSYM MF_SOURCE_READER_MEDIASOURCE}

//#if (WINVER >= _WIN32_WINNT_WIN8)
   MF_SOURCE_READER_CURRENT_TYPE_INDEX	 = MAXDW; // 0xffffffff
   {$EXTERNALSYM MF_SOURCE_READER_CURRENT_TYPE_INDEX}
//#endif // (WINVER >= _WIN32_WINNT_WIN8)

  // Interface IMFSinkWriter
  CLSID_MFSinkWriter                                    : TGUID = '{a3bbfb17-8273-4e52-9e0e-9739dc887990}';
  {$EXTERNALSYM CLSID_MFSinkWriter}

  MF_SINK_WRITER_ASYNC_CALLBACK                         : TGUID = '{48cb183e-7b0b-46f4-822e-5e1d2dda4354}';
  {$EXTERNALSYM MF_SINK_WRITER_ASYNC_CALLBACK}
  MF_SINK_WRITER_DISABLE_THROTTLING                     : TGUID = '{08b845d8-2b74-4afe-9d53-be16d2d5ae4f}';
  {$EXTERNALSYM MF_SINK_WRITER_DISABLE_THROTTLING}

  MF_SINK_WRITER_INVALID_STREAM_INDEX  = MAXDW; // 0xffffffff
  {$EXTERNALSYM MF_SINK_WRITER_INVALID_STREAM_INDEX}
  MF_SINK_WRITER_ALL_STREAMS           = DWord($fffffffe);
  {$EXTERNALSYM MF_SINK_WRITER_ALL_STREAMS}
  MF_SINK_WRITER_MEDIASINK             = MAXDW; // 0xffffffff
  {$EXTERNALSYM MF_SINK_WRITER_MEDIASINK}

  // Interface IMFSourceReader
  MF_READWRITE_DISABLE_CONVERTERS                       : TGUID = '{98d5b065-1374-4847-8d5d-31520fee7156}';
  {$EXTERNALSYM MF_READWRITE_DISABLE_CONVERTERS}
  MF_READWRITE_ENABLE_HARDWARE_TRANSFORMS               : TGUID = '{a634a91c-822b-41b9-a494-4de4643612b0}';
  {$EXTERNALSYM MF_READWRITE_ENABLE_HARDWARE_TRANSFORMS}

// #if (WINVER >= _WIN32_WINNT_WIN8)
  MF_READWRITE_MMCSS_CLASS                              : TGUID = '{39384300-d0eb-40b1-87a0-3318871b5a53}';
  {$EXTERNALSYM MF_READWRITE_MMCSS_CLASS}
  MF_READWRITE_MMCSS_PRIORITY                           : TGUID = '{43ad19ce-f33f-4ba9-a580-e4cd12f2d144}';
  {$EXTERNALSYM MF_READWRITE_MMCSS_PRIORITY}
  MF_READWRITE_MMCSS_CLASS_AUDIO                        : TGUID = '{430847da-0890-4b0e-938c-054332c547e1}';
  {$EXTERNALSYM MF_READWRITE_MMCSS_CLASS_AUDIO}
  MF_READWRITE_MMCSS_PRIORITY_AUDIO                     : TGUID = '{273db885-2de2-4db2-a6a7-fdb66fb40b61}';
  {$EXTERNALSYM MF_READWRITE_MMCSS_PRIORITY_AUDIO}
  MF_READWRITE_D3D_OPTIONAL                             : TGUID = '{216479d9-3071-42ca-bb6c-4c22102e1d18}';
  {$EXTERNALSYM MF_READWRITE_D3D_OPTIONAL}
  MF_MEDIASINK_AUTOFINALIZE_SUPPORTED                   : TGUID = '{48c131be-135a-41cb-8290-03652509c999}';
  {$EXTERNALSYM MF_MEDIASINK_AUTOFINALIZE_SUPPORTED}
  MF_MEDIASINK_ENABLE_AUTOFINALIZE                      : TGUID = '{34014265-cb7e-4cde-ac7c-effd3b3c2530}';
  {$EXTERNALSYM MF_MEDIASINK_ENABLE_AUTOFINALIZE}
  MF_READWRITE_ENABLE_AUTOFINALIZE                      : TGUID = '{dd7ca129-8cd1-4dc5-9dde-ce168675de61}';
  {$EXTERNALSYM MF_READWRITE_ENABLE_AUTOFINALIZE}
//#endif // (WINVER >= _WIN32_WINNT_WIN8)


type
  PMF_SOURCE_READER_FLAG = ^MF_SOURCE_READER_FLAG;
  MF_SOURCE_READER_FLAG = DWord;
  {$EXTERNALSYM MF_SOURCE_READER_FLAG}
const
  // Specifies that an error has occurred while processing sample
  // requests.  If this is set, then no other calls should be made
  // on the source reader besides shutting it down.
  MF_SOURCE_READERF_ERROR                   = MF_SOURCE_READER_FLAG($00000001);
  {$EXTERNALSYM MF_SOURCE_READERF_ERROR}

  // Specifies that the stream has ended.
  MF_SOURCE_READERF_ENDOFSTREAM             = MF_SOURCE_READER_FLAG($00000002);
  {$EXTERNALSYM MF_SOURCE_READERF_ENDOFSTREAM}

  // Specifies that one or more new streams have been created.
  // The application can modify stream selection and configure
  // output media types for the new streams.
  MF_SOURCE_READERF_NEWSTREAM               = MF_SOURCE_READER_FLAG($00000004);
  {$EXTERNALSYM MF_SOURCE_READERF_NEWSTREAM}

  // Specifies that the native media type for the stream has changed.
  MF_SOURCE_READERF_NATIVEMEDIATYPECHANGED  = MF_SOURCE_READER_FLAG($00000010);
  {$EXTERNALSYM MF_SOURCE_READERF_NATIVEMEDIATYPECHANGED}

  // Specifies that the current media type for the stream has changed.
  MF_SOURCE_READERF_CURRENTMEDIATYPECHANGED = MF_SOURCE_READER_FLAG($00000020);
  {$EXTERNALSYM MF_SOURCE_READERF_CURRENTMEDIATYPECHANGED}

  // Specifies that there is a gap in the stream.
  MF_SOURCE_READERF_STREAMTICK              = MF_SOURCE_READER_FLAG($00000100);
  {$EXTERNALSYM MF_SOURCE_READERF_STREAMTICK}

  // Indicates that all transforms inserted by the application have been
  // removed for a particular stream. This could be due to a dynamic format
  // change from a source or decoder that prevents custom transforms from
  // being used because they cannot handle the new media type.
  MF_SOURCE_READERF_ALLEFFECTSREMOVED       = MF_SOURCE_READER_FLAG($00000200);
  {$EXTERNALSYM MF_SOURCE_READERF_ALLEFFECTSREMOVED}

type
  PMF_SOURCE_READER_CONTROL_FLAG  = ^cwMF_SOURCE_READER_CONTROL_FLAG;
  cwMF_SOURCE_READER_CONTROL_FLAG = DWord;
  {$EXTERNALSYM cwMF_SOURCE_READER_CONTROL_FLAG}
  MF_SOURCE_READER_CONTROL_FLAG   = cwMF_SOURCE_READER_CONTROL_FLAG;
  {$EXTERNALSYM MF_SOURCE_READER_CONTROL_FLAG}
const
  MF_SOURCE_READER_CONTROLF_DRAIN = MF_SOURCE_READER_CONTROL_FLAG($00000001);
  {$EXTERNALSYM MF_SOURCE_READER_CONTROLF_DRAIN}

  //DEFINE_ENUM_FLAG_OPERATORS(MF_SOURCE_READER_CONTROL_FLAG)

type

  PMF_SINK_WRITER_STATISTICS = ^MF_SINK_WRITER_STATISTICS;
  _MF_SINK_WRITER_STATISTICS = record
    cb: DWORD;
    llLastTimestampReceived: LONGLONG;
    llLastTimestampEncoded: LONGLONG;
    llLastTimestampProcessed: LONGLONG;
    llLastStreamTickReceived: LONGLONG;
    llLastSinkSampleRequest: LONGLONG;
    qwNumSamplesReceived: QWORD;
    qwNumSamplesEncoded: QWORD;
    qwNumSamplesProcessed: QWORD;
    qwNumStreamTicksReceived: QWORD;
    dwByteCountQueued: DWORD;
    qwByteCountProcessed: QWORD;
    dwNumOutstandingSinkSampleRequests: DWORD;
    dwAverageSampleRateReceived: DWORD;
    dwAverageSampleRateEncoded: DWORD;
    dwAverageSampleRateProcessed: DWORD;
  end;
  {$EXTERNALSYM _MF_SINK_WRITER_STATISTICS}
  MF_SINK_WRITER_STATISTICS = _MF_SINK_WRITER_STATISTICS;
  {$EXTERNALSYM MF_SINK_WRITER_STATISTICS}

const

  CLSID_MFReadWriteClassFactory : TGUID = '{48e2ed0f-98c2-4a37-bed5-166312ddd83f}';
  {$EXTERNALSYM CLSID_MFReadWriteClassFactory}
  CLSID_MFSourceReader          : TGUID = '{1777133c-0881-411b-a577-ad545f0714c4}';
  {$EXTERNALSYM CLSID_MFSourceReader}

type

  // INTERFACES

  // Interface IMFReadWriteClassFactory
  // ==================================
  {
   Creates an instance of either the sink writer or the source reader.
   NOTE:  To get a pointer to this interface, call the CoCreateInstance function.
          The CLSID is CLSID_MFReadWriteClassFactory.
          Call the MFStartup function before using the interface.

          As an alternative of using this interface, you can call any of the following functions:
          MFCreateSinkWriterFromMediaSink
          MFCreateSinkWriterFromURL
          MFCreateSourceReaderFromByteStream
          MFCreateSourceReaderFromMediaSource
          MFCreateSourceReaderFromURL
          Internally, these functions use the IMFReadWriteClassFactory interface.

          This interface is available on Windows Vista if Platform Update
          Supplement for Windows Vista is installed.
  }
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFReadWriteClassFactory);'}
  {$EXTERNALSYM IMFReadWriteClassFactory}
  IMFReadWriteClassFactory = interface(IUnknown)
  ['{E7FE2E12-661C-40DA-92F9-4F002AB67627}']

    function CreateInstanceFromURL(const clsid: REFCLSID;
                                   pwszURL: LPCWSTR;
                                   pAttributes: IMFAttributes;
                                   const riid: REFIID;
                                   out ppvObject): HResult; stdcall;

    function CreateInstanceFromObject(const clsid: REFCLSID;
                                      punkObject: IUnknown;
                                      pAttributes: IMFAttributes;
                                      const riid: REFIID;
                                      out ppvObject): HResult; stdcall;

  end;
  IID_IMFReadWriteClassFactory = IMFReadWriteClassFactory;
  {$EXTERNALSYM IID_IMFReadWriteClassFactory}


  // Interface IMFSourceReader
  // =========================
  //
  // Implemented by the Microsoft Media Foundation source reader object.
  // To create the source reader, call one of the following functions:
  //  MFCreateSourceReaderFromByteStream
  //  MFCreateSourceReaderFromMediaSource
  //  MFCreateSourceReaderFromURL
  //
  // Alternatively, use the IMFReadWriteClassFactory interface.
  //
  // This interface is available on Windows Vista if Platform Update Supplement
  // for Windows Vista is installed.
  //
  // The MF Source Reader provides a simple programming model that allows
  // applications to easily access multimedia content from files or devices.
  //
  // NOTE:  THIS, AS IS IN HEADERSEQUENCE DIFFERS FROM MSDN!
  //
  PIMFSourceReader = ^IMFSourceReader;
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFSourceReader);'}
  {$EXTERNALSYM IMFSourceReader}
  IMFSourceReader = interface(IUnknown)
  ['{70ae66f2-c809-4e4f-8915-bdcb406b7993}']

    function GetStreamSelection(dwStreamIndex: DWORD;
                                out pfSelected: Boolean): HResult; stdcall;

    function SetStreamSelection(dwStreamIndex: DWORD;
                                const fSelected: Boolean): HResult; stdcall;

    function GetNativeMediaType(dwStreamIndex: DWORD;
                                dwMediaTypeIndex: DWORD;
                                out ppMediaType: IMFMediaType): HResult; stdcall;

    function GetCurrentMediaType(dwStreamIndex: DWORD;
                                 out ppMediaType: IMFMediaType): HResult; stdcall;

    function SetCurrentMediaType(dwStreamIndex: DWORD;
                      {Reserved} pdwReserved: DWORD;
                                 pMediaType: IMFMediaType): HResult; stdcall;

    function SetCurrentPosition(const guidTimeFormat: TGUID;
                                const varPosition: PROPVARIANT): HResult; stdcall;

    function ReadSample(dwStreamIndex: DWORD;   // The stream to pull data from.
                        dwControlFlags: DWORD;  // A bitwise OR of zero or more flags from the MF_SOURCE_READER_CONTROL_FLAG enumeration.
         {out optional} pdwActualStreamIndex: PDWORD = Nil;  // Receives the zero-based index of the stream.
         {out optional} pdwStreamFlags: PDWORD = Nil;        // Receives a bitwise OR of zero or more flags from the MF_SOURCE_READER_FLAG enumeration.
         {out optional} pllTimestamp: PLONGLONG = Nil;       // Receives the time stamp of the sample, or the time of the stream event indicated in pdwStreamFlags. The time is given in 100-nanosecond units.
         {out optional} ppSample: PIMFSample = Nil): HResult; stdcall;

    // function ReadSample: Remarks
    // ============================
    // If the requested stream is not selected,
    // the return code is MF_E_INVALIDREQUEST. See IMFSourceReader.SetStreamSelection.
    //
    // This method can complete synchronously or asynchronously.
    // If you provide a callback pointer when you create the source reader,
    // the method is asynchronous.
    // Otherwise, the method is synchronous.
    // For more information about setting the callback pointer, see MF_SOURCE_READER_ASYNC_CALLBACK.
    //
    // Asynchronous Mode
    // -----------------
    // In asynchronous mode:
    // - All of the [out] parameters must be Nil. Otherwise, the method returns E_INVALIDARG.
    // - The method returns immediately.
    // - When the operation completes, the application's IMFSourceReaderCallback.OnReadSample method is called.
    // - If an error occurs, the method can fail either synchronously or asynchronously.
    //   Check the return value of ReadSample, and also check the hrStatus parameter of IMFSourceReaderCallback.OnReadSample.
    //
    // Synchronous Mode
    // ----------------
    // In synchronous mode:
    // - The pdwStreamFlags and ppSample parameters cannot be Nil.
    //   Otherwise, the method returns E_POINTER.
    // - The pdwActualStreamIndex and pllTimestamp parameters can be Nil.
    // - The method blocks until the next sample is available.
    // - In synchronous mode, if the dwStreamIndex parameter is MF_SOURCE_READER_ANY_STREAM,
    //   you should pass a non-Nil value for pdwActualStreamIndex, so that you know which stream delivered the sample.
    // - This method can return flags in the pdwStreamFlags parameter without returning a media sample in ppSample.
    //   Therefore, the ppSample parameter can receive a Nil pointer even when the method succeeds.
    //   For example, when the source reader reaches the end of the stream,
    //   it returns the MF_SOURCE_READERF_ENDOFSTREAM flag in pdwStreamFlags and sets ppSample to Nil.
    //
    // If there is a gap in the stream, pdwStreamFlags receives the MF_SOURCE_READERF_STREAMTICK flag,
    // ppSample is Nil, and pllTimestamp indicates the time when the gap occurred.


    function Flush(dwStreamIndex: DWord): HResult; stdcall;

    function GetServiceForStream(dwStreamIndex: DWORD;
                                 const guidService: REFGUID;
                                 const riid: REFIID;
                                 out ppvObject): HResult; stdcall;

    function GetPresentationAttribute(const dwStreamIndex: DWORD;
                                      const guidAttribute: REFGUID;
                                      var pvarAttribute: PROPVARIANT): HResult; stdcall;

  end;
  IID_IMFSourceReader = IMFSourceReader;
  {$EXTERNALSYM IID_IMFSourceReader}


  // IMFSourceReaderEx
  // =================
  // Extends the IMFSourceReader interface.
  // The Source Reader implements this interface in Windows 8 and higher.
  // To get a pointer to this interface, call QueryInterface on the Source Reader.
  //
  PIMFSourceReaderEx = ^IMFSourceReaderEx;
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFSourceReaderEx);'}
  {$EXTERNALSYM IMFSourceReaderEx}
  IMFSourceReaderEx = interface(IMFSourceReader)
  ['{7b981cf0-560e-4116-9875-b099895f23d7}']

    function SetNativeMediaType(dwStreamIndex: DWORD;
                                pMediaType: IMFMediaType;
                                out pdwStreamFlags: DWORD): HResult; stdcall;

    function AddTransformForStream(dwStreamIndex: DWORD;
                                   pTransformOrActivate: IUnknown): HResult; stdcall;

    function RemoveAllTransformsForStream(dwStreamIndex: DWORD): HResult; stdcall;

    function GetTransformForStream(dwStreamIndex: DWORD;
                                   dwTransformIndex: DWORD;
                                   var pGuidCategory: TGUID;
                                   out ppTransform: IMFTransform): HResult; stdcall;

  end;
  IID_IMFSourceReaderEx = IMFSourceReaderEx;
  {$EXTERNALSYM IID_IMFSourceReaderEx}


  // Interface IMFSourceReaderCallback
  // =================================
  // This interface is used as the callback mechanism for when the
  // MF Source Reader is used in asynchronous mode. The application
  // passes in an instance of an object that implements this
  // callback interface as an attribute when creating the source
  // reader instance.
  //
  PIMFSourceReaderCallback = ^IMFSourceReaderCallback;
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFSourceReaderCallback);'}
  {$EXTERNALSYM IMFSourceReaderCallback}
  IMFSourceReaderCallback = interface(IUnknown)
  ['{deec8d99-fa1d-4d82-84c2-2c8969944867}']

    function OnReadSample(hrStatus: HRESULT;     // Specifies the error code if an error occurred while processing the sample request.
                          dwStreamIndex: DWORD;  // Specifies the stream index for the sample.
                          dwStreamFlags: DWORD;  // Specifies the accumulated flags for the stream.
                          llTimestamp: LONGLONG; // Contains the presentation time of the sample.
                                                 // If MF_SOURCE_READERF_STREAM_TICK is set for the stream flags,
                                                 // then this contains the timestamp for the stream tick.
                          pSample: IMFSample): HResult; stdcall; // Contains the next sample for the stream. It is possible for
                                                                 // this parameter to be Nil, so the application should
                                                                 // explicitly check for Nil before dereferencing the sample.

    function OnFlush(dwStreamIndex: DWORD): HResult; stdcall;

    function OnEvent(dwStreamIndex: DWORD;
                     pEvent: IMFMediaEvent): HResult; stdcall;

  end;
  IID_IMFSourceReaderCallback = IMFSourceReaderCallback;
  {$EXTERNALSYM IID_IMFSourceReaderCallback}


//#if (_WIN32_WINNT >= _WIN32_WINNT_WINTHRESHOLD)

  // Interface IMFSourceReaderCallback2
  // ==================================
  //
  // This interface extends IMFSourceReaderCallback and is used as the callback mechanism
  // for asynchronously notifying the caller when the transform chain changes, and
  // when an MFT in the chain raises an error event.
  //
  PIMFSourceReaderCallback2 = ^IMFSourceReaderCallback2;
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFSourceReaderCallback2);'}
  {$EXTERNALSYM IMFSourceReaderCallback2}
  IMFSourceReaderCallback2 = interface(IMFSourceReaderCallback)
  ['{CF839FE6-8C2A-4DD2-B6EA-C22D6961AF05}']

    // Callback function that is triggered when the transform chain in the SourceReader is built or
    // modified.
    function OnTransformChange(): HResult; stdcall;

    // - dwStreamIndex specifies the stream of the transform that raised the async error
    // - hrStatus specifies the async error raised by the transform
    function OnStreamError(dwStreamIndex: DWORD;
                           hrStatus: HResult): HResult; stdcall;

  end;
  IID_IMFSourceReaderCallback2 = IMFSourceReaderCallback2;
  {$EXTERNALSYM IID_IMFSourceReaderCallback2}


  // Interface IMFSinkWriter
  // =======================
  //
  // Implemented by the Microsoft Media Foundation sink writer object.
  // NOTE:  To create the sink writer, call one of the following functions:
  //        MFCreateSinkWriterFromMediaSink
  //        MFCreateSinkWriterFromURL
  //
  //        Alternatively, use the IMFReadWriteClassFactory interface.
  //
  //        This interface is available on Windows Vista if Platform Update
  //        Supplement for Windows Vista is installed.
  //
  PIMFSinkWriter = ^IMFSinkWriter;
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFSinkWriter);'}
  {$EXTERNALSYM IMFSinkWriter}
  IMFSinkWriter = interface(IUnknown)
  ['{3137f1cd-fe5e-4805-a5d8-fb477448cb3d}']

    function AddStream(pTargetMediaType: IMFMediaType;
                       out pdwStreamIndex: DWord): HResult; stdcall;

    function SetInputMediaType(dwStreamIndex: DWord;
                               pInputMediaType: IMFMediaType;
                               pEncodingParameters: IMFAttributes): HResult; stdcall;

    function BeginWriting(): HResult; stdcall;

    function WriteSample(dwStreamIndex: DWord;
                         pSample: IMFSample): HResult; stdcall;

    function SendStreamTick(dwStreamIndex: DWord;
                            llTimestamp: LONGLONG): HResult; stdcall;

    function PlaceMarker(dwStreamIndex: DWord;
                         pvContex: Pointer): HResult; stdcall;

    function NotifyEndOfSegment(dwStreamIndex: DWord): HResult; stdcall;

    function Flush(dwStreamIndex: DWord): HResult; stdcall;

    function Finalize(): HResult; stdcall;

    function GetServiceForStream(dwStreamIndex: DWord;
                                 const guidService: REFGUID;
                                 const riid: REFIID;
                                 out ppvObject): HResult; stdcall;

    function GetStatistics(dwStreamIndex: DWord;
                           out pStats: MF_SINK_WRITER_STATISTICS): HResult; stdcall;
  end;
  IID_IMFSinkWriter = IMFSinkWriter;
  {$EXTERNALSYM IID_IMFSinkWriter}


  // Interface IMFSinkWriterEx
  // =========================
  //
  PIMFSinkWriterEx = ^IMFSinkWriterEx;
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFSinkWriterEx);'}
  {$EXTERNALSYM IMFSinkWriterEx}
  IMFSinkWriterEx = interface(IMFSinkWriter)
  ['{588d72ab-5Bc1-496a-8714-b70617141b25}']

    function GetTransformForStream(dwStreamIndex: DWORD;
                                   dwTransformIndex: DWORD;
                                   var pGuidCategory: TGUID;
                                   out ppTransform: IMFTransform): HResult; stdcall;

  end;
  IID_IMFSinkWriterEx = IMFSinkWriterEx;
  {$EXTERNALSYM IID_IMFSinkWriterEx}


  // Interface IMFSinkWriterEncoderConfig
  // ====================================
  //
  PIMFSinkWriterEncoderConfig = ^IMFSinkWriterEncoderConfig;
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFSinkWriterEncoderConfig);'}
  {$EXTERNALSYM IMFSinkWriterEncoderConfig}
  IMFSinkWriterEncoderConfig = interface(IUnknown)
  ['{17C3779E-3CDE-4EDE-8C60-3899F5F53AD6}']

    function SetTargetMediaType(dwStreamIndex: DWORD;
                                var pTargetMediaType: IMFMediaType;
                                var pEncodingParameters: IMFAttributes): HResult; stdcall;

    function PlaceEncodingParameters(dwStreamIndex: DWORD;
                                     var pEncodingParameters: IMFAttributes): HResult; stdcall;

  end;
  IID_IMFSinkWriterEncoderConfig = IMFSinkWriterEncoderConfig;
  {$EXTERNALSYM IID_IMFSinkWriterEncoderConfig}


  // Interface IMFSinkWriterCallback
  // ===============================
  //
  // Callback interface for the Microsoft Media Foundation sink writer.
  // Implement this interface if you use the IMFSinkWriter interface and want to receive asynchronous notifications.
  //
  PIMFSinkWriterCallback = ^IMFSinkWriterCallback;
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFSinkWriterCallback);'}
  {$EXTERNALSYM IMFSinkWriterCallback}
  IMFSinkWriterCallback = interface(IUnknown)
  ['{666f76de-33d2-41b9-a458-29ed0a972c58}']

    function OnFinalize(hrStatus: HRESULT): HResult; stdcall;

    function OnMarker(dwStreamIndex: DWord;
                      pvContext: Pointer): HResult; stdcall;

  end;
  IID_IMFSinkWriterCallback = IMFSinkWriterCallback;
  {$EXTERNALSYM IID_IMFSinkWriterCallback}


  // Interface IMFSinkWriterCallback2
  // ================================
  //
  PIMFSinkWriterCallback2 = ^IMFSinkWriterCallback2;
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMFSinkWriterCallback2);'}
  {$EXTERNALSYM IMFSinkWriterCallback2}
  IMFSinkWriterCallback2 = interface(IMFSinkWriterCallback)
  ['{2456BD58-C067-4513-84FE-8D0C88FFDC61}']

    function OnTransformChange(): HResult; stdcall;

    function OnStreamError(dwStreamIndex: DWORD;
                           hrStatus: HResult): HResult; stdcall;

  end;
  IID_IMFSinkWriterCallback2 = IMFSinkWriterCallback2;
  {$EXTERNALSYM IID_IMFSinkWriterCallback2}


  // CREATE interface functions
  // ==========================
  //

  function MFCreateSourceReaderFromURL(const pwszURL: LPCWSTR;
                                       pAttributes: IMFAttributes;
                                       out ppSourceReader: IMFSourceReader): HResult; stdcall;
  {$EXTERNALSYM MFCreateSourceReaderFromURL}

  function MFCreateSourceReaderFromByteStream(pByteStream: IMFByteStream;
                                              pAttributes: IMFAttributes;
                                              out ppSourceReader: IMFSourceReader): HResult; stdcall;
  {$EXTERNALSYM MFCreateSourceReaderFromByteStream}

  function MFCreateSourceReaderFromMediaSource(pMediaSource: IMFMediaSource;
                                               pAttributes: IMFAttributes;
                                               out ppSourceReader: IMFSourceReader): HResult; stdcall;
  {$EXTERNALSYM MFCreateSourceReaderFromMediaSource}

  function MFCreateSinkWriterFromURL(pwszOutputURL: LPCWSTR;
                                     pByteStream: IMFByteStream;
                                     pAttributes: IMFAttributes;
                                     out ppSinkWriter: IMFSinkWriter): HResult; stdcall;
  {$EXTERNALSYM MFCreateSinkWriterFromURL}

  function MFCreateSinkWriterFromMediaSink(pMediaSink: IMFMediaSink;
                                           pAttributes: IMFAttributes;
                                           out ppSinkWriter: IMFSinkWriter): HResult; stdcall;
  {$EXTERNALSYM MFCreateSinkWriterFromMediaSink}


  // Additional Prototypes for ALL interfaces

  // end of Additional Prototypes

implementation

  // Implement Additional Prototypes here.

const
  MfReadWriteLib = 'mfreadwrite.dll';

{$WARN SYMBOL_PLATFORM OFF}
  function MFCreateSourceReaderFromURL;         external MfReadWriteLib name 'MFCreateSourceReaderFromURL' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFCreateSourceReaderFromByteStream;  external MfReadWriteLib name 'MFCreateSourceReaderFromByteStream' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFCreateSourceReaderFromMediaSource; external MfReadWriteLib name 'MFCreateSourceReaderFromMediaSource' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFCreateSinkWriterFromURL;           external MfReadWriteLib name 'MFCreateSinkWriterFromURL' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFCreateSinkWriterFromMediaSink;     external MfReadWriteLib name 'MFCreateSinkWriterFromMediaSink' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
{$WARN SYMBOL_PLATFORM ON}
end.
