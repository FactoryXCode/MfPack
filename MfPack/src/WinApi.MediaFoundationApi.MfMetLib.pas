// FactoryX
//
// Copyright © FactoryX. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.MediaFoundationApi.MfMetLib.pas
// Kind: Pascal Unit
// Release date: 05-01-2016
// Language: ENU
//
// Revision Version: 3.0.1
// Description: This unit holds basic Media Foundation methods needed to play,
//              record, encode, decode, etc.
//
// Company: FactoryX
// Intiator(s): Tony (maXcomX), Peter (OzShips), Ramyses De Macedo Rodrigues.
// Contributor(s): Tony Kalf (maXcomX),
//                 Peter Larson (ozships),
//                 Ramyses De Macedo Rodrigues,
//                 (TopPlay)
//
// -----------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 13/08/2020 All                 Enigma release. New layout and namespaces
// 10/12/2020                     Compatibility update.
// 18/01/2021 Tony                Corrected some pointer issues.
// -----------------------------------------------------------------------------
//
// Remarks: Requires Windows Vista or later.
//          Up to version 262 this unit was named MfMethods.pas and part of MfAdditional
//          On version 263 this unit was named MfpMetLib.pas and part of MfAdditional
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
// =============================================================================
// Source: Parts of examples from MSDN.
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
//
// Users may distribute this source code provided that this header is included
// in full at the top of the file.
//
//==============================================================================
unit WinApi.MediaFoundationApi.MfMetLib;

interface

uses

  {WinApi}
  WinApi.Windows,
  WinApi.Messages,
  WinApi.WinApiTypes,
  WinApi.WinError,
  WinApi.Unknwn,
  WinApi.KsMedia,
  WinApi.Ks,
  WinApi.Dbt,
  WinApi.StrmIf,
  WinApi.UuIds,
  WinApi.AmVideo,
  WinApi.Dvdmedia,
  WinApi.WinMM.MMReg,
  WinApi.ComBaseApi,
  {ActiveX}
  WinApi.ActiveX.PropVarUtil,
  WinApi.ActiveX.PropIdl,
  WinApi.ActiveX.ObjBase,
  WinApi.ActiveX.PropSys,
  {System}
  System.Win.ComObj,
  System.Classes,
  System.SysUtils,
  {DirectX}
  WinApi.DirectX.DxVa2Api,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfUtils,
  WinApi.MediaFoundationApi.MfApi,
  WinApi.MediaFoundationApi.MfIdl,
  WinApi.MediaFoundationApi.MfObjects,
  WinApi.MediaFoundationApi.Evr,
  WinApi.MediaFoundationApi.Evr9,
  WinApi.MediaFoundationApi.MFTransform,
  WinApi.MediaFoundationApi.MfReadWrite,
  WinApi.MediaFoundationApi.WMCodecDsp,
  WinApi.MediaFoundationApi.MfError,
  {CoreAudioApi}
  WinApi.CoreAudioApi.AudioPolicy,
  WinApi.CoreAudioApi.MMDeviceApi;

  {$WEAKPACKAGEUNIT ON}
  {$MINENUMSIZE 4}

  {$IFDEF WIN32}
    {$ALIGN 1}
  {$ELSE}
    {$ALIGN 8} // Win64
  {$ENDIF}

  {$I 'WinApiTypes.inc'}

type

  // used in arrays to hold enum data.
  TDeviceProperties = record
    riid: TGuid;
    count: UINT32;
    uiDeviceIndex: UINT32;
    sFriendlyName: string;
    lpSymbolicLink: LPWSTR;
    chSymbolicLink: UINT32;
  end;

  // Arrays that holds retrieved devices by name and/or index
  TDevicePropertiesArray = array of TDeviceProperties;

  // See https://docs.microsoft.com/en-us/windows/win32/medfound/media-type-guids and MfApi.pas
  TMediaTypes = (mtDefault,                   // Default stream.
                 mtAudio,                     // Audio stream.
                 mtVideo,                     // Video stream.
                 mtProtectedMedia,            // Protected media.
                 mtSAMI,                      // Synchronized Accessible Media Interchange (SAMI) captions (subtitling).
                 mtScript,                    // Script stream.
                 mtStillImage,                // Still image stream.
                 mtHTML,                      // HTML stream.
                 mtBinary,                    // Binary stream.
                 mtFileTransfer,              // A stream that contains data files.
                 mtStream,                    // Multiplexed stream or elementary stream.
                 mtMultiplexedFrames,         // Multiplexed frames stream.
                 mtSubTitle,                  // Subtitle stream.
                 mtPerception,                // Streams from a camera sensor or processing unit that reasons and understands raw video data and provides understanding of the environment or humans in it.
                 mtUnknown                    // Unknown stream type.
                );

  // Stream contents
  PStreamContents = ^TStreamContents;
  _StreamContents = record
    public
      procedure Init();
    public

    dwStreamIndex: DWORD;                 // The stream index (zero based !)
    dwStreamID: DWORD;                    // The stream identifier (see: https://msdn.microsoft.com/en-us/library/windows/desktop/ms703852)
    bSelected: BOOL;                      // The currently selected stream.
    idStreamMediaType: TMediaTypes;       // The mediatype (associated with the Major Type Guid
    idStreamMajorTypeGuid: TGuid;         // The majortype of the stream
    idStreamSubTypeGuid: TGuid;           // The subtype of the stream
    bCompressed: BOOL;                    // Compressed format.

    // Video
    video_FrameRateNumerator: UINT32;     // The upper 32 bits of the MF_MT_FRAME_RATE attribute value
    video_FrameRateDenominator: UINT32;   // The lower 32 bits of the MF_MT_FRAME_RATE attribute value
    // NOTE:
    //  To calculate the framerate in FPS use this formula: Double(video_FrameRateNominator / video_FrameRateDenominator)
    video_PixelAspectRatioNumerator: UINT32;   // The upper 32 bits of the MF_MT_PIXEL_ASPECT_RATIO attribute value
    video_PixelAspectRatioDenominator: UINT32; // The lower 32 bits of the MF_MT_PIXEL_ASPECT_RATIO attribute value
    // NOTE:
    //  To calculate the pixel aspect ratio use this formula: Double(video_PixelAspectRatioNumerator / video_PixelAspectRatioDenominator)
    video_FrameSizeHeigth: UINT32;        // Output frame heigth
    video_FrameSizeWidth: UINT32;         // Output frame width

    // Audio
    audio_lpStreamName: LPWSTR;           // The name of the stream (if stored in the sourcestream)
    audio_lpLangShortName: LPWSTR;        // Short language name (like 'en' for English, 'de' for German, 'fr' for French etc. stored in the stream
    audio_lpLangFullName: LPWSTR;         // Friendly language name (optional, caller needs to set these)
    audio_wsAudioDescr: WideString;       // Audio codec description.
    audio_iAudioChannels: UINT32;         // Number of audio channels.
    audio_iSamplesPerSec: UINT32;         // Audio Samples per second.
    audio_iBitsPerSample: UINT32;         // Audio bits per sample.
    audio_dwFormatTag: DWORD;             // FormatTag is the replacement for FOURCC
  end;
  TStreamContents = _StreamContents;
  TStreamContentsArray = array of TStreamContents;



// EVENTS
// ======

  function GetEventObject(pEvent: IMFMediaEvent;
                          out ppObject): HRESULT;

  // Alternative for ProcessMessages
  // Usage: HandleMessages(GetThreadHandle());
  procedure HandleMessages(hThread: THandle);


// Media Samples
// =============

  // Create a sample and add a buffer to it.
  function CreateMediaSample(cbData: DWORD;
                             ppSample: IMFSample): HRESULT;


// MEDIA SOURCE
// ============

  // Create a media object from an URL or stream.
  // NOTE: This is the replacement for earlier function CreateMediaSourceFromUrl
  function CreateObjectFromUrl(const sURL: WideString;           // URL of the source.
                               out pSource: IMFMediaSource;      // The received object (mediasource or bytestream)
                               pStore: IPropertyStore = Nil;     // Optional property store
                               const dwFlags: DWord = MF_RESOLUTION_MEDIASOURCE): HRESULT;  // Create a source object.

  // Deprecated, use CreateObjectFromUrl.
  function CreateMediaSourceFromUrl(const sURL: WideString;
                                    out pSource: IMFMediaSource): HRESULT; deprecated 'Use function CreateObjectFromUrl';

  // Begins an asynchronous request to create a media source or a byte stream from a URL.
  function CreateObjectFromUrlAsync(const sURL: WideString;
                                    pCallback: IMFAsyncCallback;
                                    pStore: IPropertyStore = Nil;
                                    const dwFlags: DWord = MF_RESOLUTION_MEDIASOURCE;
                                    pIUnknownCancelCookie: IUnknown = Nil;
                                    punkState: IUnknown = Nil): HRESULT;

  // The aggregated media source is useful for combining streams from separate media sources.
  // For example, you can use it to combine a video capture source and an audio capture source.
  function CreateAggregatedSource(pSource1: IMFMediaSource;
                                  pSource2: IMFMediaSource;
                                  out ppAggSource: IMFMediaSource): HRESULT;



// SINKS
// =====

  // Create an activation object for a renderer, based on the stream media type.
  function CreateMediaSinkActivate(pSourceSD: IMFStreamDescriptor;
                                   hVideoWnd: HWND;
                                   out ppActivate: IMFActivate): HRESULT;



// TOPOLOGIES
// ==========

  // Creates a playback topology from the media source.
  //
  // Pre-condition: The media source must be created already.
  // Call CreateMediaSource() before calling this method
  // Create a playback topology from a media source.
  function CreatePlaybackTopology(pSource: IMFMediaSource;               // Media source.
                                  pPD: IMFPresentationDescriptor;        // Presentation descriptor.
                                  hVideoWnd: HWND;                       // Video window.
                                  var ppTopology: IMFTopology;           // Receives a pointer to the topology.
                                  dwSourceStreams: DWORD = 0): HRESULT;  // Recieves the number of streams

  // This funtion is part of the MfPlayer class
  //function CreateFullTopology(pSession: IMFMediaSession;
  //                            pTopology: IMFTopology;
  //                            var caps: DWord): HRESULT;

  //  Adds a topology branch for one stream.
  //
  //  pTopology: Pointer to the topology object.
  //  pSourcePD: The source's presentation descriptor.
  //  iStream: Index of the stream to render.
  //
  //  Pre-conditions: The topology must be created already.
  //
  //  Notes: For each stream, we must do the following:
  //    1. Create a source node associated with the stream.
  //    2. Create an output node for the renderer.
  //    3. Connect the two nodes.
  //  The media session will resolve the topology, so we do not have
  //  to worry about decoders or other transforms.
  function AddBranchToPartialTopology(pTopology: IMFTopology;
                                      pSource: IMFMediaSource;
                                      pPD: IMFPresentationDescriptor;
                                      iStream: DWord;
                                      hVideoWnd: HWND): HRESULT;

  // Create the nodes and connect them.
  // This function is very similar to the function named AddBranchToPartialTopology (Creates a playback topology).
  // The only difference is that this function adds the extra node for the decoder.
  function AddBranchToPartialTopologyWithDecoder(pTopology: IMFTopology;          // Topology.
                                                 pSource: IMFMediaSource;         // Media source.
                                                 pPD: IMFPresentationDescriptor;  // Presentation descriptor.
                                                 iStream: DWORD;                  // Stream index.
                                                 hVideoWnd: HWND): HRESULT;       // Window for video playback.

  function GetPresentationDescriptorFromTopology(pTopology: IMFTopology;
                                                 out ppPD: IMFPresentationDescriptor): HRESULT;

  function GetDurationFromTopology(pTopology: IMFTopology;
                                   out phnsDuration: LONGLONG): HRESULT;

  function GetCollectionObject(pCollection: IMFCollection;
                               const dwIndex: DWORD;
                               out ppObject): HRESULT;

// Source nodes
// ============

  // Creates and initializes a source node from a MediaSource.
  function AddSourceNode(pTopology: IMFTopology;                  // Topology.
                         pSource: IMFMediaSource;                 // Media source.
                         pPD: IMFPresentationDescriptor;          // Presentation descriptor.
                         pSD: IMFStreamDescriptor;                // Stream descriptor.
                         out ppNode: IMFTopologyNode): HRESULT;   // Receives the node pointer.

  // Creates and initializes a source node from a MediaSource.
  function AddSourceStreamNode(pSource: IMFMediaSource;               // Media source.
                               pSourcePD: IMFPresentationDescriptor;  // Presentation descriptor.
                               pSourceSD: IMFStreamDescriptor;        // Stream descriptor.
                               out ppNode: IMFTopologyNode): HRESULT; // Receives the node pointer.



// Output nodes
// ============

  // Creates and initializes an output node from an activation object.
  function AddOutputNodeA(pTopology: IMFTopology;                 // Topology.
                          pActivate: IMFActivate;                 // Media sink activation object.
                          dwId: DWORD;                            // Identifier of the stream sink.
                          out ppNode: IMFTopologyNode): HRESULT;  // Receives the node pointer.

  // Creates and initializes an output node from a stream sink.
  function AddOutputNodeS(pTopology: IMFTopology;                   // Topology.
                          pStreamSink: IMFStreamSink;               // Stream sink.
                          out ppNode: IMFTopologyNode): HRESULT;    // Receives the node pointer.

  //
  function CreateOutputNode(pSourceSD: IMFStreamDescriptor;
                            hwndVideo: HWND;
                            out ppNode: IMFTopologyNode): HRESULT;



// Transform nodes
// ===============

  // Creates and initializes a transform node from an MFT (IMFTransform).
  function AddTransformNodeM(pTopology: IMFTopology;                // Topology.
                             pMFT: IMFTransform;                    // MFT.
                             out ppNode: IMFTopologyNode): HRESULT; // Receives the node pointer.

  // Creates and initializes a transform node from a CLSID.
  function AddTransformNodeC(pTopology: IMFTopology;                 // Topology.
                             const fclsid: CLSID;                    // CLSID of the MFT.
                             out ppNode: IMFTopologyNode): HRESULT;  // Receives the node pointer.

  // Creates and initializes a transform node from an activation object.
  function AddTransformNodeA(pTopology: IMFTopology;                 // Topology.
                             pActivate: IMFActivate;                 // MFT activation object.
                             out ppNode: IMFTopologyNode): HRESULT;  // Receives the node pointer.



// SEEKING & RATE
// ==============

  function DoScrub(const SeekTime: MFTIME;
                   pMediaSession: IMFMediaSession): HRESULT;


  function SetPlaybackRate(pMediaSession: IMFMediaSession;
                           const rateRequested: MFTIME;
                           const bThin: Boolean): HRESULT;


// Sessions
// ========

  // How to set a stop time for playback when using the Media Session.
  // Setting the Stop Time Before Playback begins
  //
  // Before you queue a topology for playback, you can specify the stop time by using
  // the MF_TOPONODE_MEDIASTOP attribute.
  // For each output node in the topology, set the value of the MF_TOPONODE_MEDIASTOP
  // to the stop time in 100-nanosecond (hns) units.
  // Note that setting this attribute after playback starts has no effect.
  // Therefore, set the attribute before calling IMFMediaSession.Start.
  // The following code shows how to set the stop time on an existing topology.

  function SetMediaStop(pTopology: IMFTopology;
                        stop: LONGLONG): HRESULT;

  // Setting the stop time AFTER playback has started.
  //
  // There is a way to set the stop time after the Media Session starts playback,
  // by using the IMFTopologyNodeAttributeEditor interface.
  //
  // Important:
  // This interface has a serious limitation, because the stop time is
  // specified as a 32-bit value. That means the maximum stop time that you can set
  // using this interface is $FFFFFFFF, or just over 7 minutes.
  // This limitation is due to an incorrect structure definition.
  //

  function SetMediaStopDynamic(pSession: IMFMediaSession;
                               pTopology: IMFTopology;
                               stop: LONGLONG): HRESULT;


// De- & encoders
// ==============

  // The following function searches for a video or audio decoder.
  // Asynchronous, hardware, transcode, and field-of-use decoders are excluded.
  // If a match is found, the code creates the first MFT in the list.
  function FindDecoderEx(const subtype: TGUID;                  // Subtype
                         bAudio: Boolean;                       // TRUE for audio, FALSE for video
                         out ppDecoder: IMFTransform): HRESULT; // Receives a pointer to the decoder.


  // Searches for a video or audio encoder.
  // Asynchronous, hardware, transcode, and field-of-use encoders are excluded.
  function FindEncoderEx(const subtype: TGUID;                  // Subtype
                         const bAudio: Boolean;                 // TRUE for audio, FALSE for video
                         out ppEncoder: IMFTransform): HRESULT; // Receives a pointer to the encoder.

  // Searches for a video decoder, with options to include asynchronous, hardware,
  // or transcode decoders.
  function FindVideoDecoder(subtype: TGUID;
                            bAllowAsync: Boolean;
                            bAllowHardware: Boolean;
                            bAllowTranscode: Boolean;
                            ppDecoder: IMFTransform): HRESULT;


  // Returns the MFT decoder based on the major type GUID.
  function GetDecoderCategory(const majorType: TGUID;
                              out pCategory: TGUID): HRESULT;

  // Returns the MFT encoder based on the major type GUID.
  function GetEncoderCategory(const majorType: TGUID;
                              out pCategory: TGUID): HRESULT;

  // Finds a decoder for a stream.
  //
  // If the stream is not compressed, pCLSID receives the value GUID_NULL.
  function FindDecoderForStream(pSD: IMFStreamDescriptor;      // Stream descriptor for the stream.
                                out opCLSID: CLSID): HRESULT;  // Receives the CLSID of the decoder.



// Enumerations
// ============

  // To enumerate the video capture devices on the system, do the following:
  // Call function EnumVideoCaptureDeviceSources with the following parameters:
  //  Parameters:  AttributeSourceType: MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE_AUDCAP_GUID for audio or
  //                                    MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE_VIDCAP_GUID for video.
  //               DeviceProperties: array of TDevicePropsA.
  //  1 Call MFCreateAttributes to create an attribute store.
  //    This function receives an empty IMFAttributes pointer.
  //  2 Call IMFAttributes.SetGUID to set the MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE attribute.
  //    Set the attribute value to MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE_VIDCAP_GUID.
  //  3 Call MFEnumDeviceSources.
  //    This function receives an array of IMFActivate pointers and the array size.
  //    Each pointer represents a distinct video capture device.
  //
  // To create an instance of a capture device:
  //  Call IMFActivate.ActivateObject to get a pointer to the IMFMediaSource interface.
  //
  // This function provide the steps 1 to 3:
  function EnumCaptureDeviceSources(AttributeSourceType: TGuid;
                                    out DeviceProperties: TDevicePropertiesArray): HRESULT;
  // This function activates a selected device stored in TDevicePropsM
  function CreateCaptureDeviceInstance(DeviceProperty: TDeviceProperties;
                                       out ppSource: IMFMediaSource;
                                       out ppActivate: IMFActivate): HRESULT;

  // Enumerates the capture formats for a device.
  function EnumerateCaptureFormats(pSource: IMFMediaSource): HRESULT;

  // Enumerates mediatypes for a stream
  // To get one of the media types for a stream, call the IMFSourceReader.GetNativeMediaType method.
  // This method takes two index parameters: The index of the stream,
  // and an index into the list of media types for the stream.
  // To enumerate all the types for a stream, increment the list index while keeping the stream index constant.
  // When the list index goes out of bounds, GetNativeMediaType returns MF_E_NO_MORE_TYPES.
  function EnumerateTypesForStream(pReader: IMFSourceReader;
                                   dwStreamIndex: DWORD): HRESULT;

  // Returns the name of a guid
  function GetGUIDNameConst(const guid: TGUID): string;



// Device Loss
// ===========

  // Register for Device Notification.
  // Before you start capturing from a device, call the RegisterDeviceNotification
  // function to register for device notifications.
  // Register for the KSCATEGORY_CAPTURE device class, as shown in this function.
  function RegisterForDeviceNotification(hwnd: HWND; out g_hdevnotify: HDEVNOTIFY): HRESULT;

  // Before an application is closing, unregister for device notifications.
  function UnRegisterForDeviceNotification(g_hdevnotify: HDEVNOTIFY): HRESULT;

  // Get the Symbolic Link of the Device
  // Enumerate the video devices on the system, as described in Enumerating Video Capture Devices.
  // Choose a device from the list, and then query the activation object for the
  // MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE_VIDCAP_SYMBOLIC_LINK (= default) or MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE_AUDCAP_SYMBOLIC_LINK attribute,
  // as shown in this function.
  function GetSymbolicLink(pActivate: IMFActivate;
                           out g_pwszSymbolicLink: PWideChar;
                           out g_cchSymbolicLink: UINT32;
                           devMediaType: TGUID): HRESULT;



  // Enable Video Acceleration
  // =========================
  // Adds an audio or video decoder to a topology (applies only to video decoders).
  // To get the best performance for video playback, you should enable
  // DirectX Video Acceleration (DXVA) if the video decoder supports it.
  // Usually this step is performed by the topology loader, but if you add the decoder to
  // the topology manually, then you must perform this step yourself.
  // As a precondition for this step, all output nodes in the topology must be bound to media sinks.
  // For more information, see Binding Output Nodes to Media Sinks.
  //
  // First, find the object in the topology that hosts the Direct3D device manager.
  // To do so, get the object pointer from each node and query the object for the
  // IDirect3DDeviceManager9 service.
  // Typically the enhanced video renderer (EVR) serves this role.
  //
  // The following code shows a function that finds the device manager:

  // Finds the node in the topology that provides the Direct3D device manager.

  function FindDeviceManager(pTopology: IMFTopology;            // Topology to search.
                             out ppDeviceManager: IInterface;   // Receives a pointer to the device manager.
                             out ppNode: IMFTopologyNode): HRESULT;


// Audio and video capture
// =======================

  // Creates a media source for the choosen deviceindex of the video capture device in the enumeration list.
  function CreateVideoCaptureDevice(const iDeviceIndex: UINT32;
                                    out ppSource: IMFMediaSource): HRESULT;

  // Does the same if you know the symbolic link
  function CreateVideoCaptureDevices(const pszSymbolicLink: LPCWSTR;
                                     out ppSource: IMFMediaSource): HRESULT;

  // Takes an audio endpoint ID and creates a media source.
  function CreateAudioCaptureDevice(const pszEndPointID: LPCWSTR;
                                    out ppSource: IMFMediaSource): HRESULT;
  //
  procedure ListDeviceNames(ppDevices: PIMFActivate; // Pointer to array of IMFActivate
                            out iList: TStringList); // output

  //
  function SetMaxFrameRate(pSource: IMFMediaSource; dwTypeIndex: DWORD): HRESULT;




// SAR (Streaming Audio Renderer)
// ==============================

 // Enumerates the audio rendering devices and assigns the first device or nDevice in the list to the SAR.
 function EnumAudioRenderingDevices(nDevice: UInt;
                                    out wstrID: PWideChar; // Id
                                    pSink: IMFMediaSink = Nil;  // Streaming audio renderer (SAR)
                                    pActivate: IMFActivate = Nil // Activation object, which can be used to create the SAR.
                                    ): HResult;


  // Helper methods
  // ==============
  //
  // Defined in WinApi.MediaFoundationApi.MfpUtils

  // This function fills in a BITMAPINFOHEADER structure from a video media type.
  // Note that this conversions loses some of the format information (interlacing, frame rate,
  // extended color data).
  // However, it might be useful when saving a bitmap from a video frame, for example.
  // Converts a video type to a BITMAPINFO structure.
  // The caller must free the structure by calling CoTaskMemFree.
  function GetBitmapInfoHeaderFromMFMediaType(pType: IMFMediaType;     // Pointer to the media type.
                                              out ppBmih: PBITMAPINFOHEADER; // Receives a pointer to the structure.
                                              out pcbSize: DWORD // Receives the size of the structure.
                                              ): HRESULT;

  // Copy an attribute value from one attribute store to another.
  function CopyAttribute(pSrc: IMFAttributes;
                         pDest: IMFAttributes;
                         key: TGUID): HRESULT;


  // Creates a compatible video format with a different subtype.
  function CloneVideoMediaType(var pSrcMediaType: IMFMediaType;
                               guidSubType: REFGUID;
                               out ppNewMediaType: IMFMediaType): HRESULT;


  // Creates a JPEG image type that is compatible with a specified video media type.
  function CreatePhotoMediaType(pSrcMediaType: IMFMediaType;
                                out ppPhotoMediaType: IMFMediaType): HRESULT ;


  // VIDEO MEDIA TYPE HELPERS //////////////////////////////////////////////////

  // NOTE: The parameter pAttributes accepts type IMFAttributes and IMFMediaType

  // Helper function to get the frame rate from a video media type.
  function GetFrameRate(pAttributes: IMFAttributes;
                        out uiNumerator: UINT32;
                        out uiDenominator: UINT32): HResult;

  // Helper function to set the frame rate on a video media type.
  function SetFrameRate(pAttributes: IMFAttributes;
                        uiNumerator: UINT32;
                        uiDenominator: UINT32): HResult;

  // Helper function to get the frame size from IMFAttributes.
  function GetFrameSize(pAttributes: IMFAttributes;
                        out uiWidth: UINT32;
                        out uiHeigth: UINT32): HResult; inline;

  // Helper function to get the frame size from a video media type.
  function GetFrameSizeFromMediaType(pType: IMFMediaType;
                                     out uiWidth: UINT32;
                                     out uiHeight: UINT32): HResult; inline;


  // Helper function to set the frame size on a video attributes.
  function SetFrameSize(pAttributes: IMFAttributes;
                        uiWidth: UINT32;
                        uiHeigth: UINT32): HResult; inline;

  // Helper function to set the frame size on a video media type.
  function SetFrameSizeOnMediaType(pType: IMFMediaType;
                                   uiWidth: UINT32;
                                   uiHeigth: UINT32): HResult; inline;

  // Helper function to get the pixel aspect ratio from a video media type.
  function GetPixelAspectRatio(pAttributes: IMFAttributes;
                               out uiNumerator: UINT32;
                               out uiDenominator: UINT32): HResult;

  // Helper function to set the pixel aspect ratio on a video media type.
  function SetPixelAspectRatio(pAttributes: IMFAttributes;
                               uiNumerator: UINT32;
                               uiDenominator: UINT32): HResult;

  // Helper function to specify whether to pad a video image so that it fits within a specified aspect ratio.
  function SetOutputRectangleAspectRatio(pAttributes: IMFAttributes;
                                         stVideoPadFlags: MFVideoPadFlags = MFVideoPadFlag_PAD_TO_None): HResult;

  //////////////////////////////////////////////////////////////////////////////



  // Gets metadata from a media source or other object.
  // If a media source supports this interface, it must expose the interface as a service.
  // To get a pointer to this interface from a media source, call IMFGetService.GetService.
  // The service identifier is MF_METADATA_PROVIDER_SERVICE.
  // Other types of object can expose this interface through QueryInterface.
  // Use this interface to get a pointer to the IMFMetadata interface.
  function GetMetadata(pSource: IMFMediaSource;
                       out ppMetadata: IMFMetadata;
                       dwStream: DWORD): HResult;

  // Returns the stream identifier from an active stream, of a given streamtype.
  function GetActiveStreamIndex(stmediaType: TMediaTypes;
                                pspd: IMFPresentationDescriptor;
                                out dwStreamId: DWORD): HRESULT;

  // Gets the streams information from a source  (like language, format, compression etc.)
  // It returns an array of the stream content values.
  function GetStreamContents(pspd: IMFPresentationDescriptor;
                             mSource: IMFMediaSource;
                             var alsCont: TStreamContentsArray): HRESULT;

  // Returns the Major guid and compression.
  function GetMediaType(pStreamDesc: IMFStreamDescriptor;
                        out tgMajorGuid: TGuid;
                        out bIsCompressedFormat: BOOL): HRESULT;

  // Returns the mediatype associated with the Major guid.
  // To get the major type call function GetMediaType
  function GetMediaDescription(pMajorGuid: TGuid;
                               out mtMediaType: TMediaTypes): HRESULT;

  // Gets audio stream info
  function GetAudioSubType(mSource: IMFMediaSource;
                           out gSubType: TGUID;
                           out FormatTag: DWord;
                           out wsDescr: Widestring;
                           out cChannels: UINT32;
                           out samplesPerSec: UINT32;
                           out bitsPerSample: UINT32): HRESULT;


// Ducking
//========

  // The following code gets a reference to the IAudioSessionControl2
  // interface and call its methods to determine whether the stream associated with
  // the audio session is a system sound.
  function SetDuckingForSystemSounds(): HResult;


// Sami (.smi .sami)
// ===================

  // Synchronized Accessible Media Interchange (SAMI) is a format for adding captions to digital media.
  // The captions are stored in a separate text file with the file name extension .smi or .sami.
  // In Media Foundation, SAMI caption files are supported through the SAMI media source.
  // Use the Source Resolver to create an instance of the SAMI media source from a URL or byte stream.
  // Media Foundation does not provide a component that displays SAMI captions.
  // The application must interpret the caption data that it receives from the SAMI media source.
  //
  // To change the current style, use the IMFSAMIStyle interface.
  // This interface is obtained by calling IMFGetService.GetService on the SAMI media source.
  // (If you are using the SAMI media source with the Media Session, call GetService on the Media Session.)
  // The service identifier is MF_SAMI_SERVICE.
  //
  // The following function sets the current SAMI style, specified by index.
  function SetSAMIStyleByIndex(pSource: IMFMediaSource;
                               index: DWORD): HRESULT;



// Media files duration and filesize
//==================================

  // Getting the File Duration
  // To get the duration of a media file, call the IMFSourceReader.GetPresentationAttribute method and
  // request the MF_PD_DURATION attribute, as shown in the following code.
  function GetFileDuration(pReader: IMFSourceReader;
                           out phnsDuration: LONGLONG): HRESULT;
  // Gets de file size
  function GetFileSize(pReader: IMFSourceReader;
                       out phnsFileSize: LONGLONG): HRESULT;


// External methods
// ================

  // Brings the thread that created the specified window into the foreground and
  // activates the window.
  // Keyboard input is directed to the window, and various visual cues are changed for the user.
  // The system assigns a slightly higher priority to the thread that created
  // the foreground window than it does to other threads.
  function SetForegroundWindow(hWnd: HWND): Boolean; stdcall;

  // The foreground process can call the LockSetForegroundWindow function to disable
  // calls to the SetForegroundWindow function.
  function LockSetForegroundWindow(uLockCode: UINT): Boolean; stdcall;


// Sequencer Source
// ================

  // The sequencer source enables an application to play a collection of media sources sequentially,
  // with seamless transitions between the sources.
  // You can use it to create playlists, or to play streams from multiple sources simultaneously.
  // See: https://docs.microsoft.com/en-us/windows/win32/medfound/about-the-sequencer-source
  // TODO: Implement as sample (MfPack/Samples/CPlayerEx).


const

// Aliases section
// ===============

  // Renamed functions and procedures for backward compatibility
  CreateVideoCaptureDeviceBySymolicLink: function(const pszSymbolicLink: LPCWSTR;
                                                  out ppSource: IMFMediaSource): HRESULT = CreateVideoCaptureDeviceS;


// System
// ======
  function MfpBoolToStr(const B: Boolean): string; inline;


implementation

const
  User32Lib = 'User32.dll';


function GetEventObject(pEvent: IMFMediaEvent;
                        out ppObject): HRESULT;
var
  vVar: PROPVARIANT;
  hr: HRESULT;

begin

  PropVariantInit(vVar);

  hr := pEvent.GetValue(vvar);
  if (SUCCEEDED(hr)) then
    begin
      if (vvar.vt = VARTYPE(VT_UNKNOWN)) then
        hr := IUnknown(vvar.ppunkVal).QueryInterface(IID_IUnknown,
                                                     ppObject)
      else
        hr := MF_E_INVALIDTYPE;

      PropVariantClear(vVar);
    end;

  Result := hr;
end;


procedure HandleMessages(hThread: THandle);
var
  Msg: TMsg;
  th: THandle;

begin
  if hThread = 0 then
    th := GetCurrentThread()
  else
    th := hThread;

  while (MsgWaitForMultipleObjects(1,
                                   th,
                                   False,
                                   INFINITE,
                                   QS_ALLINPUT) = WAIT_OBJECT_0 + 1) do
    begin
      PeekMessage(Msg, 0, 0, 0, PM_REMOVE);
      if Msg.Message = WM_QUIT then
        Exit;
      TranslateMessage(Msg);
      DispatchMessage(Msg);
    end;
end;


// Create a sample and add a buffer to it.
function CreateMediaSample(cbData: DWORD;
                           ppSample: IMFSample): HRESULT;
var
  hr: HRESULT;
  pSample: IMFSample;
  pBuffer: IMFMediaBuffer;

begin
  hr := MFCreateSample(pSample);

  if SUCCEEDED(hr) then
    hr := MFCreateMemoryBuffer(cbData,
                               pBuffer);

  if SUCCEEDED(hr) then
    hr := pSample.AddBuffer(pBuffer);

  if SUCCEEDED(hr) then
    ppSample := pSample;

  Result := hr;
end;


// Deprecated, use CreateObjectFromUrl.
function CreateMediaSourceFromUrl(const sURL: WideString;
                                  out pSource: IMFMediaSource): HRESULT;
begin
  Result := CreateObjectFromUrl(sURL,
                                pSource);
end;


// Create a media object from an URL or stream.
// NOTE: This is the replacement for earlier function CreateMediaSourceFromUrl
function CreateObjectFromUrl(const sURL: WideString;
                             out pSource: IMFMediaSource;
                             pStore: IPropertyStore = Nil;
                             const dwFlags: DWord = MF_RESOLUTION_MEDIASOURCE): HRESULT;
var
  ObjectType: MF_OBJECT_TYPE;
  pSourceResolver: IMFSourceResolver;
  unkSource: IUnknown;
  hr: HRESULT;

label
  Done;

begin

  ObjectType := MF_OBJECT_INVALID;

  // Create the source resolver.
  hr := MFCreateSourceResolver(pSourceResolver);
  if (FAILED(hr)) then
    goto done;

  // Use the source resolver to create the media source.
  // Note: For simplicity this function uses the synchronous method on
  // IMFSourceResolver to create the media source. However, creating a media
  // source can take a noticeable amount of time, especially for a network source.
  // For a more responsive UI, use the asynchronous BeginCreateObjectFromURL method.

  hr := pSourceResolver.CreateObjectFromURL(LPCWSTR(sURL), // URL of the source.
                                            dwFlags,       // Create a source object.
                                            pStore,        // Optional property store.
                                            ObjectType,    // Receives the created object type.
                                            unkSource);    // Receives a pointer to the media source (IUnknown).
  if (FAILED(hr)) then
    goto done;



  // Get the IMFMediaSource interface from the media source.
  hr := unkSource.QueryInterface(IUnknown,
                                 pSource);

  // This will work as well: pSource := IMFMediaSource(unkSource);

Done:
  // unlike C/CPP Delphi cleans up all interfaces when going out of scope.
  Result := hr;
end;


// Begins an asynchronous request to create a media source or a byte stream from a URL.
function CreateObjectFromUrlAsync(const sURL: WideString;
                                  pCallback: IMFAsyncCallback;
                                  pStore: IPropertyStore = Nil;
                                  const dwFlags: DWord = MF_RESOLUTION_MEDIASOURCE;
                                  pIUnknownCancelCookie: IUnknown = Nil;
                                  punkState: IUnknown = Nil): HRESULT;
var
  pSourceResolver: IMFSourceResolver;
  hr: HRESULT;

label
  Done;

begin

  // Create the source resolver.
  hr := MFCreateSourceResolver(pSourceResolver);
  if (FAILED(hr)) then
    goto done;

  // Use the source resolver to create the media source.

  hr := pSourceResolver.BeginCreateObjectFromURL(LPCWSTR(sURL),         // URL of the source.
                                                 dwFlags,               // Create a source object.
                                                 pStore,                // Optional property store.
                                                 pIUnknownCancelCookie, // Receives an IUnknown pointer or the value Nil.
                                                 pCallback,             // Pointer to the IMFAsyncCallback interface of a callback object. The caller must implement this interface.
                                                 punkState);            // Pointer to the IUnknown interface of a state object, defined by the caller. This parameter can be Nil.

  if (FAILED(hr)) then
    goto done;

Done:
  // unlike C/CPP Delphi cleans up all interfaces when going out of scope.
  Result := hr;
end;


// Combine streams from separate media sources. For example one for audio and one for video
function CreateAggregatedSource(pSource1: IMFMediaSource;
                                pSource2: IMFMediaSource;
                                out ppAggSource: IMFMediaSource): HRESULT;
var
  hr: HRESULT;
  pCollection: IMFCollection;

begin
  ppAggSource := Nil;
  pCollection := Nil;

  hr := MFCreateCollection(pCollection);

  // Add sources to IMFCollection
  if SUCCEEDED(hr) then
    hr := pCollection.AddElement(pSource1);

  if SUCCEEDED(hr) then
    hr := pCollection.AddElement(pSource2);

  // Create the aggregate source
  if SUCCEEDED(hr) then
    hr := MFCreateAggregateSource(pCollection,
                                  ppAggSource);

  Result := hr;
end;


//  Create an activation object for a renderer, based on the stream media type.
function CreateMediaSinkActivate(pSourceSD: IMFStreamDescriptor;
                                 hVideoWnd: HWND;
                                 out ppActivate: IMFActivate): HRESULT;
var
  phandler: IMFMediaTypeHandler;
  pActivate: IMFActivate;
  guidMajorType: TGUID;
  hr: HRESULT;

label
  Done;

begin
  // Get the media type handler for the stream
  hr := pSourceSD.GetMediaTypeHandler(pHandler);
  if FAILED(hr) then
    goto Done;

  // Get the major media type
  hr := pHandler.GetMajorType(guidMajorType);
  if FAILED(hr) then
    goto Done;

  // Create an IMFActivate object for the renderer, based on the media type
  if IsEqualGuid(MFMediaType_Audio,
                guidMajorType) then
    hr := MFCreateAudioRendererActivate(pActivate)
  else if IsEqualGuid(MFMediaType_Video,
                      guidMajorType) then
    hr := MFCreateVideoRendererActivate(hVideoWnd,
                                        pActivate)
  else
    hr := E_FAIL;

  if FAILED(hr) then
    goto Done;

  // Return IMFactivate pointer to caller
  ppActivate := pActivate;

done:
  Result := hr;
end;


//
function AddSourceStreamNode(pSource: IMFMediaSource;
                             pSourcePD: IMFPresentationDescriptor;
                             pSourceSD: IMFStreamDescriptor;
                             out ppNode: IMFTopologyNode): HRESULT;
var
   hr: HRESULT;

label
  done;

begin

  if (not Assigned(pSource) or
      not Assigned(pSourcePD) or
      not Assigned(pSourceSD)) then
    begin
      Result := E_POINTER;
      Exit;
    end;

  // Create the source-stream node.
  hr := MFCreateTopologyNode(MF_TOPOLOGY_SOURCESTREAM_NODE,
                             ppNode);
  if (FAILED(hr)) then
    goto done;

  // Set attribute: Pointer to the media source.
  hr := ppNode.SetUnknown(MF_TOPONODE_SOURCE,
                          pSource);
  if (FAILED(hr)) then
    goto done;

  // Set attribute: Pointer to the presentation descriptor.
  hr := ppNode.SetUnknown(MF_TOPONODE_PRESENTATION_DESCRIPTOR,
                          pSourcePD);
  if (FAILED(hr)) then
    goto done;

  // Set attribute: Pointer to the stream descriptor.
  hr := ppNode.SetUnknown(MF_TOPONODE_STREAM_DESCRIPTOR,
                          pSourceSD);
  if (FAILED(hr)) then
    goto done;

done:
  Result := hr;
end;


// Create a playback topology from a media source.
function CreatePlaybackTopology(pSource: IMFMediaSource;               // Media source.
                                pPD: IMFPresentationDescriptor;        // Presentation descriptor.
                                hVideoWnd: HWND;                       // Video window.
                                var ppTopology: IMFTopology;           // Receives a pointer to the topology.
                                dwSourceStreams: DWORD = 0): HRESULT;
var
  tmpTopology: IMFTopology;
  hr: HResult;
  i: integer;

label
  done;

begin

  hr := MFCreateTopology(tmpTopology);
  if (FAILED(hr)) then
    goto done;

  // Get the number of streams in the media source.
  hr := pPD.GetStreamDescriptorCount(dwSourceStreams);
  if (FAILED(hr)) then
    goto done;

  // For each stream, create the topology nodes and add them to the topology.
  for i := 0 to dwSourceStreams - 1 do
    begin
      hr := AddBranchToPartialTopology(tmpTopology,
                                       pSource,
                                       pPD,
                                       i,
                                       hVideoWnd);
      if (FAILED(hr)) then
        goto done;

      hr := pPD.SelectStream(i);
    end;

  ppTopology := tmpTopology;
  if (FAILED(hr)) then
    goto done;

done:
  Result := hr;
end;


// Add a source node to a topology.
function AddSourceNode(pTopology: IMFTopology;                   // Topology.
                       pSource: IMFMediaSource;                  // Media source.
                       pPD: IMFPresentationDescriptor;           // Presentation descriptor.
                       pSD: IMFStreamDescriptor;                 // Stream descriptor.
                       out ppNode: IMFTopologyNode): HRESULT;    // Receives the node pointer.

var
  hr: HRESULT;

label
  done;

begin
  ppNode := Nil;

  // Create the node.
  hr := MFCreateTopologyNode(MF_TOPOLOGY_SOURCESTREAM_NODE,
                             ppNode);
  if (FAILED(hr)) then
    goto done;

  // Set the attributes.
  hr := ppNode.SetUnknown(MF_TOPONODE_SOURCE,
                          pSource);
  if (FAILED(hr)) then
    goto done;

  hr := ppNode.SetUnknown(MF_TOPONODE_PRESENTATION_DESCRIPTOR,
                          pPD);
  if (FAILED(hr)) then
    goto done;

  hr := ppNode.SetUnknown(MF_TOPONODE_STREAM_DESCRIPTOR,
                          pSD);
  if (FAILED(hr)) then
    goto done;

  // Add the node to the topology.
  hr := pTopology.AddNode(ppNode);

  if (FAILED(hr)) then
    goto done;

done:
   Result := hr;
end;


// Add an output node to a topology.
function AddOutputNodeA(pTopology: IMFTopology;                 // Topology.
                        pActivate: IMFActivate;                 // Media sink activation object.
                        dwId: DWORD;                            // Identifier of the stream sink.
                        out ppNode: IMFTopologyNode): HRESULT;  // Receives the node pointer.
var
  hr: HRESULT;

label
  done;

begin

  // Create the node.
  hr := MFCreateTopologyNode(MF_TOPOLOGY_OUTPUT_NODE,
                             ppNode);
  if (FAILED(hr)) then
    goto done;

  // Set the object pointer.
  hr := ppNode.SetObject(pActivate);
  if (FAILED(hr)) then
    goto done;

  // Set the stream sink ID attribute.
  hr := ppNode.SetUINT32(MF_TOPONODE_STREAMID,
                         dwId);
  if (FAILED(hr)) then
    goto done;

  hr := ppNode.SetUINT32(MF_TOPONODE_NOSHUTDOWN_ON_REMOVE,
                         0);
  if (FAILED(hr)) then
    goto done;

  // Add the node to the topology.
  hr := pTopology.AddNode(ppNode);
  if (FAILED(hr)) then
    goto done;

done:
  Result := hr;
end;


function AddOutputNodeS(pTopology: IMFTopology;                   // Topology.
                        pStreamSink: IMFStreamSink;               // Stream sink.
                        out ppNode: IMFTopologyNode): HRESULT;    // Receives the node pointer.
var
  hr: HRESULT;

begin
  ppNode := Nil;

  // Create the node.
  hr := MFCreateTopologyNode(MF_TOPOLOGY_OUTPUT_NODE,
                             ppNode);

  // Set the object pointer.
  if (SUCCEEDED(hr)) then
    begin
      hr := ppNode.SetObject(pStreamSink);
    end;

  // Add the node to the topology.
  if (SUCCEEDED(hr)) then
    begin
      hr := pTopology.AddNode(ppNode);
    end;

  if (SUCCEEDED(hr)) then
    begin
      hr := ppNode.SetUINT32(MF_TOPONODE_NOSHUTDOWN_ON_REMOVE,
                             0);
    end;

  Result := hr;
end;


//  Add a topology branch for one stream.
//
//  For each stream, this function does the following:
//
//    1. Creates a source node associated with the stream.
//    2. Creates an output node for the renderer.
//    3. Connects the two nodes.
//
//  The media session will add any decoders that are needed.
function AddBranchToPartialTopology(pTopology: IMFTopology;
                                    pSource: IMFMediaSource;
                                    pPD: IMFPresentationDescriptor;
                                    iStream: DWord;
                                    hVideoWnd: HWND): HRESULT;
var
  pSD: IMFStreamDescriptor;
  pSinkActivate: IMFActivate;
  pSourceNode: IMFTopologyNode;
  pOutputNode: IMFTopologyNode;
  fSelected: BOOL;
  hr: HRESULT;

label
  Done;

begin
  // Use assertions only for debugging purposes
  assert(pTopology <> Nil);

  // Get the stream descriptor for this stream.
  hr := pPD.GetStreamDescriptorByIndex(iStream,
                                       fSelected,
                                       pSD);
  if (FAILED(hr)) then
    goto done;

  // Create the topology branch only if the stream is selected.
  // Otherwise, do nothing.
  if (fSelected) then
    begin
      // create the media sink activation object
      hr := CreateMediaSinkActivate(pSD,
                                    hVideoWnd,
                                    pSinkActivate);
      if (FAILED(hr)) then
        goto done;

      // Create a source node for this stream.
      hr := AddSourceNode(pTopology,
                          pSource,
                          pPD,
                          pSD,
                          pSourceNode);
      if (FAILED(hr)) then
        goto done;

      // Create the output node for the renderer.
      hr := AddOutPutNodeA(pTopology,
                           pSinkActivate,
                           0,
                           pOutPutNode);
      if (FAILED(hr)) then
        goto done;

      // Connect the source node to the output node.
      hr := pSourceNode.ConnectOutput(0,
                                      pOutputNode,
                                      0);
    end;

done:
  Result := hr;
end;

//
function AddBranchToPartialTopologyWithDecoder(pTopology: IMFTopology;          // Topology.
                                               pSource: IMFMediaSource;         // Media source.
                                               pPD: IMFPresentationDescriptor;  // Presentation descriptor.
                                               iStream: DWORD;                  // Stream index.
                                               hVideoWnd: HWND): HRESULT;       // Window for video playback.
var
  hr: HRESULT;
  pSD: IMFStreamDescriptor;
  pSinkActivate: IMFActivate;
  pSourceNode: IMFTopologyNode;
  pOutputNode: IMFTopologyNode;
  pDecoderNode: IMFTopologyNode;

  fSelected: BOOL;
  clsidDecoder: CLSID;

begin

  fSelected := False;
  clsidDecoder := GUID_NULL;

  // Get the stream descriptor.
  hr := pPD.GetStreamDescriptorByIndex(iStream,
                                       fSelected,
                                       pSD);
  if (FAILED(hr)) then
    begin
      Result := hr;
      Exit;
    end;

  if (fSelected = True) then
    begin
      // Add a source node for this stream.
      hr := AddSourceNode(pTopology,
                          pSource,
                          pPD,
                          pSD,
                          pSourceNode);

      // Create the media sink activation object.
      if (SUCCEEDED(hr)) then
        begin
          hr := CreateMediaSinkActivate(pSD,
                                        hVideoWnd,
                                        pSinkActivate);
        end;

      // Create the output node for the renderer.
      if (SUCCEEDED(hr)) then
        begin
          hr := AddOutputNodeA(pTopology,
                               pSinkActivate,
                               0,
                               pOutputNode);
        end;

      // Find a decoder.
      if (SUCCEEDED(hr)) then
        begin
          hr := FindDecoderForStream(pSD,
                                     clsidDecoder);
        end;

      if (SUCCEEDED(hr)) then
        begin
          if (clsidDecoder = GUID_NULL) then
            begin
              // No decoder is required.
              // Connect the source node to the output node.
              hr := pSourceNode.ConnectOutput(0,
                                              pOutputNode,
                                              0);
            end
          else
            begin
              // Add a decoder node.
              hr := AddTransformNodeC(pTopology,
                                      clsidDecoder,
                                      pDecoderNode);

              // Connect the source node to the decoder node.
              if (SUCCEEDED(hr)) then
                begin
                  hr := pSourceNode.ConnectOutput(0,
                                                  pDecoderNode,
                                                  0);
                end;

              // Connect the decoder node to the output node.
              if (SUCCEEDED(hr)) then
                begin
                  hr := pDecoderNode.ConnectOutput(0,
                                                   pOutputNode,
                                                   0);
                end;
            end;
        end;

      // Mark this branch as not requiring a decoder.
      if (SUCCEEDED(hr)) then
        begin
          hr := pOutputNode.SetUINT32(MF_TOPONODE_CONNECT_METHOD,
                                      UINT32(MF_CONNECT_ALLOW_CONVERTER));
        end;

      if (SUCCEEDED(hr)) then
        begin
          hr := pDecoderNode.SetUINT32(MF_TOPONODE_CONNECT_METHOD,
                                       UINT32(MF_CONNECT_ALLOW_CONVERTER));
        end;

    end;
    // else: If not selected, don't add the branch.

  Result := hr;
end;


//
function CreateOutputNode(pSourceSD: IMFStreamDescriptor;
                          hwndVideo: HWND;
                          out ppNode: IMFTopologyNode): HRESULT;
var
  pNode: IMFTopologyNode;
  pHandler: IMFMediaTypeHandler;
  pRendererActivate: IMFActivate;
  guidMajorType: TGUID;
  streamID: DWORD;
  hr: HRESULT;

label
  done;

begin

  // Get the stream ID.
  streamID := 0;

  // Just for debugging, ignore any failures.
  hr := pSourceSD.GetStreamIdentifier(streamID);
  if (FAILED(hr)) then
    goto done;

  // Get the media type handler for the stream.
  hr := pSourceSD.GetMediaTypeHandler(pHandler);
  if (FAILED(hr)) then
    goto done;

  // Get the major media type.
  hr := pHandler.GetMajorType(guidMajorType);
  if (FAILED(hr)) then
    goto done;

  // Create a downstream node.
  hr := MFCreateTopologyNode(MF_TOPOLOGY_OUTPUT_NODE,
                             pNode);
  if (FAILED(hr)) then
    goto done;

  // Create an IMFActivate object for the renderer, based on the media type.
  if MFMediaType_Audio = guidMajorType then
    begin
      // Create the audio renderer.
      hr := MFCreateAudioRendererActivate(pRendererActivate);
    end
  else
    if (MFMediaType_Video = guidMajorType) then
      begin
        // Create the video renderer.
        hr := MFCreateVideoRendererActivate(hwndVideo, pRendererActivate);
      end
    else
      hr := E_FAIL;

  if (FAILED(hr)) then
    goto done;

  // Set the IActivate object on the output node.
  hr := pNode.SetObject(pRendererActivate);
  if (FAILED(hr)) then
    goto done;

  // Return the IMFTopologyNode pointer to the caller.
  ppNode := pNode;
  
done:
  Result := hr;
end;


function AddTransformNodeM(pTopology: IMFTopology;      // Topology.
                           pMFT: IMFTransform;          // MFT.
                           out ppNode: IMFTopologyNode): HRESULT;
var
  hr: HRESULT;

begin
  ppNode := Nil;

  // Create the node.
  hr := MFCreateTopologyNode(MF_TOPOLOGY_TRANSFORM_NODE,
                             ppNode);

  // Set the object pointer.
  if (SUCCEEDED(hr)) then
    begin
      hr := ppNode.SetObject(pMFT);
    end;

  // Add the node to the topology.
  if (SUCCEEDED(hr)) then
    begin
      hr := pTopology.AddNode(ppNode);
    end;

  Result := hr;
end;


function AddTransformNodeC(pTopology: IMFTopology;                 // Topology.
                           const fclsid: CLSID;                    // CLSID of the MFT.
                           out ppNode: IMFTopologyNode): HRESULT;  // Receives the node pointer.
var
  hr: HRESULT;

begin
  ppNode := Nil;

  // Create the node.
  hr := MFCreateTopologyNode(MF_TOPOLOGY_TRANSFORM_NODE,
                             ppNode);

  // Set the CLSID attribute.
  if (SUCCEEDED(hr)) then
    begin
      hr := ppNode.SetGUID(MF_TOPONODE_TRANSFORM_OBJECTID,
                           fclsid);
    end;

  // Add the node to the topology.
  if (SUCCEEDED(hr)) then
    begin
      hr := pTopology.AddNode(ppNode);
    end;

  Result := hr;
end;


function AddTransformNodeA(pTopology: IMFTopology;                 // Topology.
                           pActivate: IMFActivate;                 // MFT activation object.
                           out ppNode: IMFTopologyNode): HRESULT;  // Receives the node pointer.
var
  hr: HRESULT;

begin
  ppNode := Nil;

  // Create the node.
  hr := MFCreateTopologyNode(MF_TOPOLOGY_TRANSFORM_NODE,
                             ppNode);

  // Set the object pointer.
  if (SUCCEEDED(hr)) then
    begin
      hr := ppNode.SetObject(pActivate);
    end;

  // Add the node to the topology.
  if (SUCCEEDED(hr)) then
    begin
      hr := pTopology.AddNode(ppNode);
    end;

  Result := hr;
end;


// Given a topology (pTopology), returns a pointer to the presentation descriptor.
function GetPresentationDescriptorFromTopology(pTopology: IMFTopology;
                                               out ppPD: IMFPresentationDescriptor): HRESULT;
var
  hr: HRESULT;
  pCollection: IMFCollection;
  pUnk: PIUnknown;
  pNode: IMFTopologyNode;
  dwElementcount: DWord;
  dwIndex: DWORD;

label
  done;

begin

  dwIndex := 0;

  // Get the collection of source nodes from the topology.
  hr := pTopology.GetSourceNodeCollection(pCollection);
  if FAILED(hr) then
    goto done;

  // Any of the source nodes should have the PD, so take the first
  // object in the collection.
  hr := pCollection.GetElementCount(dwElementcount);
  if FAILED(hr) then
    begin
      OleCheck(hr);
      goto done;
    end;

  if (dwElementcount > 0) then
    begin
      hr := pCollection.GetElement(dwIndex,
                                   pUnk);
      if FAILED(hr) then
        begin
          OleCheck(hr);
          goto done;
        end;
    end
  else
    begin
      hr := MF_E_NOT_FOUND;
      goto done;
    end;

  hr := pUnk.QueryInterface(IID_IMFTopologyNode,
                            pNode);
    if FAILED(hr) then
      goto done;

  // Get the PD, which is stored as an attribute.
  hr := pNode.GetUnknown(MF_TOPONODE_PRESENTATION_DESCRIPTOR,
                         IID_IMFPresentationDescriptor,
                         ppPD);

done:
  Result := hr;
end;

//
function GetDurationFromTopology(pTopology: IMFTopology;
                                 out phnsDuration: LONGLONG): HRESULT;
var
  pSourceNodes: IMFCollection;
  pNode: IMFTopologyNode;
  pPD: IMFPresentationDescriptor;
  hr: HRESULT;

label
  done;

begin
  phnsDuration := 0;

  hr := pTopology.GetSourceNodeCollection(pSourceNodes);
  if FAILED(hr) then
    goto done;

  hr := GetCollectionObject(pSourceNodes,
                            0,
                            pNode);
  if FAILED(hr) then
    goto done;

  hr := pNode.GetUnknown(MF_TOPONODE_PRESENTATION_DESCRIPTOR,
                         IID_IMFPresentationDescriptor,
                         pPD);
  if FAILED(hr) then
    goto done;

  phnsDuration := MFGetAttributeUINT64(pPD,
                                       MF_PD_DURATION,
                                       0);

done:
  Result := hr;
end;


function GetCollectionObject(pCollection: IMFCollection;
                             const dwIndex: DWORD;
                             out ppObject): HRESULT;
var
  pUnk: PIUnknown;
  hr: HRESULT;

begin

  hr := pCollection.GetElement(dwIndex,
                               pUnk);
  if SUCCEEDED(hr) then
    begin
      hr := pUnk.QueryInterface(IID_IUnknown,
                                ppObject);
    end;

  Result := hr;
end;



// SCRUBBING
function DoScrub(const SeekTime: MFTIME;
                 pMediaSession: IMFMediaSession): HRESULT;
var
  hr: HRESULT;
  pvar: PROPVARIANT;
  pRateControl: IMFRateControl;

begin
  pRateControl := Nil;
  PropVariantInit(pvar);

  // Get the rate control service.
  hr := MFGetService(pMediaSession,
                     MF_RATE_CONTROL_SERVICE,
                     IID_IMFRateControl,
                     pRateControl);

  // Set the playback rate to zero without thinning.
  if (SUCCEEDED(hr)) then
    hr := pRateControl.SetRate(Boolean(0), 0.0);

  // Create the Media Session start position.
  if (SeekTime = PRESENTATION_CURRENT_POSITION)then
    pvar.vt := VARTYPE(VT_EMPTY)
  else
    begin
      pvar.vt := VARTYPE(VT_I8);
      pvar.hVal.QuadPart := SeekTime;
    end;

  // Start the Media Session.
  if (SUCCEEDED(hr)) then
    hr := pMediaSession.Start(GUID_NULL,
                              pvar);

  // Clean up.
  PropVariantClear(pvar);
  Result := hr;
end;


///////////////////////////////////////////////////////////////////////
//  Name: SetPlaybackRate
//  Description:
//      Gets the rate control service from Media Session.
//      Sets the playback rate to the specified rate.
//  Parameter:
//      pMediaSession: [in] Media session object to query.
//      rateRequested: [in] Playback rate to set.
//      bThin: [in] Indicates whether to use thinning.
///////////////////////////////////////////////////////////////////////

function SetPlaybackRate(pMediaSession: IMFMediaSession;
                         const rateRequested: MFTIME;
                         const bThin: Boolean): HRESULT;
var
  hr: HRESULT;
  pRateControl: IMFRateControl;

begin

  pRateControl := Nil;

  // Get the rate control object from the Media Session.
  hr := MFGetService(pMediaSession,
                     MF_RATE_CONTROL_SERVICE,
                     IID_IMFRateControl,
                     pRateControl);

  // Set the playback rate.
  if (SUCCEEDED(hr)) then
    hr := pRateControl.SetRate(bThin,
                               rateRequested);

  Result := hr;
end;

// Before you queue a topology for playback, you can specify the stop time by using
// the MF_TOPONODE_MEDIASTOP attribute.
// For each output node in the topology, set the value of the MF_TOPONODE_MEDIASTOP
// to the stop time in 100-nanosecond units.
// Note that setting this attribute after playback starts has no effect.
// Therefore, set the attribute before calling IMFMediaSession.Start.
// The following code shows how to set the stop time on an existing topology.
// Note:
//   Instead of creating a class (like the MS example) we can do
//   with a nested function.
//
function SetMediaStop(pTopology: IMFTopology;
                      stop: LONGLONG): HRESULT;

  function GetCollectionObject(pCollection: IMFCollection;
                               dwIndex: DWORD;
                               ppObject: Pointer): HRESULT;
  var
    pUnk: PIUnknown;
    hr: HRESULT;

  begin
    ppObject := Nil;   // zero output
    pUnk := Nil;

    hr := pCollection.GetElement(dwIndex,
                                 pUnk);
    if SUCCEEDED(hr) then
      begin
        hr := pUnk.QueryInterface(IID_IMFTopologyNode,
                                  ppObject);
        SafeRelease(pUnk);
      end;
    Result := hr;
  end;

var
  pCol: IMFCollection;
  cNodes: DWORD;
  hr: HRESULT;
  i: Integer;
  pNode: IMFTopologyNode;

begin

  hr := pTopology.GetSourceNodeCollection(pCol);
  if SUCCEEDED(hr) then
    begin
      hr := pCol.GetElementCount(cNodes);
    end;

  if SUCCEEDED(hr) then
    begin
      for i := 0 to cNodes - 1 do
        begin
          hr := GetCollectionObject(pCol,
                                   i,
                                   @pNode);
          if SUCCEEDED(hr) then
            begin
              pNode.SetUINT64(MF_TOPONODE_MEDIASTOP,
                              stop);
            end;
          SafeRelease(pNode);
        end;
    end;
  Result := hr;
end;

// Important
//  This interface has a serious limitation, because the stop time is specified as a 32-bit value.
//  That means the maximum stop time that you can set using this interface is 0xFFFFFFFF,
//  or just over 7 minutes. This limitation is due to an incorrect structure definition.
//
// To set the stop time using the IMFTopologyNodeAttributeEditor interface, perform the following steps.
// 1 Call MFGetService to get the IMFTopologyNodeAttributeEditor interface from the Media Session.
// 2 Call IMFTopology.GetTopologyID to get the ID of the playback topology.
// 3 For each output node in the topology, call IMFTopologyNodeAttributeEditor.UpdateNodeAttributes.
//   This method takes the topology ID and a pointer to a MFTOPONODE_ATTRIBUTE_UPDATE structure.
//   Initialize the structure as follows.
//
//    Member	           Value
//    ------------------ ------------------------------------------------------------------------------
//    NodeId	           The node ID. To get the node ID, call call IMFTopologyNode.GetTopoNodeID.
//    guidAttributeKey	 MF_TOPONODE_MEDIASTOP
//    attrType	         MF_ATTRIBUTE_UINT64
//    u64	               The stop time, in 100-nanosecond units.
//
function SetMediaStopDynamic(pSession: IMFMediaSession;
                             pTopology: IMFTopology;
                             stop: LONGLONG): HRESULT;
const
  MAXUINT32 = 4294967294;  // UINT32 0..4294967295 on 32 bit platforms.
                           // Int64 on 64 bit platforms.
var
  pAttr: IMFTopologyNodeAttributeEditor;
  pCol: IMFCollection;
  pNode: IMFTopologyNode;
  hr: HRESULT;
  id: TOPOID;
  nodeID: TOPOID;
  cNodes: DWORD;
  i: Integer;
  update: MFTOPONODE_ATTRIBUTE_UPDATE;

label
  done;

begin

  if (stop > MAXUINT32) then
    begin
      Result := E_INVALIDARG;
      Exit;
    end;


  hr := MFGetService(pSession,
                     MF_TOPONODE_ATTRIBUTE_EDITOR_SERVICE,
                     IID_IMFTopologyNodeAttributeEditor,
                     pAttr);
  if FAILED(hr) then
    goto done;

  hr := pTopology.GetTopologyID(id);
  if FAILED(hr) then
    goto done;

  hr := pTopology.GetSourceNodeCollection(pCol);
  if FAILED(hr) then
    goto done;

  hr := pCol.GetElementCount(cNodes);
  if FAILED(hr) then
    goto done;

  for i := 0 to cNodes - 1 do
    begin
      hr := GetCollectionObject(pCol,
                                i,
                                pNode);
      if FAILED(hr) then
        goto done;

      hr := pNode.GetTopoNodeID(nodeID);
      if FAILED(hr) then
        goto done;

      update.NodeId := nodeID;
      update.guidAttributeKey := MF_TOPONODE_MEDIASTOP;
      update.attrType := MF_ATTRIBUTE_UINT64;
      // Be careful to set the value of attrType correctly.
      // Although u64 is a 32-bit type, the method requires that attrType be set to MF_ATTRIBUTE_UINT64.
      update.u64 := UINT32(stop); // ! See Remarks !

      hr := pAttr.UpdateNodeAttributes(id,
                                       1,
                                       update);
      if FAILED(hr) then
        goto done;

      SafeRelease(pNode);
    end;

done:
  Result := hr;
end;

//
function FindDecoderEx(const subtype: TGUID;         // Subtype
                       bAudio: Boolean;              // TRUE for audio, FALSE for video
                       out ppDecoder: IMFTransform): HRESULT;
const
  enumflag = MFT_ENUM_FLAG_SYNCMFT or
             MFT_ENUM_FLAG_LOCALMFT or
             MFT_ENUM_FLAG_SORTANDFILTER;

var
  hr: HRESULT;
  count: UINT32;
  ppMFTActivate: PIMFActivate;  // Pointer to array
  info: MFT_REGISTER_TYPE_INFO;
  mft: TGuid;

begin
  hr := S_OK;
  count := 0;

try

  if bAudio = True then
    begin
      info.guidMajorType := MFMediaType_Audio;
      info.guidSubtype := subtype;
      mft := MFT_CATEGORY_AUDIO_DECODER;
    end
  else
    begin
      info.guidMajorType := MFMediaType_Video;
      info.guidSubtype := subtype;
      mft := MFT_CATEGORY_VIDEO_DECODER;
    end;

  hr := MFTEnumEx(mft,
                  enumflag,
                  @info,    // Input type
                  Nil,      // Output type
                  ppMFTActivate, // array of IMFActivate
                  count);        // number of returned elements

  if (SUCCEEDED(hr) And (count = 0)) then
    begin
      hr := MF_E_TOPO_CODEC_NOT_FOUND;
    end;

{$POINTERMATH ON}
  // Create the first decoder in the list.
  if (SUCCEEDED(hr)) then
    begin
      hr := ppMFTActivate[0].ActivateObject(IID_IMFActivate,
                                            ppDecoder);
    end;
{$POINTERMATH OFF}

finally
  Result := hr;
end;
end;

//
function FindEncoderEx(const subtype: TGUID;
                       const bAudio: Boolean;
                       out ppEncoder: IMFTransform): HRESULT;
var
  hr: HRESULT;
  count: UINT32;
  ppActivate: PIMFActivate;
  info: MFT_REGISTER_TYPE_INFO;
  mft: TGuid;

begin
  hr := S_OK;

try

  if (bAudio = true) then
    begin
      info.guidMajorType := MFMediaType_Audio;
      info.guidSubtype := subtype;
      mft := MFT_CATEGORY_AUDIO_ENCODER;
    end
  else
    begin
      info.guidMajorType := MFMediaType_Video;
      info.guidSubtype := subtype;
      mft := MFT_CATEGORY_VIDEO_ENCODER
    end;

  hr := MFTEnumEx(mft,
                  MFT_ENUM_FLAG_SYNCMFT or
                  MFT_ENUM_FLAG_LOCALMFT or
                  MFT_ENUM_FLAG_SORTANDFILTER,
                  Nil,       // Input type
                  @info,     // Output type
                  ppActivate,
                  count);

  if (SUCCEEDED(hr) And (count = 0)) then
      begin
        hr := MF_E_TOPO_CODEC_NOT_FOUND;
      end;

  // Create the first encoder in the list.
{$POINTERMATH ON}
  if SUCCEEDED(hr) then
    begin
      hr := ppActivate[0].ActivateObject(IID_IMFTransform,
                                         ppEncoder);
    end;
{$POINTERMATH OFF}

finally
  Result := hr;
end;
end;

//
function FindVideoDecoder(subtype: TGUID;
                          bAllowAsync: Boolean;
                          bAllowHardware: Boolean;
                          bAllowTranscode: Boolean;
                          ppDecoder: IMFTransform): HRESULT;
var
  hr: HRESULT;
  count: UINT32;
  ppActivate: PIMFActivate;
  info: MFT_REGISTER_TYPE_INFO;
  unFlags: UINT32;

begin
  hr := S_OK;

  unFlags := MFT_ENUM_FLAG_SYNCMFT or
             MFT_ENUM_FLAG_LOCALMFT or
             MFT_ENUM_FLAG_SORTANDFILTER;

try

  info.guidMajorType := MFMediaType_Video;
  info.guidSubtype := subtype;

  if (bAllowAsync = True) then
    begin
      unFlags := unFlags or MFT_ENUM_FLAG_ASYNCMFT;
    end;

  if (bAllowHardware = True) then
    begin
      unFlags := unFlags or MFT_ENUM_FLAG_HARDWARE;
    end;

  if (bAllowTranscode = True) then
    begin
      unFlags := unFlags or MFT_ENUM_FLAG_TRANSCODE_ONLY;
    end;


  hr := MFTEnumEx(MFT_CATEGORY_VIDEO_DECODER,
                  unFlags,
                  @info,     // Input type
                  Nil,       // Output type
                  ppActivate,
                  count);

  if (SUCCEEDED(hr) and (count = 0)) then
    begin
      hr := MF_E_TOPO_CODEC_NOT_FOUND;
    end;

{$POINTERMATH ON}
  // Create the first decoder in the list.
  if (SUCCEEDED(hr)) then
    begin
      hr := ppActivate[0].ActivateObject(IID_IMFTransform,
                                         ppDecoder);
    end;
{$POINTERMATH OFF}

finally
  Result := hr;
end;
end;



function GetDecoderCategory(const majorType: TGUID;
                            out pCategory: TGUID): HRESULT;
var
  hr: HRESULT;

begin
  hr := S_OK;

  if (majorType = MFMediaType_Video) then
    begin
      pCategory := MFT_CATEGORY_VIDEO_DECODER;
    end
  else if (majorType = MFMediaType_Audio) then
    begin
      pCategory := MFT_CATEGORY_AUDIO_DECODER;
    end
  else
    begin
      hr := MF_E_INVALIDMEDIATYPE;
    end;

  Result := hr;
end;


function GetEncoderCategory(const majorType: TGUID;
                            out pCategory: TGUID): HRESULT;
var
  hr: HRESULT;

begin
  hr := S_OK;

  if (majorType = MFMediaType_Video) then
    begin
      pCategory := MFT_CATEGORY_VIDEO_ENCODER;
    end
  else if (majorType = MFMediaType_Audio) then
    begin
      pCategory := MFT_CATEGORY_AUDIO_ENCODER;
    end
  else
    begin
      hr := MF_E_INVALIDMEDIATYPE;
    end;

  Result := hr;
end;


//
function FindDecoderForStream(pSD: IMFStreamDescriptor; // Stream descriptor for the stream.
                              out opCLSID: CLSID): HRESULT;  // Receives the CLSID of the decoder.
var
  hr: HRESULT;
  bIsCompressed: BOOL;
  guidMajorType: TGUID;
  guidSubtype: TGUID;
  guidDecoderCategory: TGUID;
  ppDecoderCLSIDs: array of CLSID;
  cDecoderCLSIDs: UINT32;    // Size of the array.
  pHandler: IMFMediaTypeHandler;
  pMediaType: IMFMediaType;
  mftinfo: MFT_REGISTER_TYPE_INFO;

begin
  bIsCompressed := False;
  guidMajorType := GUID_NULL;
  guidSubtype := GUID_NULL;
  guidDecoderCategory := GUID_NULL;
  cDecoderCLSIDs := 0;


  // Find the media type for the stream.
  hr := pSD.GetMediaTypeHandler(pHandler);

  if (SUCCEEDED(hr)) then
    begin
      hr := pHandler.GetCurrentMediaType(pMediaType);
    end;

  // Get the major type and subtype.
  if (SUCCEEDED(hr)) then
    begin
      hr := pMediaType.GetMajorType(guidMajorType);
    end;

  if (SUCCEEDED(hr)) then
    begin
      hr := pMediaType.GetGUID(MF_MT_SUBTYPE,
                              guidSubtype);
    end;

  // Check whether the stream is compressed.
  if (SUCCEEDED(hr)) then
    begin
      hr := pMediaType.IsCompressedFormat(bIsCompressed);
    end;

//{$if WINVER < _WIN32_WINNT_WIN7}

  // Starting in Windows 7, you can connect an uncompressed video source
  // directly to the EVR. In earlier versions of Media Foundation, this
  // is not supported.

  //if (SUCCEEDED(hr)) then
  //  begin
  //    if ((bIsCompressed = True) And (guidMajorType = MFMediaType_Video)) then
  //      begin
  //        hr := MF_E_INVALIDMEDIATYPE;
  //      end;
  //  end;

//{$endif}

  // If the stream is compressed, find a decoder.
  if (SUCCEEDED(hr)) then
    begin
      if (bIsCompressed) then
        begin
          // Select the decoder category from the major type (audio/video).
          hr := GetDecoderCategory(guidMajorType,
                                   guidDecoderCategory);

          // Look for a decoder.
          if (SUCCEEDED(hr)) then
            begin
              mftinfo.guidMajorType := guidMajorType;
              mftinfo.guidSubtype := guidSubtype;

              hr := MFTEnum(guidDecoderCategory,
                            0,                   // Reserved
                            @mftinfo,            // Input type to match. (Encoded type.)
                            Nil,                 // Output type to match. (Don't care.)
                            Nil,                 // Attributes to match. (None.)
                            @ppDecoderCLSIDs[0], // Receives an array of CLSIDs.
                            cDecoderCLSIDs);     // Receives the size of the array.
            end;

          // MFTEnum can return zero matches.
          if (SUCCEEDED(hr) and (cDecoderCLSIDs = 0)) then
            begin
              hr := MF_E_TOPO_CODEC_NOT_FOUND;
            end;

          // Return the first CLSID in the list to the caller.
          if (SUCCEEDED(hr) and (cDecoderCLSIDs > 0)) then
            begin
              opCLSID := ppDecoderCLSIDs[0];
            end;
        end
      else
        begin
          // Uncompressed. A decoder is not required.
          opCLSID := GUID_NULL;
        end;
    end;
  Result := hr;
end;


// Enumerates a list of audio or video capture devices.
// Parameters:
// AttributeSourceType:  MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE_VIDCAP_GUID or MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE_AUDCAP_GUID
// DeviceProperties:  TDevicePropsA record
function EnumCaptureDeviceSources(AttributeSourceType: TGuid;
                                  out DeviceProperties: TDevicePropertiesArray): HRESULT;
var
  hr: HRESULT;
  pAttributes: IMFAttributes;
  ppDevices: PIMFActivate; // Pointer to array of IMFActivate
  count: UINT32;
  uiNameLen: UINT32;
  szName: LPWSTR;
  _i: integer;

label
  done;

begin

  uiNameLen := 0;

  // Create an attribute store to specify the enumeration parameters.
  hr := MFCreateAttributes(pAttributes,
                           0);
  if (FAILED(hr)) then
    begin
      goto done;
    end;

  hr := pAttributes.SetGUID(MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE,
                            AttributeSourceType); // Source type: video or audio capture devices

  if (FAILED(hr)) then
    begin
      goto done;
    end;

  // Enumerate devices.
  hr := MFEnumDeviceSources(pAttributes,
                            ppDevices,   // pointer to array of IMFActivate interfaces.
                            count);      // Number of elements (ppDevices)

  if (FAILED(hr)) then
    begin
      GetLastError();
      goto done;
    end;

  if (count = 0) then
    begin
      // Nothing found
      hr := MF_E_NOT_FOUND;
      goto done;
    end;

{$POINTERMATH ON}

  for _i := 0 to count -1 do
    begin

      // Try to get the display name.
      hr := ppDevices[_i].GetAllocatedString(MF_DEVSOURCE_ATTRIBUTE_FRIENDLY_NAME,
                                             szName,
                                             uiNameLen);

      if SUCCEEDED(hr) then
        begin
          //Set the new length
          SetLength(DeviceProperties, _i +1);
          // Add count
          DeviceProperties[_i].count := count;
          // Add the fiendlyname to the list.
          DeviceProperties[_i].uiDeviceIndex := _i;
          DeviceProperties[_i].sFriendlyName := String(szName);
          // Add the Interface identifier (IID) of the requested interface.
          DeviceProperties[_i].riid := AttributeSourceType;
        end;

       // Try to get the SymbolicLink name.
      if IsEqualGuid(AttributeSourceType, MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE_VIDCAP_GUID) then
        // Video
        hr := ppDevices[_i].GetAllocatedString(MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE_VIDCAP_SYMBOLIC_LINK,
                                               szName,
                                               uiNameLen)
      else
        // Audio
        hr := ppDevices[_i].GetAllocatedString(MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE_AUDCAP_SYMBOLIC_LINK,
                                               szName,
                                               uiNameLen);

      if SUCCEEDED(hr) then
        begin
          DeviceProperties[_i].lpSymbolicLink := szName;
        end;

      szName := Nil;

      if (FAILED(hr)) then
        goto done;
    end;

  // Normally this is the point where you will create the media source object.
  // Call it like this:
  // hr := ppDevices[0].ActivateObject(IID_IMFMediaSource,
  //                                   pSource);
  // and copy the retrieved source to your source var
  // ppSource := pSource;

  // As an alternative you could use
  // function ActivateObj(const DeviceProperty: TDevicePropsA): HRESULT;

done:
{$POINTERMATH OFF}
  Result := hr;

end;


function CreateCaptureDeviceInstance(DeviceProperty: TDeviceProperties;
                                     out ppSource: IMFMediaSource;
                                     out ppActivate: IMFActivate): HRESULT;
var
  count: UINT32;
  pConfig: IMFAttributes;
  ppDevices: PIMFActivate;  // Pointer to array of IMFActivate
  hr: HRESULT;

begin
  ppSource := Nil;
  ppActivate := Nil;
  count := 0;
  pConfig := Nil;
  hr := S_OK;

try

  // Create an attribute store to hold the search criteria.
  hr := MFCreateAttributes(pConfig,
                          0);

  // Request video capture devices.
  if SUCCEEDED(hr) then
    begin
      hr := pConfig.SetGUID(MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE,
                            DeviceProperty.riid);
    end;

  // Enumerate the devices again, because in the mean while a device could be
  // disconnected.
  if SUCCEEDED(hr) then
    begin
      hr := MFEnumDeviceSources(pConfig,
                                ppDevices,
                                count);
    end;

{$POINTERMATH ON}

  // Create a media source from the selected device.
  if SUCCEEDED(hr) then
    begin
      if (count > 0) then
        begin
          hr := ppDevices[DeviceProperty.uiDeviceIndex].ActivateObject(IID_IMFMediaSource,
                                                                       ppSource);
          if Succeeded(hr) then
            ppActivate := ppDevices[DeviceProperty.uiDeviceIndex];
        end
      else
        begin
          hr := MF_E_NOT_FOUND;
        end;
    end;

finally
{$POINTERMATH OFF}
  Result := hr;
end;
end;


//
function EnumerateCaptureFormats(pSource: IMFMediaSource): HRESULT;
var
  pPD: IMFPresentationDescriptor;
  pSD: IMFStreamDescriptor;
  pHandler: IMFMediaTypeHandler;
  pType: IMFMediaType;
  hr: HRESULT;
  fSelected: BOOL;
  cTypes: DWORD;
  i: Integer;

label
  done;

begin

  cTypes := 0;

  hr := pSource.CreatePresentationDescriptor(pPD);
  if FAILED(hr) then
    goto done;

  hr := pPD.GetStreamDescriptorByIndex(0,
                                       fSelected,
                                       pSD);
  if FAILED(hr) then
    goto done;

  hr := pSD.GetMediaTypeHandler(pHandler);
  if FAILED(hr) then
    goto done;

  hr := pHandler.GetMediaTypeCount(cTypes);
  if FAILED(hr) then
    goto done;

  for i := 0 to cTypes - 1 do
    begin
      hr := pHandler.GetMediaTypeByIndex(i,
                                         pType);
      if FAILED(hr) then
        goto done;

     // todo: LogMediaType(pType);
     // OutputDebugString(L"\n");
     // GetTypeName(TypeInfo(IMFMediaType))
    end;

done:
  Result := hr;
end;


function EnumerateTypesForStream(pReader: IMFSourceReader;
                                 dwStreamIndex: DWORD): HRESULT;
var
  hr: HRESULT;
  dwMediaTypeIndex: DWORD;
  pType: IMFMediaType;

begin
  hr := E_FAIL;

  dwMediaTypeIndex := MF_SOURCE_READER_CURRENT_TYPE_INDEX;

  while (SUCCEEDED(hr)) do
    begin
      pType := Nil;
      hr := pReader.GetNativeMediaType(dwStreamIndex,
                                       dwMediaTypeIndex, // MF_SOURCE_READER_CURRENT_TYPE_INDEX
                                       pType);

      if (hr = MF_E_NO_MORE_TYPES) then
        begin
          hr := S_OK;
          Break;
        end
      else if (SUCCEEDED(hr)) then
        begin

          //todo:  Examine the media type here. (Not implemented)

          SafeRelease(pType);
        end;
      inc(dwMediaTypeIndex);
    end;
  Result := hr;
end;


// Note: RTTI is not used, because it will not work on all Delphi versions.
//       So, we do it the alternative way.
function GetGUIDNameConst(const guid: TGuid): string;
var
  sGuidName: string;

  function IfEqualReturnGuidName(const gtype: TGuid;
                                 const sgName: string): Boolean;
    begin
      sGuidName := '';
      if IsEqualGuid(guid, gtype) then
        begin
          sGuidName := sgName;
          Result := True;
        end
      else
        Result := False;
    end;

label
  done;

begin

   if IfEqualReturnGuidName(MF_MT_MAJOR_TYPE, 'MF_MT_MAJOR_TYPE') then
     goto done;
   if IfEqualReturnGuidName(MF_MT_SUBTYPE, 'MF_MT_SUBTYPE') then
     goto done;
   if IfEqualReturnGuidName(MF_MT_ALL_SAMPLES_INDEPENDENT, 'MF_MT_ALL_SAMPLES_INDEPENDENT') then
     goto done;
   if IfEqualReturnGuidName(MF_MT_FIXED_SIZE_SAMPLES, 'MF_MT_FIXED_SIZE_SAMPLES') then
     goto done;
   if IfEqualReturnGuidName(MF_MT_COMPRESSED, 'MF_MT_COMPRESSED') then
     goto done;
   if IfEqualReturnGuidName(MF_MT_SAMPLE_SIZE, 'MF_MT_SAMPLE_SIZE') then
     goto done;
   if IfEqualReturnGuidName(MF_MT_WRAPPED_TYPE, 'MF_MT_WRAPPED_TYPE') then
     goto done;
   if IfEqualReturnGuidName(MF_MT_AUDIO_NUM_CHANNELS, 'MF_MT_AUDIO_NUM_CHANNELS') then
     goto done;
   if IfEqualReturnGuidName(MF_MT_AUDIO_SAMPLES_PER_SECOND, 'MF_MT_AUDIO_SAMPLES_PER_SECOND') then
     goto done;
   if IfEqualReturnGuidName(MF_MT_AUDIO_FLOAT_SAMPLES_PER_SECOND, 'MF_MT_AUDIO_FLOAT_SAMPLES_PER_SECOND') then
     goto done;
   if IfEqualReturnGuidName(MF_MT_AUDIO_AVG_BYTES_PER_SECOND, 'MF_MT_AUDIO_AVG_BYTES_PER_SECOND') then
     goto done;
   if IfEqualReturnGuidName(MF_MT_AUDIO_BLOCK_ALIGNMENT, 'MF_MT_AUDIO_BLOCK_ALIGNMENT') then
     goto done;
   if IfEqualReturnGuidName(MF_MT_AUDIO_BITS_PER_SAMPLE, 'MF_MT_AUDIO_BITS_PER_SAMPLE') then
     goto done;
   if IfEqualReturnGuidName(MF_MT_AUDIO_VALID_BITS_PER_SAMPLE, 'MF_MT_AUDIO_VALID_BITS_PER_SAMPLE') then
     goto done;
   if IfEqualReturnGuidName(MF_MT_AUDIO_SAMPLES_PER_BLOCK, 'MF_MT_AUDIO_SAMPLES_PER_BLOCK') then
     goto done;
   if IfEqualReturnGuidName(MF_MT_AUDIO_CHANNEL_MASK, 'MF_MT_AUDIO_CHANNEL_MASK') then
     goto done;
   if IfEqualReturnGuidName(MF_MT_AUDIO_FOLDDOWN_MATRIX, 'MF_MT_AUDIO_FOLDDOWN_MATRIX') then
     goto done;
   if IfEqualReturnGuidName(MF_MT_AUDIO_WMADRC_PEAKREF, 'MF_MT_AUDIO_WMADRC_PEAKREF') then
     goto done;
   if IfEqualReturnGuidName(MF_MT_AUDIO_WMADRC_PEAKTARGET, 'MF_MT_AUDIO_WMADRC_PEAKTARGET') then
     goto done;
   if IfEqualReturnGuidName(MF_MT_AUDIO_WMADRC_AVGREF, 'MF_MT_AUDIO_WMADRC_AVGREF') then
     goto done;
   if IfEqualReturnGuidName(MF_MT_AUDIO_WMADRC_AVGTARGET, 'MF_MT_AUDIO_WMADRC_AVGTARGET') then
     goto done;
   if IfEqualReturnGuidName(MF_MT_AUDIO_PREFER_WAVEFORMATEX, 'MF_MT_AUDIO_PREFER_WAVEFORMATEX') then
     goto done;
   if IfEqualReturnGuidName(MF_MT_AAC_PAYLOAD_TYPE, 'MF_MT_AAC_PAYLOAD_TYPE') then
     goto done;
   if IfEqualReturnGuidName(MF_MT_AAC_AUDIO_PROFILE_LEVEL_INDICATION, 'MF_MT_AAC_AUDIO_PROFILE_LEVEL_INDICATION') then
     goto done;
   if IfEqualReturnGuidName(MF_MT_FRAME_SIZE, 'MF_MT_FRAME_SIZE') then
     goto done;
   if IfEqualReturnGuidName(MF_MT_FRAME_RATE, 'MF_MT_FRAME_RATE') then
     goto done;
   if IfEqualReturnGuidName(MF_MT_FRAME_RATE_RANGE_MAX, 'MF_MT_FRAME_RATE_RANGE_MAX') then
     goto done;
   if IfEqualReturnGuidName(MF_MT_FRAME_RATE_RANGE_MIN, 'MF_MT_FRAME_RATE_RANGE_MIN') then
     goto done;
   if IfEqualReturnGuidName(MF_MT_PIXEL_ASPECT_RATIO, 'MF_MT_PIXEL_ASPECT_RATIO') then
     goto done;
   if IfEqualReturnGuidName(MF_MT_DRM_FLAGS, 'MF_MT_DRM_FLAGS') then
     goto done;
   if IfEqualReturnGuidName(MF_MT_PAD_CONTROL_FLAGS, 'MF_MT_PAD_CONTROL_FLAGS') then
     goto done;
   if IfEqualReturnGuidName(MF_MT_SOURCE_CONTENT_HINT, 'MF_MT_SOURCE_CONTENT_HINT') then
     goto done;
   if IfEqualReturnGuidName(MF_MT_VIDEO_CHROMA_SITING, 'MF_MT_VIDEO_CHROMA_SITING') then
     goto done;
   if IfEqualReturnGuidName(MF_MT_INTERLACE_MODE, 'MF_MT_INTERLACE_MODE') then
     goto done;
   if IfEqualReturnGuidName(MF_MT_TRANSFER_FUNCTION, 'MF_MT_TRANSFER_FUNCTION') then
     goto done;
   if IfEqualReturnGuidName(MF_MT_VIDEO_PRIMARIES, 'MF_MT_VIDEO_PRIMARIES') then
     goto done;
   if IfEqualReturnGuidName(MF_MT_CUSTOM_VIDEO_PRIMARIES, 'MF_MT_CUSTOM_VIDEO_PRIMARIES') then
     goto done;
   if IfEqualReturnGuidName(MF_MT_YUV_MATRIX, 'MF_MT_YUV_MATRIX') then
     goto done;
   if IfEqualReturnGuidName(MF_MT_VIDEO_LIGHTING, 'MF_MT_VIDEO_LIGHTING') then
     goto done;
   if IfEqualReturnGuidName(MF_MT_VIDEO_NOMINAL_RANGE, 'MF_MT_VIDEO_NOMINAL_RANGE') then
     goto done;
   if IfEqualReturnGuidName(MF_MT_GEOMETRIC_APERTURE, 'MF_MT_GEOMETRIC_APERTURE') then
     goto done;
   if IfEqualReturnGuidName(MF_MT_MINIMUM_DISPLAY_APERTURE, 'MF_MT_MINIMUM_DISPLAY_APERTURE') then
     goto done;
   if IfEqualReturnGuidName(MF_MT_PAN_SCAN_APERTURE, 'MF_MT_PAN_SCAN_APERTURE') then
     goto done;
   if IfEqualReturnGuidName(MF_MT_PAN_SCAN_ENABLED, 'MF_MT_PAN_SCAN_ENABLED') then
     goto done;
   if IfEqualReturnGuidName(MF_MT_AVG_BITRATE, 'MF_MT_AVG_BITRATE') then
     goto done;
   if IfEqualReturnGuidName(MF_MT_AVG_BIT_ERROR_RATE, 'MF_MT_AVG_BIT_ERROR_RATE') then
     goto done;
   if IfEqualReturnGuidName(MF_MT_MAX_KEYFRAME_SPACING, 'MF_MT_MAX_KEYFRAME_SPACING') then
     goto done;
   if IfEqualReturnGuidName(MF_MT_DEFAULT_STRIDE, 'MF_MT_DEFAULT_STRIDE') then
     goto done;
   if IfEqualReturnGuidName(MF_MT_PALETTE, 'MF_MT_PALETTE') then
     goto done;
   if IfEqualReturnGuidName(MF_MT_USER_DATA, 'MF_MT_USER_DATA') then
     goto done;
   if IfEqualReturnGuidName(MF_MT_AM_FORMAT_TYPE, 'MF_MT_AM_FORMAT_TYPE') then
     goto done;
   if IfEqualReturnGuidName(MF_MT_MPEG_START_TIME_CODE, 'MF_MT_MPEG_START_TIME_CODE') then
     goto done;
   if IfEqualReturnGuidName(MF_MT_MPEG2_PROFILE, 'MF_MT_MPEG2_PROFILE') then
     goto done;
   if IfEqualReturnGuidName(MF_MT_MPEG2_LEVEL, 'MF_MT_MPEG2_LEVEL') then
     goto done;
   if IfEqualReturnGuidName(MF_MT_MPEG2_FLAGS, 'MF_MT_MPEG2_FLAGS') then
     goto done;
   if IfEqualReturnGuidName(MF_MT_MPEG_SEQUENCE_HEADER, 'MF_MT_MPEG_SEQUENCE_HEADER') then
     goto done;
   if IfEqualReturnGuidName(MF_MT_DV_AAUX_SRC_PACK_0, 'MF_MT_DV_AAUX_SRC_PACK_0') then
     goto done;
   if IfEqualReturnGuidName(MF_MT_DV_AAUX_CTRL_PACK_0, 'MF_MT_DV_AAUX_CTRL_PACK_0') then
     goto done;
   if IfEqualReturnGuidName(MF_MT_DV_AAUX_SRC_PACK_1, 'MF_MT_DV_AAUX_SRC_PACK_1') then
     goto done;
   if IfEqualReturnGuidName(MF_MT_DV_AAUX_CTRL_PACK_1, 'MF_MT_DV_AAUX_CTRL_PACK_1') then
     goto done;
   if IfEqualReturnGuidName(MF_MT_DV_VAUX_SRC_PACK, 'MF_MT_DV_VAUX_SRC_PACK') then
     goto done;
   if IfEqualReturnGuidName(MF_MT_DV_VAUX_CTRL_PACK, 'MF_MT_DV_VAUX_CTRL_PACK') then
     goto done;
   if IfEqualReturnGuidName(MF_MT_ARBITRARY_HEADER, 'MF_MT_ARBITRARY_HEADER') then
     goto done;
   if IfEqualReturnGuidName(MF_MT_ARBITRARY_FORMAT, 'MF_MT_ARBITRARY_FORMAT') then
     goto done;
   if IfEqualReturnGuidName(MF_MT_IMAGE_LOSS_TOLERANT, 'MF_MT_IMAGE_LOSS_TOLERANT') then
     goto done;
   if IfEqualReturnGuidName(MF_MT_MPEG4_SAMPLE_DESCRIPTION, 'MF_MT_MPEG4_SAMPLE_DESCRIPTION') then
     goto done;
   if IfEqualReturnGuidName(MF_MT_MPEG4_CURRENT_SAMPLE_ENTRY, 'MF_MT_MPEG4_CURRENT_SAMPLE_ENTRY') then
     goto done;
   if IfEqualReturnGuidName(MF_MT_ORIGINAL_4CC, 'MF_MT_ORIGINAL_4CC') then
     goto done;
   if IfEqualReturnGuidName(MF_MT_ORIGINAL_WAVE_FORMAT_TAG, 'MF_MT_ORIGINAL_WAVE_FORMAT_TAG') then
     goto done;

   // Media types

   if IfEqualReturnGuidName(MFMediaType_Audio, 'MFMediaType_Audio') then
     goto done;
   if IfEqualReturnGuidName(MFMediaType_Video, 'MFMediaType_Video') then
     goto done;
   if IfEqualReturnGuidName(MFMediaType_Protected, 'MFMediaType_Protected') then
     goto done;
   if IfEqualReturnGuidName(MFMediaType_SAMI, 'MFMediaType_SAMI') then
     goto done;
   if IfEqualReturnGuidName(MFMediaType_Script, 'MFMediaType_Script') then
     goto done;
   if IfEqualReturnGuidName(MFMediaType_Image, 'MFMediaType_Image') then
     goto done;
   if IfEqualReturnGuidName(MFMediaType_HTML, 'MFMediaType_HTML') then
     goto done;
   if IfEqualReturnGuidName(MFMediaType_Binary, 'MFMediaType_Binary') then
     goto done;
   if IfEqualReturnGuidName(MFMediaType_FileTransfer, 'MFMediaType_FileTransfer') then
     goto done;

   // Video formats

   if IfEqualReturnGuidName(MFVideoFormat_AI44, 'MFVideoFormat_AI44') then //     FCC('AI44')
     goto done;
   if IfEqualReturnGuidName(MFVideoFormat_ARGB32, 'MFVideoFormat_ARGB32') then //   D3DFMT_A8R8G8B8
     goto done;
   if IfEqualReturnGuidName(MFVideoFormat_AYUV, 'MFVideoFormat_AYUV') then //     FCC('AYUV')
     goto done;
   if IfEqualReturnGuidName(MFVideoFormat_DV25, 'MFVideoFormat_DV25') then //     FCC('dv25')
     goto done;
   if IfEqualReturnGuidName(MFVideoFormat_DV50, 'MFVideoFormat_DV50') then //     FCC('dv50')
     goto done;
   if IfEqualReturnGuidName(MFVideoFormat_DVH1, 'MFVideoFormat_DVH1') then //     FCC('dvh1')
     goto done;
   if IfEqualReturnGuidName(MFVideoFormat_DVSD, 'MFVideoFormat_DVSD') then //     FCC('dvsd')
     goto done;
   if IfEqualReturnGuidName(MFVideoFormat_DVSL, 'MFVideoFormat_DVSL') then //     FCC('dvsl')
     goto done;
   if IfEqualReturnGuidName(MFVideoFormat_H264, 'MFVideoFormat_H264') then //     FCC('H264')
     goto done;
   if IfEqualReturnGuidName(MFVideoFormat_I420, 'MFVideoFormat_I420') then //     FCC('I420')
     goto done;
   if IfEqualReturnGuidName(MFVideoFormat_IYUV, 'MFVideoFormat_IYUV') then //     FCC('IYUV')
     goto done;
   if IfEqualReturnGuidName(MFVideoFormat_M4S2, 'MFVideoFormat_M4S2') then //     FCC('M4S2')
     goto done;
   if IfEqualReturnGuidName(MFVideoFormat_MJPG, 'MFVideoFormat_MJPG') then //     FCC('MJPG')
     goto done;
   if IfEqualReturnGuidName(MFVideoFormat_MP43, 'MFVideoFormat_MP43') then //     FCC('MP43')
     goto done;
   if IfEqualReturnGuidName(MFVideoFormat_MP4S, 'MFVideoFormat_MP4S') then //     FCC('MP4S')
     goto done;
   if IfEqualReturnGuidName(MFVideoFormat_MP4V, 'MFVideoFormat_MP4V') then //     FCC('MP4V')
     goto done;
   if IfEqualReturnGuidName(MFVideoFormat_MPG1, 'MFVideoFormat_MPG1') then //     FCC('MPG1')
     goto done;
   if IfEqualReturnGuidName(MFVideoFormat_MSS1, 'MFVideoFormat_MSS1') then //     FCC('MSS1')
     goto done;
   if IfEqualReturnGuidName(MFVideoFormat_MSS2, 'MFVideoFormat_MSS2') then //     FCC('MSS2')
     goto done;
   if IfEqualReturnGuidName(MFVideoFormat_NV11, 'MFVideoFormat_NV11') then //     FCC('NV11')
     goto done;
   if IfEqualReturnGuidName(MFVideoFormat_NV12, 'MFVideoFormat_NV12') then //     FCC('NV12')
     goto done;
   if IfEqualReturnGuidName(MFVideoFormat_P010, 'MFVideoFormat_P010') then //     FCC('P010')
     goto done;
   if IfEqualReturnGuidName(MFVideoFormat_P016, 'MFVideoFormat_P016') then //     FCC('P016')
     goto done;
   if IfEqualReturnGuidName(MFVideoFormat_P210, 'MFVideoFormat_P210') then //     FCC('P210')
     goto done;
   if IfEqualReturnGuidName(MFVideoFormat_P216, 'MFVideoFormat_P216') then //     FCC('P216')
     goto done;
   if IfEqualReturnGuidName(MFVideoFormat_RGB24, 'MFVideoFormat_RGB24') then //    D3DFMT_R8G8B8
     goto done;
   if IfEqualReturnGuidName(MFVideoFormat_RGB32, 'MFVideoFormat_RGB32') then //    D3DFMT_X8R8G8B8
     goto done;
   if IfEqualReturnGuidName(MFVideoFormat_RGB555, 'MFVideoFormat_RGB555') then //   D3DFMT_X1R5G5B5
     goto done;
   if IfEqualReturnGuidName(MFVideoFormat_RGB565, 'MFVideoFormat_RGB565') then //   D3DFMT_R5G6B5
     goto done;
   if IfEqualReturnGuidName(MFVideoFormat_RGB8, 'MFVideoFormat_RGB8') then
     goto done;
   if IfEqualReturnGuidName(MFVideoFormat_UYVY, 'MFVideoFormat_UYVY') then //     FCC('UYVY')
     goto done;
   if IfEqualReturnGuidName(MFVideoFormat_v210, 'MFVideoFormat_v210') then //     FCC('v210')
     goto done;
   if IfEqualReturnGuidName(MFVideoFormat_v410, 'MFVideoFormat_v410') then //     FCC('v410')
     goto done;
   if IfEqualReturnGuidName(MFVideoFormat_WMV1, 'MFVideoFormat_WMV1') then //     FCC('WMV1')
     goto done;
   if IfEqualReturnGuidName(MFVideoFormat_WMV2, 'MFVideoFormat_WMV2') then //     FCC('WMV2')
     goto done;
   if IfEqualReturnGuidName(MFVideoFormat_WMV3, 'MFVideoFormat_WMV3') then //     FCC('WMV3')
     goto done;
   if IfEqualReturnGuidName(MFVideoFormat_WVC1, 'MFVideoFormat_WVC1') then //     FCC('WVC1')
     goto done;
   if IfEqualReturnGuidName(MFVideoFormat_Y210, 'MFVideoFormat_Y210') then //     FCC('Y210')
     goto done;
   if IfEqualReturnGuidName(MFVideoFormat_Y216, 'MFVideoFormat_Y216') then //     FCC('Y216')
     goto done;
   if IfEqualReturnGuidName(MFVideoFormat_Y410, 'MFVideoFormat_Y410') then //     FCC('Y410')
     goto done;
   if IfEqualReturnGuidName(MFVideoFormat_Y416, 'MFVideoFormat_Y416') then //     FCC('Y416')
     goto done;
   if IfEqualReturnGuidName(MFVideoFormat_Y41P, 'MFVideoFormat_Y41P') then
     goto done;
   if IfEqualReturnGuidName(MFVideoFormat_Y41T, 'MFVideoFormat_Y41T') then
     goto done;
   if IfEqualReturnGuidName(MFVideoFormat_YUY2, 'MFVideoFormat_YUY2') then //     FCC('YUY2')
     goto done;
   if IfEqualReturnGuidName(MFVideoFormat_YV12, 'MFVideoFormat_YV12') then //     FCC('YV12')
     goto done;
   if IfEqualReturnGuidName(MFVideoFormat_YVYU, 'MFVideoFormat_YVYU') then
     goto done;

   // Audio formats

   if IfEqualReturnGuidName(MFAudioFormat_PCM, 'MFAudioFormat_PCM') then //              WAVE_FORMAT_PCM
     goto done;
   if IfEqualReturnGuidName(MFAudioFormat_Float, 'MFAudioFormat_Float') then //            WAVE_FORMAT_IEEE_FLOAT
     goto done;
   if IfEqualReturnGuidName(MFAudioFormat_DTS, 'MFAudioFormat_DTS') then //              WAVE_FORMAT_DTS
     goto done;
   if IfEqualReturnGuidName(MFAudioFormat_Dolby_AC3_SPDIF, 'MFAudioFormat_Dolby_AC3_SPDIF') then //  WAVE_FORMAT_DOLBY_AC3_SPDIF
     goto done;
   if IfEqualReturnGuidName(MFAudioFormat_DRM, 'MFAudioFormat_DRM') then //              WAVE_FORMAT_DRM
     goto done;
   if IfEqualReturnGuidName(MFAudioFormat_WMAudioV8, 'MFAudioFormat_WMAudioV8') then //        WAVE_FORMAT_WMAUDIO2
     goto done;
   if IfEqualReturnGuidName(MFAudioFormat_WMAudioV9, 'MFAudioFormat_WMAudioV9') then //        WAVE_FORMAT_WMAUDIO3
     goto done;
   if IfEqualReturnGuidName(MFAudioFormat_WMAudio_Lossless, 'MFAudioFormat_WMAudio_Lossless') then // WAVE_FORMAT_WMAUDIO_LOSSLESS
     goto done;
   if IfEqualReturnGuidName(MFAudioFormat_WMASPDIF, 'MFAudioFormat_WMASPDIF') then //         WAVE_FORMAT_WMASPDIF
     goto done;
   if IfEqualReturnGuidName(MFAudioFormat_MSP1, 'MFAudioFormat_MSP1') then //             WAVE_FORMAT_WMAVOICE9
     goto done;
   if IfEqualReturnGuidName(MFAudioFormat_MP3, 'MFAudioFormat_MP3') then //              WAVE_FORMAT_MPEGLAYER3
     goto done;
   if IfEqualReturnGuidName(MFAudioFormat_MPEG, 'MFAudioFormat_MPEG') then //             WAVE_FORMAT_MPEG
     goto done;
   if IfEqualReturnGuidName(MFAudioFormat_AAC, 'MFAudioFormat_AAC') then //              WAVE_FORMAT_MPEG_HEAAC
     goto done;
   if IfEqualReturnGuidName(MFAudioFormat_ADTS, 'MFAudioFormat_ADTS') then  //             WAVE_FORMAT_MPEG_ADTS_AA
     goto done;

done:
  Result := sGuidName;

end;


// Device Loss
// ===========
// Allways call UnregisterDeviceNotification (Windows) when finnished
function RegisterForDeviceNotification(hwnd: HWND; out g_hdevnotify: HDEVNOTIFY): HRESULT;
var
  di: DEV_BROADCAST_DEVICEINTERFACE;

begin

  di.dbcc_size := SizeOf(di);
  di.dbcc_devicetype := DBT_DEVTYP_DEVICEINTERFACE;
  di.dbcc_classguid := KSCATEGORY_CAPTURE;

  g_hdevnotify := RegisterDeviceNotification(hwnd,
                                             @di,
                                             DEVICE_NOTIFY_WINDOW_HANDLE);

  if (g_hdevnotify = Nil) then
    Result := E_FAIL // {include winerror for this} HRESULT_FROM_WIN32(GetLastError())  // or use HRESULT_FROM_NT()
  else
    Result := S_OK;
end;


function UnRegisterForDeviceNotification(g_hdevnotify: HDEVNOTIFY): HRESULT;
var
  hr : HResult;

begin
  hr := S_OK;
  if (g_hdevnotify <> Nil) then
    if UnregisterDeviceNotification(g_hdevnotify) then
      hr := S_OK
    else
      hr := E_FAIL;
  Result := hr;
end;

//
function GetSymbolicLink(pActivate: IMFActivate;
                         out g_pwszSymbolicLink: PWideChar;
                         out g_cchSymbolicLink: UINT32;
                         devMediaType: TGUID): HRESULT;
begin

  if IsEqualGuid(devMediaType, MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE_VIDCAP_SYMBOLIC_LINK) or
     IsEqualGuid(devMediaType, MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE_AUDCAP_SYMBOLIC_LINK) then
    begin
      Result := (pActivate as IMFAttributes).GetAllocatedString(devMediaType,
                                                                g_pwszSymbolicLink,
                                                                g_cchSymbolicLink);
    end
  else
    Result := E_FAIL;
end;

//
function FindDeviceManager(pTopology: IMFTopology;          // Topology to search.
                           out ppDeviceManager: IInterface;     // Receives a pointer to the device manager.
                           out ppNode: IMFTopologyNode): HRESULT;
var
  hr: HRESULT;
  cNodes: WORD;
  bFound: Boolean;
  pNode: IMFTopologyNode;
  pNodeObject: IInterface;  // = IUnknown !
  pD3DManager: IDirect3DDeviceManager9;
  i: integer;

begin

  cNodes := 0;
  bFound := False;
  pNode := Nil;
  pNodeObject := Nil;
  pD3DManager := Nil;

  // Search all of the nodes in the topology.

  hr := pTopology.GetNodeCount(cNodes);

  if FAILED(hr) then
    begin
      Result := hr;
      Exit;
    end;

  for i := 0 to cNodes - 1 do
    begin
      // For each of the following calls, failure just means we
      // did not find the node we're looking for, so keep looking.

      hr := pTopology.GetNode(i,
                             pNode);

      // Get the node's object pointer.
      if SUCCEEDED(hr) then
        begin
          hr := pNode.GetObject(pNodeObject);
        end;

      // Query the node object for the device manager service.
      if SUCCEEDED(hr) then
        begin
          hr := MFGetService(pNodeObject,
                             MR_VIDEO_ACCELERATION_SERVICE,
                             IID_IDirect3DDeviceManager9,
                             pD3DManager);
        end;

      if SUCCEEDED(hr) then
        begin
          // Found the right node. Return the pointers to the caller.
          ppDeviceManager := pD3DManager;
          ppNode := pNode;
          bFound := True;
          Break;
        end;

      SafeRelease(pNodeObject);
      SafeRelease(pD3DManager);
      SafeRelease(pNode);

    end; // End of for loop.

  if (bFound = True) then
    Result := S_OK
  else
    Result := E_FAIL;
end;


// Audio and video capture
//========================

// To enumerate the capture devices on the system, perform the following steps:
//  1 Call the MFCreateAttributes function to create an attribute store.
//  2 Set the MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE attribute to one of the following values:
//    Value	                                          Description
//    ----------------------------------------------  -------------------------------------
//    MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE_AUDCAP_GUID	Enumerate audio capture devices.
//    MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE_VIDCAP_GUID	Enumerate video capture devices.
//  3 Call the MFEnumDeviceSources function. This function allocates an array of IMFActivate pointers.
//    Each pointer represents an activation object for one device on the system.
//  4 Call the IMFActivate.ActivateObject method to create an instance of the media source from
//    one of the activation objects.
//
// The following fuction creates a media source for the given video capture device (iDeviceIndex) in
// the enumeration list:
//
function CreateVideoCaptureDevice(const iDeviceIndex: UINT32;
                                  out ppSource: IMFMediaSource): HRESULT;
var
  count: UINT32;
  pConfig: IMFAttributes;
  ppDevices: PIMFActivate; // Pointer to array of IMFActivate interfaces
  hr: HRESULT;

begin

  count := 0;

  // Create an attribute store to hold the search criteria.
  hr := MFCreateAttributes(pConfig,
                           1);

  // Request video capture devices.
  if SUCCEEDED(hr) then
    begin
      hr := pConfig.SetGUID(MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE,
                            MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE_VIDCAP_GUID);
    end;

  // Enumerate the devices,
  if SUCCEEDED(hr) then
    begin
      hr := MFEnumDeviceSources(pConfig,
                                ppDevices,
                                count);
    end;

{$POINTERMATH ON}

  // Create a media source for the first device in the list.
  if SUCCEEDED(hr) then
    begin
      if (count > 0) and (iDeviceIndex <= count) then
        begin
          hr := ppDevices[iDeviceIndex].ActivateObject(IID_IMFMediaSource,
                                                       ppSource);
        end
      else
        begin
          hr := MF_E_NOT_FOUND;
        end;
    end;

  ppDevices := Nil;

{$POINTERMATH OFF}
  Result := hr;
end;

//
function CreateVideoCaptureDevices(const pszSymbolicLink: LPCWSTR;
                                   out ppSource: IMFMediaSource): HRESULT;

var
  pAttributes: IMFAttributes;
  hr: HRESULT;

begin

  hr := MFCreateAttributes(pAttributes,
                           2);

  // Set the device type to video.
  if SUCCEEDED(hr) then
    begin
      hr := pAttributes.SetGUID(MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE,
                                MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE_VIDCAP_GUID);
    end;

  // Set the symbolic link.
  if SUCCEEDED(hr) then
    begin
      hr := pAttributes.SetString(MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE_VIDCAP_SYMBOLIC_LINK,
                                  LPCWSTR(pszSymbolicLink));
    end;

  if SUCCEEDED(hr) then
    begin
      hr := MFCreateDeviceSource(pAttributes,
                                 ppSource);
    end;

  Result := hr;
end;

// Equivalent way to create an audio device from the audio endpoint ID:
// 1 Call MFCreateAttributes to create an attribute store.
// 2 Set the MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE attribute to
//   MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE_AUDCAP_GUID.
// 3 Set the MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE_AUDCAP_ENDPOINT_ID attribute to the endpoint ID.
// 4 Call either the MFCreateDeviceSource or MFCreateDeviceSourceActivate function.
//
function CreateAudioCaptureDevice(const pszEndPointID: LPCWSTR;
                                  out ppSource: IMFMediaSource): HRESULT;
var
  pAttributes: IMFAttributes;
  hr: HRESULT;

begin

  hr := MFCreateAttributes(pAttributes,
                           2);

  // Set the device type to audio.
  if SUCCEEDED(hr) then
    begin
      hr := pAttributes.SetGUID(MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE,
                                MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE_AUDCAP_GUID);
    end;

  // Set the endpoint ID.
  if SUCCEEDED(hr) then
    begin
      hr := pAttributes.SetString(MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE_AUDCAP_ENDPOINT_ID,
                                  pszEndPointID);
    end;

  if SUCCEEDED(hr) then
    begin
      hr := MFCreateDeviceSource(pAttributes,
                                 ppSource);
    end;

  Result := hr;
end;

// You can query the activation objects for various attributes, including the following:
// * The MF_DEVSOURCE_ATTRIBUTE_FRIENDLY_NAME attribute contains the display name of the device.
//   The display name is suitable for showing to the user, but might not be unique.
// * For video devices, the MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE_VIDCAP_SYMBOLIC_LINK
//   attribute contains the symbolic link to the device.
//   The symbolic link uniquely identifies the device on the system, but is not a readable string.
// * For audio devices, the MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE_AUDCAP_ENDPOINT_ID
//   attribute contains the audio endpoint ID of the device.
//   The audio endpoint ID is similar to a symbolic link.
//   It uniquely identifies the device on the system, but is not a readable string.
//
// The following function takes an array of IMFActivate pointers and prints the
// lists the name of each device to the stringlist:
//
procedure ListDeviceNames(ppDevices: PIMFActivate;
                          out iList: TStringList);
var
  i: Integer;
  hr: HRESULT;
  szFriendlyName: LPWSTR;
  cchName: UINT32;
  iCount: UINT32;

begin
  cchName := 0;
  iList.Clear;

{$POINTERMATH ON}

  hr := ppDevices[0].GetCount(iCount);

  if SUCCEEDED(hr) then
    for i := 0 to iCount - 1 do
      begin
        szFriendlyName := '';

        // Try to get the display name.
        hr := (ppDevices[i] as IMFAttributes).GetAllocatedString(MF_DEVSOURCE_ATTRIBUTE_FRIENDLY_NAME,
                                                                 szFriendlyName,
                                                                 cchName);
        // Add the fiendlyname to the stringlist.
        if SUCCEEDED(hr) then
          iList.Append(szFriendlyName);

        szFriendlyName := Nil;

    end;
{$POINTERMATH OFF}
end;


function EnumAudioRenderingDevices(nDevice: UInt;
                                   out wstrID: PWideChar; // Device ID.
                                   pSink: IMFMediaSink = Nil;  // Streaming audio renderer (SAR)
                                   pActivate: IMFActivate = Nil // Activation object, which can be used to create the SAR.
                                   ): HResult;
var
  hr: HResult;
  pEnum: IMMDeviceEnumerator;       // Audio device enumerator.
  pDevices: IMMDeviceCollection;    // Audio device collection.
  pDevice: IMMDevice;               // An audio device.
  pAttributes: IMFAttributes;       // Attribute store.

begin

  // Create the device enumerator.
  hr := CoCreateInstance(CLSID_MMDeviceEnumerator,
                         Nil,
                         CLSCTX_ALL,
                         IID_IMMDeviceEnumerator,
                         pEnum);

  // Enumerate the rendering devices.
  if SUCCEEDED(hr) then
    hr := pEnum.EnumAudioEndpoints(eRender,
                                   DEVICE_STATE_ACTIVE,
                                   pDevices);

  // Get ID of the first device in the list by default or the nDevice value.
  if SUCCEEDED(hr) then
    hr := pDevices.Item(nDevice,
                        pDevice);

  if SUCCEEDED(hr) then
    hr := pDevice.GetId(wstrID);

  // Create the activation object for the SAR if needed ie when streaming protected content.
  if Assigned(pActivate) then
    begin
      hr := MFCreateAudioRendererActivate(pActivate);
      if SUCCEEDED(hr) then
        hr := pActivate.SetString(MF_AUDIO_RENDERER_ATTRIBUTE_ENDPOINT_ID,
                                  wstrID);
    end
  else  // For unprotected content
    begin
      // Create an attribute store and set the device ID attribute.
      if SUCCEEDED(hr) then
        hr := MFCreateAttributes(pAttributes,
                                 2);

      if SUCCEEDED(hr) then
        hr := pAttributes.SetString(MF_AUDIO_RENDERER_ATTRIBUTE_ENDPOINT_ID,
                                    wstrID);

      // Create the audio renderer.
      if SUCCEEDED(hr) then
        hr := MFCreateAudioRenderer(pAttributes,
                                    pSink);
    end;

  Result := hr;
  CoTaskMemFree(wstrID);
end;


function SetMaxFrameRate(pSource: IMFMediaSource; dwTypeIndex: DWORD): HRESULT;
var
  pPD: IMFPresentationDescriptor;
  pSD: IMFStreamDescriptor;
  pHandler: IMFMediaTypeHandler;
  pType: IMFMediaType;
  hr: HResult;
  fSelected: BOOL;
  propvar: PROPVARIANT;

label
  done;

begin
  hr := pSource.CreatePresentationDescriptor(pPD);
  if FAILED(hr) then
    goto done;

  hr := pPD.GetStreamDescriptorByIndex(dwTypeIndex,
                                       fSelected,
                                       pSD);
  if FAILED(hr) then
    goto done;

  hr := pSD.GetMediaTypeHandler(pHandler);
  if FAILED(hr) then
    goto done;

  hr := pHandler.GetCurrentMediaType(pType);
  if FAILED(hr) then
    goto done;

  // Get the maximum frame rate for the selected capture format.

  // Note: To get the minimum frame rate, use the
  // MF_MT_FRAME_RATE_RANGE_MIN attribute instead.

  if SUCCEEDED(pType.GetItem(MF_MT_FRAME_RATE_RANGE_MAX,
                             propvar)) then
    begin
      hr := pType.SetItem(MF_MT_FRAME_RATE,
                          propvar);

      PropVariantClear(propvar);

      if FAILED(hr) then
        goto done;

      hr := pHandler.SetCurrentMediaType(pType);
    end;

done:
  Result := hr;
end;









function GetBitmapInfoHeaderFromMFMediaType(pType: IMFMediaType;     // Pointer to the media type.
                                            out ppBmih: PBITMAPINFOHEADER; // Receives a pointer to the structure.
                                            out pcbSize: DWORD // Receives the size of the structure.
                                            ): HRESULT;
var
  majorType: TGUID;
  pmt: PAM_MEDIA_TYPE;
  cbSize: DWORD;
  cbOffset: DWORD;
  pBMIH: PBITMAPINFOHEADER;
  hr: HRESULT;
  ulcbOffset: ULONG;

label
  done;

begin
  pBMIH := Nil;
  ppBmih := Nil;
  pcbSize := 0;

  majorType := GUID_NULL;
  pmt := Nil;

  // Verify that this is a video type.
  hr := pType.GetMajorType(majorType);
  if FAILED(hr) then
    goto done;

  if not isEqualGuid(majorType,
                     MFMediaType_Video) then
    begin
      hr := MF_E_INVALIDMEDIATYPE;
      goto done;
    end;

  hr := pType.GetRepresentation(AM_MEDIA_TYPE_REPRESENTATION,
                                pmt);
  if FAILED(hr) then
    goto done;


  // To avoid use of RTTI (to make a kind of FIELD_OFFSET work) we have have luck, because
  // both  recordfields are at the end of the VIDEOINFOHEADER(2) records.
  // This makes calculating the recordfieldoffset a bit easier.
  if IsEqualGuid(pmt.formattype, FORMAT_VideoInfo) then
    cbOffset := SizeOf(VIDEOINFOHEADER) - SizeOf(WinApi.AmVideo.VIDEOINFOHEADER)
  else if (pmt.formattype = FORMAT_VideoInfo2) then
    cbOffset := SizeOf(VIDEOINFOHEADER2) - SizeOf(WinApi.DvdMedia.VIDEOINFOHEADER2)
  else
    begin
      hr := MF_E_INVALIDMEDIATYPE; // Unsupported format type.
      goto done;
    end;

    if ((pmt.cbFormat - cbOffset) < SizeOf(BITMAPINFOHEADER)) then
      begin
        hr := E_UNEXPECTED; // Bad format size.
        goto done;
      end;

    cbSize := (pmt.cbFormat - cbOffset);

    pBMIH := CoTaskMemAlloc(cbSize);
    if (pBMIH = Nil) then
      begin
        hr := E_OUTOFMEMORY;
        goto done;
      end;

    ulcbOffset := (pmt.cbFormat + cbOffset);

    CopyMemory(pBMIH,
               @ulcbOffset,
               cbSize);

    ppBmih := pBMIH;
    pcbSize := cbSize;

done:
  if assigned(pmt) then
    pType.FreeRepresentation(AM_MEDIA_TYPE_REPRESENTATION,
                             pmt);
  if Assigned(pBMIH) then
    CoTaskMemFree(pBMIH);

  Result := hr;

end;


//
function CopyAttribute(pSrc: IMFAttributes;
                       pDest: IMFAttributes;
                       key: TGUID): HRESULT;
var
  pvar: PROPVARIANT;
  hr: HRESULT;

begin

  PropVariantInit(pvar);

  hr := pSrc.GetItem(key,
                     pvar);

  if (SUCCEEDED(hr)) then
    begin
      hr := pDest.SetItem(key,
                          pvar);
      PropVariantClear(pvar);
    end;

  Result := hr;

end;



//
function CloneVideoMediaType(var pSrcMediaType: IMFMediaType;
                             guidSubType: REFGUID;
                             out ppNewMediaType: IMFMediaType): HRESULT;
var
  hr: HRESULT;

label
  done;

begin
  ppNewMediaType := Nil;

  hr := MFCreateMediaType(ppNewMediaType);
  if (FAILED(hr)) then
    goto done;


  hr := ppNewMediaType.SetGUID(MF_MT_MAJOR_TYPE,
                               MFMediaType_Video);
  if (FAILED(hr)) then
    goto done;

  hr := ppNewMediaType.SetGUID(MF_MT_SUBTYPE,
                               guidSubType);
  if (FAILED(hr)) then
    goto done;

  hr := CopyAttribute(pSrcMediaType,
                      ppNewMediaType,
                      MF_MT_FRAME_SIZE);
  if (FAILED(hr)) then
    goto done;

  hr := CopyAttribute(pSrcMediaType,
                      ppNewMediaType,
                      MF_MT_FRAME_RATE);
  if (FAILED(hr)) then
    goto done;

  hr := CopyAttribute(pSrcMediaType,
                      ppNewMediaType,
                      MF_MT_PIXEL_ASPECT_RATIO);
  if (FAILED(hr)) then
    goto done;

  hr := CopyAttribute(pSrcMediaType,
                      ppNewMediaType,
                      MF_MT_INTERLACE_MODE);
  if (FAILED(hr)) then
    goto done;

done:
  Result := hr;
end;


//
function CreatePhotoMediaType(pSrcMediaType: IMFMediaType;
                              out ppPhotoMediaType: IMFMediaType): HRESULT;
const
  uiFrameRateNumerator   = UINT32(30);
  uiFrameRateDenominator = UINT32(1);

var
  hr: HRESULT;

label
  done;

begin
  ppPhotoMediaType := Nil;

  hr := MFCreateMediaType(ppPhotoMediaType);
  if (FAILED(hr)) then
    goto done;

  hr := ppPhotoMediaType.SetGUID(MF_MT_MAJOR_TYPE,
                                 MFMediaType_Image);
  if (FAILED(hr)) then
    goto done;

  hr := ppPhotoMediaType.SetGUID(MF_MT_SUBTYPE,
                                 MFImageFormat_JPEG { = GUID_ContainerFormatJpeg});
  if (FAILED(hr)) then
    goto done;

  hr := CopyAttribute(pSrcMediaType,
                      ppPhotoMediaType,
                      MF_MT_FRAME_SIZE);
  if (FAILED(hr)) then
    goto done;

done:
  Result := hr;
end;



function GetFrameRate(pAttributes: IMFAttributes;
                      out uiNumerator: UINT32;
                      out uiDenominator: UINT32): HResult;
begin
  Result := MFGetAttributeRatio(pAttributes,
                                MF_MT_FRAME_RATE,
                                uiNumerator,
                                uiDenominator);
end;


function SetFrameRate(pAttributes: IMFAttributes;
                      uiNumerator: UINT32;
                      uiDenominator: UINT32): HResult;
begin
  Result := MFSetAttributeRatio(pAttributes,
                                MF_MT_FRAME_RATE,
                                uiNumerator,
                                uiDenominator);
end;

//
function GetFrameSize(pAttributes: IMFAttributes;
                      out uiWidth: UINT32;
                      out uiHeigth: UINT32): HResult; inline;
begin
  Result := MFGetAttributeSize(pAttributes,
                               MF_MT_FRAME_SIZE,
                               uiWidth,
                               uiHeigth);
end;

//
function GetFrameSizeFromMediaType(pType: IMFMediaType;
                                   out uiWidth: UINT32;
                                   out uiHeight: UINT32): HResult; inline;
begin
  Result := MFGetAttributeSize(pType,
                               MF_MT_FRAME_SIZE,
                               uiWidth,
                               uiHeight);
end;

//
function SetFrameSize(pAttributes: IMFAttributes;
                      uiWidth: UINT32;
                      uiHeigth: UINT32): HResult; inline;
begin
  Result := MFSetAttributeSize(pAttributes,
                               MF_MT_FRAME_SIZE,
                               uiWidth,
                               uiHeigth);
end;

//
function SetFrameSizeOnMediaType(pType: IMFMediaType;
                                   uiWidth: UINT32;
                                   uiHeigth: UINT32): HResult; inline;
begin
  Result := MFSetAttributeSize(pType,
                               MF_MT_FRAME_SIZE,
                               uiWidth,
                               uiHeigth);
end;

//
function GetPixelAspectRatio(pAttributes: IMFAttributes;
                             out uiNumerator: UINT32;
                             out uiDenominator: UINT32): HResult;
begin
  Result := MFGetAttributeRatio(pAttributes,
                                MF_MT_PIXEL_ASPECT_RATIO,
                                uiNumerator,
                                uiDenominator);
end;


//
function SetPixelAspectRatio(pAttributes: IMFAttributes;
                             uiNumerator: UINT32;
                             uiDenominator: UINT32): HResult;
begin
  Result := MFGetAttributeRatio(pAttributes,
                                MF_MT_PIXEL_ASPECT_RATIO,
                                uiNumerator,
                                uiDenominator);
end;

// MF_MT_PAD_CONTROL_FLAGS Specifies the aspect ratio of the output rectangle for a video media type.
function SetOutputRectangleAspectRatio(pAttributes: IMFAttributes;
                                       stVideoPadFlags: MFVideoPadFlags = MFVideoPadFlag_PAD_TO_None): HResult;
begin
  Result := MFGetAttributeUINT32(pAttributes,
                                 MF_MT_PAD_CONTROL_FLAGS,
                                 UINT32(stVideoPadFlags));
end;



// The following example shows how to get an IMFMetadata pointer from a media source.
// Metadata contains descriptive information for the media content, such as title, artist, composer, and genre.
// Metadata can also describe encoding parameters.
// It can be faster to access this information through metadata than through media-type attributes.
function GetMetadata(pSource: IMFMediaSource;
                     out ppMetadata: IMFMetadata;
                     dwStream: DWORD): HResult;
var
  hr: HRESULT;
  pPD: IMFPresentationDescriptor;
  pProvider: IMFMetadataProvider;

label
  done;

begin

  hr := pSource.CreatePresentationDescriptor(pPD);
  if FAILED(hr) then
    goto done;


  hr := MFGetService(pSource,
                     MF_METADATA_PROVIDER_SERVICE,
                     IID_IMFMetadataProvider,
                     pProvider);

  if FAILED(hr) then
    goto done;

  hr := pProvider.GetMFMetadata(pPD,
                                dwStream,
                                0,
                                ppMetadata);

done:
  Result := hr;
end;



// Returns the stream identifier from an active stream.
function GetActiveStreamIndex(stmediaType: TMediaTypes;          // [in] mediatype
                              pspd: IMFPresentationDescriptor;   // [in] presentation descriptor interface
                              out dwStreamId: DWORD): HResult;   // [out] stream identifier
var
  hr: HRESULT;
  sdCount: DWORD;
  i: Integer;
  pSourceSD: IMFStreamDescriptor;
  pHandler: IMFMediaTypeHandler;
  bSelected: BOOL;
  gStreamMajorType: TGuid;
  tmpMediaType: TMediaTypes;

begin

  hr := S_OK;

  // Check if IMFPresentationDescriptor is initialized
  if assigned(pspd) then
    begin
      // Count the streams
      hr := pspd.GetStreamDescriptorCount(sdCount);

      // Itterate through streams to get the given stream result
      for i := 0 to sdCount - 1 do
        begin
          //
          hr := pspd.GetStreamDescriptorByIndex(i,          // Zero-based index of the stream.
                                                bSelected,  // TRUE if the stream is currently selected, FALSE if the stream is currently deselected.
                                                pSourceSD); // Receives a pointer to the stream descriptor's IMFStreamDescriptor interface. The caller must release the interface.

          if SUCCEEDED(hr) and (bSelected = True) then
            begin
              // Get the media type handler interface
              hr := pSourceSD.GetMediaTypeHandler(pHandler);

              // Get Major guid from the stream
              if SUCCEEDED(hr) then
                pHandler.GetMajorType(gStreamMajorType);

              // Figure out what media type we are dealing with
              if SUCCEEDED(hr) then
                GetMediaDescription(gStreamMajorType,
                                    tmpMediaType);

              // Return streamID when both params match
              if (tmpMediaType = stmediaType) then
                begin
                  dwStreamId := i;
                  Break;
                end;
            end;
          SafeRelease(pSourceSD);
          SafeRelease(pHandler);
        end;
    end;
  Result := hr;
end;


// Retrieves information of the streams from a source
function GetStreamContents(pspd: IMFPresentationDescriptor;
                           mSource: IMFMediaSource;
                           var alsCont: TStreamContentsArray): HRESULT;
var
  hr: HRESULT;
  i: Integer;
  pSourceSD: IMFStreamDescriptor;
  pMediaTypeHandler: IMFMediaTypeHandler;
  pMediaType: IMFMediaType;
  pwszValue: LPWSTR;
  pcchLength,
  uiNumerator,
  uiDenominator,
  uiHeigth,
  uiWidth: UINT32;
  sdCount: DWORD;

begin

  SetLength(alsCont, 0);
  sdCount := 0;
  pcchLength := 0;
  hr := S_OK;

try
try
  // Check if IMFPresentationDescriptor is initialized
  if assigned(pspd) then
    begin
      // Count streams
      hr := pspd.GetStreamDescriptorCount(sdCount);

      SetLength(alsCont, sdCount);

      for i := 0 to sdCount - 1 do
        begin
           // Initialize the record
           alsCont[i].Init();

           // Store the stream index
           alsCont[i].dwStreamIndex := i;

          // Get stream descriptor interface
          hr := pspd.GetStreamDescriptorByIndex(i,                    // Zero-based index of the stream.
                                                alsCont[i].bSelected, // TRUE if the stream is currently selected, FALSE if the stream is currently deselected.
                                                pSourceSD);           // Receives a pointer to the stream descriptor's IMFStreamDescriptor interface. The caller must release the interface.

          // Store the streamID
          if SUCCEEDED(hr) then
            pSourceSD.GetStreamIdentifier(alsCont[i].dwStreamId);

          // Get the media major type
          if SUCCEEDED(hr) then
            hr := GetMediaType(pSourceSD,
                               alsCont[i].idStreamMajorTypeGuid,
                               alsCont[i].bCompressed);


          // Figure out what media type we are dealing with
          if SUCCEEDED(hr) then
            hr := GetMediaDescription(alsCont[i].idStreamMajorTypeGuid,
                                      alsCont[i].idStreamMediaType);


          // If audio stream then try to get the language of this stream
          if SUCCEEDED(hr) and (alsCont[i].idStreamMediaType = mtAudio) then
            begin

              // Get the audio format type and qualities
              hr := GetAudioSubType(mSource,
                                    alsCont[i].idStreamSubTypeGuid,
                                    alsCont[i].audio_dwFormatTag,
                                    alsCont[i].audio_wsAudioDescr,
                                    alsCont[i].audio_iAudioChannels,
                                    alsCont[i].audio_iSamplesPerSec,
                                    alsCont[i].audio_iBitsPerSample);


              // Retrieves a wide-character string associated with a key (MF_SD_LANGUAGE).
              // This method allocates the memory for the string.
              // A returnvalue of -1072875802 / $C00D36E6
              // (The requested attribute was not found.) is returned when no language information was found.
              hr := pSourceSD.GetAllocatedString(MF_SD_LANGUAGE,
                                                 pwszValue,
                                                 pcchLength);


              if SUCCEEDED(hr) then
                alsCont[i].audio_lpLangShortName := pwszValue
              else
                begin
                  alsCont[i].audio_lpLangShortName := 'Not available';
                  hr := S_OK;
                end;
            end;

          pwszValue := Nil;
          pcchLength := 0;
          // Retrieves a wide-character string associated with a key (MF_SD_STREAM_NAME)
          // If a stream is not provided with a name the Hresult will be MF_E_ATTRIBUTENOTFOUND.
          hr := pSourceSD.GetAllocatedString(MF_SD_STREAM_NAME,
                                            pwszValue,
                                            pcchLength);
          if SUCCEEDED(hr) then
            alsCont[i].audio_lpStreamName := pwszValue
          else
            begin
              alsCont[i].audio_lpStreamName := 'Not available';
              hr := S_OK;
            end;
          // Note:
          // Set your initial preffered language somewhere in the caller.
          // hr := pspd.DeselectStream(dwDescriptorIndex);
          //
          // hr := pspd.SelectStream(iMySelectedLanguage);
          // finally set the new topology.

          // If video stream then try to get the properties of this stream
          if SUCCEEDED(hr) and (alsCont[i].idStreamMediaType = mtVideo) then
            begin
              hr := pSourceSD.GetMediaTypeHandler(pMediaTypeHandler);
              hr := pMediaTypeHandler.GetCurrentMediaType(pMediaType);

              // Get the video frame rate
              // To calculate the framerate in FPS : Single(uiNumerator / uiDenominator)
              hr := GetFrameRate(pMediaType,
                                 uiNumerator,
                                 uiDenominator);
              alsCont[i].video_FrameRateNumerator := uiNumerator;
              alsCont[i].video_FrameRateDenominator := uiDenominator;

              // Get the pixel aspect ratio
              // To calculate the pixel aspect ratio: Single(uiNumerator / uiDenominator)
              hr := GetPixelAspectRatio(pMediaType,
                                        uiNumerator,
                                        uiDenominator);
              alsCont[i].video_PixelAspectRatioNumerator := uiNumerator;
              alsCont[i].video_PixelAspectRatioDenominator := uiDenominator;

              // Get the video frame size
              hr := GetFrameSize(pMediaType,
                                 uiWidth,
                                 uiHeigth);
              alsCont[i].video_FrameSizeWidth := uiWidth;
              alsCont[i].video_FrameSizeHeigth := uiHeigth;
            end;
          SafeRelease(pSourceSD);
        end;
    end;

except
  hr := E_POINTER;
end;
finally
  CoTaskMemFree(pwszValue);
  Result := hr;
end;
end;


// Shows how to get the media type handler, enumerate the preferred media types, and set the media type.
function GetMediaType(pStreamDesc: IMFStreamDescriptor;
                      out tgMajorGuid: TGuid;
                      out bIsCompressedFormat: BOOL): HRESULT;
var
  hr: HRESULT;
  cTypes: DWORD;
  pHandler: IMFMediaTypeHandler;
  pMediaType: IMFMediaType;
  iType: DWORD;

begin

  cTypes := 0;
  tgMajorGuid := GUID_NULL;

  hr := pStreamDesc.GetMediaTypeHandler(pHandler);

  if SUCCEEDED(hr) then
    hr := pHandler.GetMediaTypeCount(cTypes);

  if SUCCEEDED(hr) then
    begin

      for iType := 0 to cTypes -1 do
        begin
          hr := pHandler.GetMediaTypeByIndex(iType,
                                            pMediaType);

          if FAILED(hr) then
            break;

          // Examine the media type.
          // here you have to examine the GetMajorType method
          // for major types that will give you information about video, audio etc.
          hr := pMediaType.GetMajorType(tgMajorGuid);

          // Check if it's a compressed format.
          if SUCCEEDED(hr) then
            hr := pMediaType.IsCompressedFormat(bIsCompressedFormat);
          SafeRelease(pMediaType);
        end;
   end;
 Result := hr;
end;


//
function GetMediaDescription(pMajorGuid: TGuid;
                             out mtMediaType: TMediaTypes): HRESULT;
var
  hr: HRESULT;

begin
  hr := S_OK;

  if isEqualGuid(pMajorGuid, MFMediaType_Default) then
    mtMediaType := mtDefault
  else if isEqualGuid(pMajorGuid, MFMediaType_Audio) then
    mtMediaType := mtAudio
  else if isEqualGuid(pMajorGuid, MFMediaType_Video) then
    mtMediaType := mtVideo
  else if isEqualGuid(pMajorGuid, MFMediaType_Protected) then
    mtMediaType := mtProtectedMedia
  else if isEqualGuid(pMajorGuid, MFMediaType_SAMI) then
    mtMediaType := mtSAMI
  else if isEqualGuid(pMajorGuid, MFMediaType_Script) then
    mtMediaType := mtScript
  else if isEqualGuid(pMajorGuid, MFMediaType_Image) then
    mtMediaType := mtStillImage
  else if isEqualGuid(pMajorGuid, MFMediaType_HTML) then
    mtMediaType := mtHTML
  else if isEqualGuid(pMajorGuid, MFMediaType_Binary) then
    mtMediaType := mtBinary
  else if isEqualGuid(pMajorGuid, MFMediaType_FileTransfer) then
    mtMediaType := mtFileTransfer
  else if isEqualGuid(pMajorGuid, MFMediaType_Stream) then
    mtMediaType := mtStream
  else if isEqualGuid(pMajorGuid, MFMediaType_MultiplexedFrames) then
    mtMediaType := mtMultiplexedFrames
  else if isEqualGuid(pMajorGuid, MFMediaType_Subtitle) then
    mtMediaType := mtSubTitle
  else if isEqualGuid(pMajorGuid, MFMediaType_Perception) then
    mtMediaType := mtPerception
  else
    begin
      mtMediaType := mtUnknown;
      hr := MF_E_INVALIDMEDIATYPE;
    end;
  Result := hr;
end;

//-------------------------------------------------------------------
// ConvertAudioTypeToPCM
//
// Given an audio media type (which might describe a compressed audio
// format), returns a media type that describes the equivalent
// uncompressed PCM format.
//-------------------------------------------------------------------

function GetAudioSubType(mSource: IMFMediaSource;
                         out gSubType: TGUID;
                         out FormatTag: DWord;
                         out wsDescr: Widestring;
                         out cChannels: UINT32;
                         out samplesPerSec: UINT32;
                         out bitsPerSample: UINT32): HRESULT;

var
  hr: HResult;
  majortype: TGUID;
  subtype: TGUID;
  pPD: IMFPresentationDescriptor;
  pSD: IMFStreamDescriptor;
  pHandler: IMFMediaTypeHandler;
  pType: IMFMediaType;
  cTypes: DWORD;
  i, j: DWORD;
  bSelected: BOOL;
  sGuid: string;
  sDescr: Widestring;

label done;

begin
  majortype := GUID_NULL;
  subtype := GUID_NULL;
  gSubType := subtype;
  cChannels := 0;
  samplesPerSec := 0;
  bitsPerSample := 0;
  cTypes := 0;
  i := 0;

  //
  repeat

  sGuid := '';
  sDescr := '';

  hr := mSource.CreatePresentationDescriptor(pPD);
  if FAILED(hr) then
    goto done;

  hr := pPD.GetStreamDescriptorByIndex(i,
                                      bSelected,
                                      pSD);
  if FAILED(hr) then
    goto done;

  hr := pSD.GetMediaTypeHandler(pHandler);
  if FAILED(hr) then
    goto done;

  hr := pHandler.GetMediaTypeCount(cTypes);
  if FAILED(hr) then
    goto done;

  // find the proper subtype
  for j := 0 to cTypes-1 do
    begin

      hr := pHandler.GetMediaTypeByIndex(j,
                                        pType);
      if FAILED(hr) then
        goto done;

      hr := pType.GetMajorType(majortype);
        if (FAILED(hr)) then
          goto done;

      if IsEqualGuid(majortype,
                     MFMediaType_Audio) then
        begin

          // Get the audio subtype. If not, skip.
          hr := pType.GetGUID(MF_MT_SUBTYPE,
                              subtype);
          if (FAILED(hr)) then
            goto done;

          // readable audiosubtype guid
          sGuid := GuidToString(subtype);

          // Get description by guid   (this is just a short list of most common audioformats)
          // You may want to extend the list with audiosubtypes.
          if IsEqualGuid(subtype, MFAudioFormat_PCM) then  //D1: WAVE_FORMAT_PCM;
            begin
              sDescr := 'Uncompressed PCM audio.';
              FormatTag := WAVE_FORMAT_PCM;
            end
          else if IsEqualGuid(subtype, MFAudioFormat_Float) then //(D1: WAVE_FORMAT_IEEE_FLOAT;
            begin
              sDescr := 'Uncompressed IEEE floating-point audio.';
              FormatTag := WAVE_FORMAT_IEEE_FLOAT;
            end
          else if IsEqualGuid(subtype, MFAudioFormat_DTS) then  //(D1: WAVE_FORMAT_DTS;
            begin
              sDescr := 'Microsoft DTS.';
              FormatTag := WAVE_FORMAT_DTS;
            end
          else if IsEqualGuid(subtype, MEDIASUBTYPE_DOLBY_TRUEHD) then // Not derived from an existing wFormatTag
            begin
              sDescr := 'Dolby Digital (AC-3) True High Definition.';
              FormatTag := 0;
            end
          else if IsEqualGuid(subtype, MFAudioFormat_Dolby_AC3) then // Not derived from an existing wFormatTag
            begin
              sDescr := 'Dolby Digital (AC-3).';
              FormatTag := 0;
            end
          else if IsEqualGuid(subtype, MEDIASUBTYPE_DVM) then // Not derived from an existing wFormatTag
            begin
              sDescr := 'DVM AC-3 codec. Used when playing AVI files with Dolby Digital Audio.';
              FormatTag := WAVE_FORMAT_DVM;
            end
          else if IsEqualGuid(subtype, MFAudioFormat_Dolby_AC3_SPDIF) then //(D1: WAVE_FORMAT_DOLBY_AC3_SPDIF;
            begin
              sDescr := 'Dolby AC-3 audio over Sony/Philips Digital Interface (S/PDIF).';
              FormatTag := WAVE_FORMAT_DOLBY_AC3_SPDIF;
            end
          else if IsEqualGuid(subtype, MFAudioFormat_Dolby_AC3_HDCP) then
            begin
              sDescr := 'Dolby AC-3. (HDCP)';
              FormatTag := 0;
            end
          else if IsEqualGuid(subtype, MFAudioFormat_Dolby_DDPlus) then
            begin
              sDescr := 'Dolby Digital Plus.';
              FormatTag := 0;
            end
          else if IsEqualGuid(subtype, MFAudioFormat_DRM) then //(D1: WAVE_FORMAT_DRM;
            begin
              sDescr := 'Audio Digital Rights Management codec';
              FormatTag := WAVE_FORMAT_DRM;
            end
          else if IsEqualGuid(subtype, MFAudioFormat_WMAudioV8) then //(D1: WAVE_FORMAT_WMAUDIO2;
            begin
              sDescr := 'Windows Media Audio 8 codec, Windows Media Audio 9 codec, or Windows Media Audio 9.1 codec.';
              FormatTag := WAVE_FORMAT_WMAUDIO2;
            end
          else if IsEqualGuid(subtype, MFAudioFormat_WMAudioV9) then //(D1: WAVE_FORMAT_WMAUDIO3;
            begin
              sDescr := 'Windows Media Audio 9 Professional codec or Windows Media Audio 9.1 Professional codec.';
              FormatTag := WAVE_FORMAT_WMAUDIO3;
            end
          else if IsEqualGuid(subtype, MFAudioFormat_WMAudio_Lossless) then  //(D1: WAVE_FORMAT_WMAUDIO_LOSSLESS;
            begin
              sDescr := 'Windows Media Audio 9 Lossless codec or Windows Media Audio 9.1 codec.';
              FormatTag := WAVE_FORMAT_WMAUDIO_LOSSLESS;
            end
          else if IsEqualGuid(subtype, MFAudioFormat_WMASPDIF) then //(D1: WAVE_FORMAT_WMASPDIF;
            begin
              sDescr := 'Windows Media Audio S/PDIF.';
              FormatTag := WAVE_FORMAT_WMASPDIF;
            end
          else if IsEqualGuid(subtype, MFAudioFormat_MSP1) then  // (D1: WAVE_FORMAT_WMAVOICE9;
            begin
              sDescr := 'Windows Media Audio 9 Voice codec.';
              FormatTag := WAVE_FORMAT_WMAVOICE9;
            end
          else if IsEqualGuid(subtype, MFAudioFormat_MP3) then  // (D1: WAVE_FORMAT_MPEGLAYER3;
            begin
              sDescr := 'MPEG Audio Layer-3 (MP3).';
              FormatTag := WAVE_FORMAT_MPEGLAYER3;
            end
          else if IsEqualGuid(subtype, MFAudioFormat_MPEG) then // (D1: WAVE_FORMAT_MPEG;
            begin
              sDescr := 'MPEG-1 audio payload.';
              FormatTag := WAVE_FORMAT_MPEG;
            end
          else if IsEqualGuid(subtype, MFAudioFormat_AAC) then  // (D1: WAVE_FORMAT_MPEG_HEAAC;
            begin
              sDescr := 'Raw AAC or ADTS AAC.';
              FormatTag := WAVE_FORMAT_MPEG_HEAAC;
            end
          else if IsEqualGuid(subtype, MEDIASUBTYPE_RAW_AAC1) then  // (D1: WAVE_FORMAT_RAW_AAC1;
            begin
              sDescr := 'Raw AAC.';
              FormatTag := WAVE_FORMAT_MPEG_HEAAC;
            end
          else if IsEqualGuid(subtype, MFAudioFormat_ADTS) then  // (D1: WAVE_FORMAT_MPEG_ADTS_AAC;
            begin
              sDescr := 'Mpeg ADTS (AAC).';
              FormatTag := WAVE_FORMAT_MPEG_ADTS_AAC;
            end
          else if IsEqualGuid(subtype, MFAudioFormat_AMR_NB) then  // (D1: WAVE_FORMAT_AMR_NB;
            begin
              sDescr := 'Adaptative Multi-Rate Wideband audio. (NB)';
              FormatTag := WAVE_FORMAT_AMR_NB;
            end
          else if IsEqualGuid(subtype, MFAudioFormat_AMR_WB) then  // (D1: WAVE_FORMAT_AMR_WB;
            begin
              sDescr := 'Adaptative Multi-Rate Wideband audio. (WB)';
              FormatTag := WAVE_FORMAT_AMR_WB;
            end
          else if IsEqualGuid(subtype, MFAudioFormat_AMR_WP) then  // (D1: WAVE_FORMAT_AMR_WP;
            begin
              sDescr := 'Adaptative Multi-Rate Wideband audio. (WP)';
              FormatTag := WAVE_FORMAT_AMR_WP;
            end
          else if IsEqualGuid(subtype, MFAudioFormat_FLAC) then  // (D1: WAVE_FORMAT_FLAC;
            begin
              sDescr := 'Free Lossless Audio Codec.';
              FormatTag := WAVE_FORMAT_FLAC;
            end
          else if IsEqualGuid(subtype, MFAudioFormat_ALAC) then  // (D1: WAVE_FORMAT_ALAC;
            begin
              sDescr := 'Apple Lossless Audio Codec.';
              FormatTag := WAVE_FORMAT_ALAC;
            end
          else if IsEqualGuid(subtype, MFAudioFormat_Opus) then  // (D1: WAVE_FORMAT_OPUS;
            begin
              sDescr := 'Opus audio codec.';
              FormatTag := WAVE_FORMAT_OPUS;
            end
          else if IsEqualGuid(subtype, MFAudioFormat_Vorbis) then
            begin
              sDescr := 'Vorbis audio codec.';
              FormatTag := $00006700; // See vorbis formats and FormatTag's in MMreg.pas
            end
          else if IsEqualGuid(subtype, MFAudioFormat_Float_SpatialObjects) then
            begin
              sDescr := 'Uncompressed IEEE floating-point audio.';
              FormatTag := WAVE_FORMAT_UNKNOWN;
            end
          else if IsEqualGuid(subtype, MFAudioFormat_LPCM) then
            begin
              sDescr := 'LPCM audio with headers for encapsulation in an MPEG2 bitstream.';
              FormatTag := WAVE_FORMAT_UNKNOWN;
            end
          else if IsEqualGuid(subtype, MFAudioFormat_PCM_HDCP) then
            begin
              sDescr := 'Uncompressed PCM audio. (HDCP)';
              FormatTag := WAVE_FORMAT_PCM;
            end
          else if IsEqualGuid(subtype, MFAudioFormat_AAC_HDCP) then
            begin
              sDescr := 'Raw AAC, HDCP AAC.';
              FormatTag := WAVE_FORMAT_UNKNOWN;
            end
          else if IsEqualGuid(subtype, MFAudioFormat_ADTS_HDCP) then
            begin
              sDescr := 'Advanced Audio Coding (AAC) in Audio Data Transport Stream (ADTS) format.';
              FormatTag := WAVE_FORMAT_UNKNOWN;
            end
          else if IsEqualGuid(subtype, MFAudioFormat_Base_HDCP) then
            begin
              sDescr := 'Base HDCP (High-bandwidth Digital Content Protection).';
              FormatTag := WAVE_FORMAT_UNKNOWN;
            end
          else
            begin
              sDescr := 'Unknown audio type';
              FormatTag := WAVE_FORMAT_UNKNOWN;
            end;

          gSubType := subtype;

          sGuid := ' ( GUID: ' + sGuid + ' )';
          wsDescr := sDescr + sGuid;


          // Get the sample rate and other information from the audio format.

          cChannels := MFGetAttributeUINT32(pType,
                                            MF_MT_AUDIO_NUM_CHANNELS,
                                            0);

          samplesPerSec := MFGetAttributeUINT32(pType,
                                                MF_MT_AUDIO_SAMPLES_PER_SECOND,
                                                0);

          bitsPerSample := MFGetAttributeUINT32(pType,
                                                MF_MT_AUDIO_BITS_PER_SAMPLE,
                                                16);

          // Note: Some encoded audio formats do not contain a value for bits/sample.
          // In that case, use a default value of 16. Most codecs will accept this value.

          if (cChannels = 0) or (samplesPerSec = 0) then
            begin
              hr := MF_E_INVALIDTYPE;
              goto done;
            end;

        end; //if mediatype = audio
     end; //end if

      pPD := Nil;
      pSD := Nil;
      pHandler := Nil;
      pType := Nil;
      inc(i);

   until (i > cTypes);  // end repeat

done:
  Result := hr;
end;


// Ducking
//========
function SetDuckingForSystemSounds(): HResult;
var
  hr: HResult;
  bCoUnInit: Boolean;
  pDevice: IMMDevice;
  pEnumerator: IMMDeviceEnumerator;
  pSessionControl: IAudioSessionControl;
  pSessionControl2: IAudioSessionControl2;
  pSessionManager: IAudioSessionManager;

label
  done;

begin

  bCoUnInit := False;

  // Do a CoInitialize check
  // This is just a check, because normally CoInitialize should be initialized
  // before starting MF.

  hr := CoInitialize(Nil);
  if (hr and $80000000 = 0) then
    CoUnInitialize()
  else
    bCoUnInit := True;


  // Create the device enumerator.
  hr := CoCreateInstance(CLSID_MMDeviceEnumerator,
                         Nil,
                         CLSCTX_ALL,
                         IID_IMMDeviceEnumerator,
                         IUnknown(pEnumerator));

  // Get the default audio device.
  if Succeeded(hr) then
    hr := pEnumerator.GetDefaultAudioEndpoint(eRender,
                                              eConsole,
                                              pDevice);

  // Get the audio client.
  if Succeeded(hr) then
    hr := pDevice.Activate(IID_IAudioSessionManager,
                           CLSCTX_ALL,
                           Nil,
                           Pointer(pSessionManager));

  // Get a reference to the session manager.
  if Succeeded(hr) then
    hr := pSessionManager.GetAudioSessionControl(Nil,
                                                 0,
                                                 pSessionControl);

  // Get the extended session control interface pointer.
  if Succeeded(hr) then
    hr := pSessionControl.QueryInterface(IAudioSessionControl2,
                                         pSessionControl2);

  // Check whether this is a system sound.
  if Succeeded(hr) then
    hr := pSessionControl2.IsSystemSoundsSession();

  // If it is a system sound, opt out of the default
  // stream attenuation experience.
  if Succeeded(hr) then
    hr := pSessionControl2.SetDuckingPreference(True);

done:
  // Clean up. In Delphi all interfaces are referenced counted, so
  // Delphi compiler will take care of releasing the interfaces.
  // pSessionControl2 := Nil;
  // pSessionControl := Nil;
  // pEnumerator := Nil;
  // pDevice := Nil;

  if bCoUnInit then
    CoUnInitialize();

  Result := hr;
end;


//  Sami/smi
//=================

// This function Sets the current SAMI style, specified by index and
// calls the following methods on the SAMI media source:
//   IMFSAMIStyle.GetStyleCount gets the number of styles.
//   IMFSAMIStyle.GetStyles gets a list of the style names, stored in a PROPVARIANT (PROPVARIANT).
//   IMFSAMIStyle.SetSelectedStyle sets a style by name.
// The list of style names is also stored on the presentation descriptor, in the MF_PD_SAMI_STYLELIST attribute.
//
function SetSAMIStyleByIndex(pSource: IMFMediaSource;
                             index: DWORD): HRESULT;
var
  hr: HRESULT;
  pSami: IMFSAMIStyle;
  cStyles: DWORD;
  varStyles: PROPVARIANTarray;
  i: Integer;

label
  Done;

begin
  // Get the SAMIstyle interface
  hr := MFGetService(pSource,
                     MF_SAMI_SERVICE,
                     IID_IMFSAMIStyle,
                     pSami);

  if FAILED(hr) then
    goto done;

  hr := pSami.GetStyleCount(cStyles);

  // When using a dynamic array
  //SetLength(varStyles, cStyles);

  for i := 0 to cStyles-1 do
    PropVariantInit(varStyles[i]);

  if FAILED(hr) then
    goto done;

  if (index >= cStyles) then
    begin
      hr := E_INVALIDARG;
      goto done;
    end;

  hr := pSami.GetStyles(varStyles);

  if FAILED(hr) then
    goto done;

  hr := pSami.SetSelectedStyle(varStyles[index].calpwstr.pElems);

done:
  for i := 0 to cStyles-1 do
    PropVariantClear(varStyles[i]);

  Result := hr;
end;


// Getting the File Duration
// To get the duration of a media file, call the IMFSourceReader.GetPresentationAttribute method and
// request the MF_PD_DURATION attribute, as shown in the following code.
function GetFileDuration(pReader: IMFSourceReader;
                         out phnsDuration: LONGLONG): HRESULT;
var
  hr: HRESULT;
  pvVar: PROPVARIANT;

begin
  PropVariantInit(pvVar);

  // Get file duration
  // Gets the duration in 100-nanosecond units.
  // Divide by 10,000,000 to get the duration in seconds.
  hr := pReader.GetPresentationAttribute(MF_SOURCE_READER_MEDIASOURCE,
                                         MF_PD_DURATION,
                                         pvVar);
  if (SUCCEEDED(hr)) then
    begin
      hr := PropVariantToInt64(pvVar,
                               phnsDuration);
      PropVariantClear(pvVar);
    end;

  Result := hr;
end;


// Gets de file size
function GetFileSize(pReader: IMFSourceReader;
                     out phnsFileSize: LONGLONG): HRESULT;
var
  hr: HRESULT;
  pvVar: PROPVARIANT;

begin
  PropVariantInit(pvVar);
  hr := pReader.GetPresentationAttribute(MF_SOURCE_READER_MEDIASOURCE,
                                         MF_PD_TOTAL_FILE_SIZE,
                                         pvVar);
  if (SUCCEEDED(hr)) then
    begin
      hr := PropVariantToInt64(pvVar,
                               phnsFileSize);
      PropVariantClear(pvVar);
    end;

  Result := hr;
end;


// System
function MfpBoolToStr(const B: Boolean): string; inline;
begin
  case Integer(Bool(B)) of
    -1 : Result := 'True';
     0 : Result := 'False';
  end;
end;


// TStreamContents record
procedure TStreamContents.Init();
begin

  dwStreamIndex := 0;
  dwStreamID := 0;
  idStreamMediaType := mtUnknown;
  idStreamMajorTypeGuid := Guid_Null;
  idStreamSubTypeGuid := Guid_Null;
  bSelected := False;
  bCompressed := False;

  video_FrameRateNumerator := 0;
  video_FrameRateDenominator := 0;
  video_PixelAspectRatioNumerator := 0;
  video_PixelAspectRatioDenominator := 0;
  video_FrameSizeHeigth := 0;
  video_FrameSizeWidth := 0;

  audio_lpLangShortName := Nil;
  audio_lpLangFullName := Nil;
  audio_wsAudioDescr := '';
  audio_iAudioChannels := 0;
  audio_iSamplesPerSec := 0;
  audio_iBitsPerSample := 0;
  audio_dwFormatTag := 0;
end;



// External methods
//=================
{$WARN SYMBOL_PLATFORM OFF}
  function SetForegroundWindow; external User32Lib name 'SetForegroundWindow' delayed;
  // If the window was brought to the foreground, the return value is nonzero.
  // If the window was not brought to the foreground, the return value is zero.

  function LockSetForegroundWindow; external User32Lib name 'LockSetForegroundWindow' delayed;
{$WARN SYMBOL_PLATFORM ON}

end.
